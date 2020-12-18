#=============Hierarchical time series forecasting===========
setwd("/Users/mahaoxi/Desktop/project/Hierarchical Time Series Forecasting")

#import useful packages
library(tidyr)
library(mice)
library(hts)
library(data.table)
library(fable)
library(dplyr)
library(ggplot2)

#import the dataset(get the lastest data until 2020.12.16)
#case.data <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
#raw_case <- read.csv(case.data,header=T)
#write.csv(raw_case,file="Data.csv")

raw_case<-read.csv("Data.csv")
raw_case<-raw_case[,-1]

#===data manipulation===
#check and delete the missing value and some useless column
md.pattern(raw_case)
j<-raw_case[rowSums(is.na(raw_case))>0,]
#all missing vlaue shows in FIPS
Covid_case<-raw_case %>% 
  dplyr::select(-UID,-iso2,-iso3,-code3,-FIPS,-Combined_Key,-Country_Region,-Lat,-Long_) %>%
  dplyr::rename(state="Province_State",County="Admin2")

#delete the state not having county
str(Covid_case$state)
str(Covid_case$County)
Covid_case<-Covid_case %>% filter(County!="")
Covid_case$County<-as.character(factor(Covid_case$County))
Covid_case$state<-factor(Covid_case$state)

#later, we will find some county named "unassigned" is constant, we need to add noise to it. 
#here, we change their names
index_unass<-Covid_case$County=="Unassigned"
Covid_case$County[index_unass]<-
  paste(Covid_case$County[index_unass],Covid_case$state[index_unass],sep = " of ")

#calculate the daily new case from 2020.1.23-2020.12.16
new_case<-Covid_case[,4:332]-Covid_case[,3:331]
Covid_case<-cbind(Covid_case[,1:2],new_case)
state_names<-as.character(levels(Covid_case$state))

#Get the number of nodes in different levels
level_1<-nlevels(Covid_case$state)
level_2<-Covid_case %>% group_by(state) %>% summarise(C=n()) %>% pull(C)

#transpose the tibble
County_name<-Covid_case$County
hts_case<-Covid_case %>% dplyr::select(-County,-state) %>% as.matrix() %>% t() %>% as.data.frame()
names(hts_case)<-County_name

#Because we need to preform MinT reconilition, when there is a constant column in the dataset,it is
#impossible to estimate covariances when one of the series is constant. Add some samll random noise 
#to the constant column is good!
set.seed(123456)
hts_case[, sapply(hts_case, function(v) var(v)==0)]<-
  hts_case[, sapply(hts_case, function(v) var(v)==0)]+rnorm(nrow(hts_case),mean=0,sd=0.01)

#generate the hts
HTS_case <- hts::hts(
  ts(hts_case, start=1, frequency=1),
  list(level_1,level_2)
  ) 

#covert into tibble for plot
HTS_case_t<-HTS_case %>% as_tsibble() %>%
  rename(
    state = "Level 1",
    County = "Level 2",
    date = index,
    new_cases = value
  ) %>%
  mutate(
    state = rep(state_names,level_2*329)
    )

#====plots====
p1 <- HTS_case_t %>%
  summarise(total_new_cases = sum(new_cases)) %>%
  autoplot(total_new_cases)+
  ylab("Daily new cases") + xlab("Date(2020.1.23-2020.12.16)") +
  ggtitle("Total daily new cases in America")

p2 <- HTS_case_t %>%
  group_by(state) %>%
  summarise(total_new_cases = sum(new_cases)) %>%
  autoplot(total_new_cases)+
  ylab("Daily new cases") + xlab("Date(2020.1.23-2020.12.16)") +
  ggtitle("Total daily new cases in America: by state")

p3 <- HTS_case_t %>%
  filter(state=="Connecticut") %>%
  group_by(County) %>%
  autoplot(new_cases)+
  ylab("Daily new cases") + xlab("Date(2020.1.23-2020.12.16)") +
  ggtitle("Total daily new cases in America: Connecticut by county")

p4 <- HTS_case_t %>%
  filter(County=="Hartford") %>%
  autoplot(new_cases)+
  ylab("Daily new cases") + xlab("Date(2020.1.23-2020.12.16)") +
  ggtitle("Total daily new cases in America: Hartford")

#===Model comparesion===

#Fit the seperate arima model to total new cases
Ts_data<-HTS_case_t %>%
  summarise(total_new_cases = sum(new_cases)) %>% pull(total_new_cases)

ts_train<-Ts_data[1:300]
ts_test<-Ts_data[301:length(Ts_data)]

auto_1=auto.arima(ts_train,ic="bic",trace = T)
pred_value<-forecast(auto_1,h=29)

#MAPE
error<-ts_test-pred_value$mean
MAPE<-100*(mean(abs((error)/ts_test)))

#Hierarchical forecasting
#split into train and test dataset
train<-window(HTS_case,start=1,end=300)
test<-window(HTS_case,start=301)

###Bottom-up
#arima
forecasting_bottomup_arima<-forecast.gts(train,
                                   method = "bu",fmethod = "arima",algorithms = "cg",h=29)
accuracy.gts(forecasting_bottomup_arima,test,levels = 0)
#ETS
forecasting_bottomup_ets<-forecast.gts(train,
                                   method = "bu",fmethod = "ets",algorithms = "cg",h=29)
accuracy.gts(forecasting_bottomup_ets,test,levels = 0)

###Top-down
#arima
forecasting_topdown_arima<-forecast.gts(train,
                                         method = "tdfp",fmethod = "arima",algorithms = "cg",h=29)
accuracy.gts(forecasting_topdown_arima,test,levels = 0)
#ETS
forecasting_topdown_ets<-forecast.gts(train,
                                       method = "tdfp",fmethod = "ets",algorithms = "cg",h=29)
accuracy.gts(forecasting_topdown_ets,test,levels = 0)

###OLS
#arima
forecasting_ols_arima<-forecast.gts(train,
                                        method = "comb",weights ="ols",
                                    fmethod = "arima",algorithms = "cg",h=29)
accuracy.gts(forecasting_ols_arima,test,levels = 0)
#ETS
forecasting_ols_ets<-forecast.gts(train,
                                      method = "comb",weights = "ols",
                                  fmethod = "ets",algorithms = "cg",h=29)
accuracy.gts(forecasting_ols_ets,test,levels = 0)

###WLS
#arima
forecasting_wls_arima<-forecast.gts(train,
                                    method = "comb",weights ="wls",
                                    fmethod = "arima",algorithms = "cg",h=29)
accuracy.gts(forecasting_wls_arima,test,levels = 0)
#ETS
forecasting_wls_ets<-forecast.gts(train,
                                  method = "comb",weights = "wls",
                                  fmethod = "ets",algorithms = "cg",h=29)
accuracy.gts(forecasting_wls_ets,test,levels = 0)

###Structural scaling
#arima
forecasting_SS_arima<-forecast.gts(train,
                                    method = "comb",weights ="nseries",
                                    fmethod = "arima",algorithms = "cg",h=29)
accuracy.gts(forecasting_SS_arima,test,levels = 0)
#ETS
forecasting_SS_ets<-forecast.gts(train,
                                  method = "comb",weights = "nseries",
                                  fmethod = "ets",algorithms = "cg",h=29)
accuracy.gts(forecasting_SS_ets,test,levels = 0)

#MinT
forecasting_minT_arima<-forecast.gts(train,
                                   method = "comb",weights ="mint",covariance = "shr",
                                   fmethod = "arima",algorithms = "cg",h=29)
accuracy.gts(forecasting_minT_arima,test,levels = 0)
#ETS
forecasting_minT_ets<-forecast.gts(train,
                                 method = "comb",weights = "mint",covariance = "shr",
                                 fmethod = "ets",algorithms = "cg",h=29)
accuracy.gts(forecasting_minT_ets,test,levels = 0)
#Use MPE to judge whether the forecast is unbiased. If MAP is closed to zero, then unbiased.

plot(forecasting_minT_ets,include = 5,levels = c(0,1))












