dataset_MODEL2 <- read.csv("~/LA_analysis/dataset_MODEL2.csv")

###  A) Time Series: look at ARIMA model for each of the variables, we will combine into one model later

library(tidyverse) 
library(forecast) 
library(fpp2)

new.deaths.ts <- ts(dataset_MODEL2$new_deaths)
new.deaths.FIT <- auto.arima(new.deaths.ts)
summary(new.deaths.FIT)
# ARIMA(2,1,2) 

retail.ts <- ts(dataset_MODEL2$retail_and_recreation_percent_change_from_baseline)
retail.FIT <- auto.arima(retail.ts)
summary(retail.FIT)
# ARIMA(0,2,1) 

grocery.ts <- ts(dataset_MODEL2$grocery_and_pharmacy_percent_change_from_baseline)
grocery.FIT <- auto.arima(grocery.ts)
summary(grocery.FIT)

parks.ts <- ts(dataset_MODEL2$parks_percent_change_from_baseline)
parks.FIT <- auto.arima(parks.ts)
summary(parks.FIT)
# ARIMA(0,1,1) 

transit.ts <- ts(dataset_MODEL2$transit_stations_percent_change_from_baseline)
transit.FIT <- auto.arima(transit.ts)
summary(transit.FIT)
# ARIMA(2,2,1) 

workplace.ts <- ts(dataset_MODEL2$workplaces_percent_change_from_baseline)
workplace.FIT <- auto.arima(workplace.ts)
summary(workplace.FIT)
# ARIMA(0,1,2) 

residential.ts <- ts(dataset_MODEL2$residential_percent_change_from_baseline)
residential.FIT <- auto.arima(residential.ts)
summary(residential.FIT)
# ARIMA(1,0,2) with non-zero mean 


###  B) Multivariate Time Series (VAR) to predict death rate based on lagged variables

colnames(dataset_MODEL2) <- c("day", "date", "new_case", "new_deaths", "new_persons_tested", "case.fatality rate", "retail_change", "grocery_change", "parks_change", "transit_change", "workplaces_change", "residential_change")

# RESPONSE # DEATHS PER DAY
dataset_MODEL2_TS_cf <-ts(dataset_MODEL2[c(4,7,8,9,11)])
dataset_MODEL2_TS_training <- dataset_MODEL2_TS_cf[-c(65:81), ]
cor.vars2 <- cor(dataset_MODEL2[c(4,7:12)])

best_allvar_TS_cf_training <- VARselect(dataset_MODEL2_TS_training, lag.max=5, type = "none")
best_allvar_TS_cf_training
acf(dataset_MODEL2_TS_training)
pacf(dataset_MODEL2_TS_training)
## Use lag=5

## Create VAR(5) model
FINAL_fit_cf <- VAR(dataset_MODEL2_TS_training, p=5, type="none")
summary(FINAL_fit_cf$varresult$new_deaths)
plot(FINAL_fit_cf)

# Determine performance of Model according to prediction and true values in the holdout set
cf.pred <- predict(FINAL_fit_cf,n.ahead=17,ci=0.95)
fanchart(cf.pred)
predicted.cf <- data.frame(cf.pred$fcst$new_deaths)
predicted.cf$true.value <- dataset_MODEL2$new_deaths[c(65:81)]
predicted.cf$within.CI <- "No"
predicted.cf$within.CI[predicted.cf$true.value>=predicted.cf$lower & predicted.cf$true.value<=predicted.cf$upper] <- "Yes"
table(predicted.cf$within.CI) ##15/17 contained within 95% CI

# Predict # deaths for each day within the next 30 days
cf.pred2 <- predict(FINAL_fit_cf,n.ahead=30,ci=0.95)
cf.pred2$fcst$workplaces_change
fanchart(cf.pred2)

##########################################


