setwd("/Users/mahaoxi/Desktop/5605/project")
library(ggplot2)
library(mice)
library(carData)
library(car)
library(corrplot)
library(olsrr)
library(MASS)
source("/Users/mahaoxi/Desktop/5605/textbook/stepwise.r")

data<-read.csv("insurance.csv",header=TRUE)
str(data)
data$children<-factor(data$children)
attach(data)

#detect NA value
md.pattern(data)

#judge the distribution of charges
hist_charges<-ggplot(data,aes(x=charges))+
              geom_histogram(aes(y=..density..),binwidth=1000,fill="white",color="black")+
              geom_density(alpha=0.2,fill="#FF6666")+
              labs(title="Histogram for Charges",x="Charges",y="Density")
plot(hist_charges)
#not very normal, we should do transformation later

#plot for continuous variables vs charges
gg_age<-ggplot(data,aes(x=age,y=charges))+geom_point(color=rgb(0,0,1,1/2))+
        labs(title="Scatter plot for Age vs Charges",y="Charges",x="Age")
gg_BMI<-ggplot(data,aes(x=bmi,y=charges))+geom_point(color=rgb(0,0,1,1/2))+
  labs(title="Scatter plot for BMI vs Charges",y="Charges",x="BMI")
plot(gg_age)
plot(gg_BMI)
##indentify the intersections between age,bmi and sex,childern,smoker,region
#age vs sex
gg_age_sex<-ggplot(data,aes(x=age,y=charges))+geom_point(aes(col=sex),size=1)
gg_age_sex<-gg_age_sex+labs(title="Charges vs Age",subtitle="Classifed by sex",
                            y="Charges",x="Age")
gg_age_sex<-gg_age_sex+geom_smooth(aes(col=sex),method="lm",se=FALSE)
plot(gg_age_sex)
#age vs childern
gg_age_childern<-ggplot(data,aes(x=age,y=charges))+geom_point(aes(col=children),size=1)
gg_age_childern<-gg_age_childern+labs(title="Charges vs Age",subtitle="Classifed by children"
                                      ,y="Charges",x="Age")
gg_age_childern<-gg_age_childern+geom_smooth(aes(col=children),method="lm",se=FALSE)
plot(gg_age_childern)
#age vs smoker
gg_age_smoker<-ggplot(data,aes(x=age,y=charges))+geom_point(aes(col=smoker),size=1)
gg_age_smoker<-gg_age_smoker+labs(title="Charges vs Age",subtitle="Classifed by smoker"
                                      ,y="Charges",x="Age")
gg_age_smoker<-gg_age_smoker+geom_smooth(aes(col=smoker),method="lm",se=FALSE)
plot(gg_age_smoker)
#age vs region
gg_age_region<-ggplot(data,aes(x=age,y=charges))+geom_point(aes(col=region),size=1)
gg_age_region<-gg_age_region+labs(title="Charges vs Age",subtitle="Classifed by region"
                                  ,y="Charges",x="Age")
gg_age_region<-gg_age_region+geom_smooth(aes(col=region),method="lm",se=FALSE)
plot(gg_age_region)
#bmi vs sex
gg_bmi_sex<-ggplot(data,aes(x=bmi,y=charges))+geom_point(aes(col=sex),size=1)
gg_bmi_sex<-gg_bmi_sex+labs(title="Charges vs Bmi",subtitle="Classifed by sex"
                                  ,y="Charges",x="Bmi")
gg_bmi_sex<-gg_bmi_sex+geom_smooth(aes(col=sex),method="lm",se=FALSE)
plot(gg_bmi_sex)
#bmi vs children
gg_bmi_children<-ggplot(data,aes(x=bmi,y=charges))+geom_point(aes(col=children),size=1)
gg_bmi_children<-gg_bmi_children+labs(title="Charges vs Bmi",subtitle="Classifed by childern"
                            ,y="Charges",x="Bmi")
gg_bmi_children<-gg_bmi_children+geom_smooth(aes(col=children),method="lm",se=FALSE)
plot(gg_bmi_children)
#bmi vs smoker
gg_bmi_smoker<-ggplot(data,aes(x=bmi,y=charges))+geom_point(aes(col=smoker),size=1)
gg_bmi_smoker<-gg_bmi_smoker+labs(title="Charges vs Bmi",subtitle="Classifed by smoker"
                                      ,y="Charges",x="Bmi")
gg_bmi_smoker<-gg_bmi_smoker+geom_smooth(aes(col=smoker),method="lm",se=FALSE)
plot(gg_bmi_smoker)
#bmi vs region
gg_bmi_region<-ggplot(data,aes(x=bmi,y=charges))+geom_point(aes(col=region),size=1)
gg_bmi_region<-gg_bmi_region+labs(title="Charges vs Bmi",subtitle="Classifed by region"
                                  ,y="Charges",x="Bmi")
gg_bmi_region<-gg_bmi_region+geom_smooth(aes(col=region),method="lm",se=FALSE)
plot(gg_bmi_region)

##indentify the intersection between age and bmi
summary(age)
data$age_group<-data$age
data$age_group[which(data$age<=27)]<-"Young"
data$age_group[which(data$age>27 & data$age<=51)]<-"Medium"
data$age_group[which(data$age>51)]<-"Old"
gg_bmi_age<-ggplot(data,aes(x=bmi,y=charges))+geom_point(aes(col=age_group),size=1)
gg_bmi_age<-gg_bmi_age+labs(title="Charges vs Bmi",subtitle="Classifed by age"
                                  ,y="Charges",x="Bmi")
gg_bmi_age<-gg_bmi_age+geom_smooth(aes(col=age_group),method="lm",se=FALSE)
plot(gg_bmi_age)
detach(data)
#Then we assume the model:
#charges~age+sex+bmi+children+smoker+region+bmi*sex+age*children+bmi*children+bmi*smoker

##test main variables multicollinearity
data<-data[-8]
str(data)
#convert factor into numeric to plot correlation matrix and corplot
data1<-data
index<-sapply(data1,is.factor)
data1[index]<-sapply(data1[index],as.numeric)
correlation<-cor(data1[-7])
#correlation plot
corrplot(correlation, method = "number")
#reverse factor back
str(data)
model1<-lm(charges~age+sex+bmi+children+smoker+region,data=data)
vif(model1)>5
#none multicollinearity exists in main effect

##Select "best" model
#divide data into train and test datase
n<-round(length(data$age)*0.75)
set.seed(123)
index<-sample(c(1:nrow(data)),n,replace=FALSE)
train<-data[index,]
test<-data[-index,]
nrow(train);str(train)
#do log transformation on charges-
#because we find that the variance is close to mean^2
train$charges<-log(train$charges)
test$charges<-log(test$charges)

#stepwise to select model
attach(train)
#method1
stepwise(charges~age+sex+bmi+children+smoker+region+bmi*sex+
           age*children+bmi*children+bmi*smoker,charges~1,
         alpha.to.enter=0.05,alpha.to.leave=0.05)
#method2
full_model<-lm(charges~age+sex+bmi+children+smoker+region+bmi*sex+
                 age*children+bmi*children+bmi*smoker)
stepAIC(full_model)
#two methods come to the same "best" model then recheck it:
fit1<-lm(charges~age+sex+bmi+children+smoker+
           region+age*children+bmi*smoker,data=train)
Anova(fit1,type=3)
detach(train)
#get the model:
#charges~age+sex+bmi+children+smoker+region+age*children+bmi*smoker

##diagnostics and remedy 
qqnorm(fit1$residuals)
shapiro.test(fit1$residuals)
plot(fit1$fitted.values,fit1$residuals,main="Fitted value vs Residuals",
     xlab = "Fitted value",ylab="Residuals")
#exist non-normality and heteroscedasticity
#remedy---Box-Cox Transformations
p1<-powerTransform(fit1)
summary(p1)
train$charges<-bcPower(train$charges,p1$lambda,jacobian.adjusted=TRUE)
test$charges<-bcPower(test$charges,p1$lambda,jacobian.adjusted=TRUE)
fit2<-lm(charges~age+sex+bmi+children+smoker+
           region+age*children+bmi*smoker,data=train)
qqnorm(fit2$residuals)
plot(fit2$fitted.values,fit2$residuals)

#test constant variance
#renumber the train and test data
attach(train)
train<-data.frame(age=age,sex=sex,bmi=bmi,children=children,
                   smoker=smoker,region=region,charges=charges)
detach(train)
attach(test)
test<-data.frame(age=age,sex=sex,bmi=bmi,children=children,
                 smoker=smoker,region=region,charges=charges)
detach(test)
fit3<-lm(charges~age+sex+bmi+children+smoker+
           region+age*children+bmi*smoker,data=train)
fit3_fitted<-fit3$fitted.values
length(fit3_fitted) 
G1<-sort(fit3_fitted)[1:502];G2<-sort(fit3_fitted)[503:1004]
G1_atr<-attributes(G1);G2_atr<-attributes(G2)
G1_index<-as.numeric(G1_atr$names);G2_index<-as.numeric(G2_atr$names)
G1_res<-fit3$residuals[G1_index];G2_res<-fit3$residuals[G2_index]
d1<-abs(G1_res-median(G1_res));d2<-abs(G2_res-median(G2_res))
x<-mean(d1)-mean(d2);y<-(501*var(d1)+501*var(d2))/1002
t<-x/(sqrt(y)*sqrt(1/502+1/502))
qt(0.975,1002)

#test autocorrealtion
durbinWatsonTest(fit2,alternative="two.sided")

##check validation
model<-fit2
#reliable-shrinkage on cross-validation
Rsquare_train<-cor(model$fitted.values,train$charges)^2
fitted_value_test<-predict(model,test[-7])
Rsquare_test<-cor(fitted_value_test,test$charges)^2
shrinkage<-Rsquare_train-Rsquare_test;shrinkage<0.1
#measures of predictive ability
MSE<-0.4084^2
MSPR<-sum((test$charges-fitted_value_test)^2)/nrow(test)
MSE;MSPR

##summarize
attach(test)
test<-data.frame(age=age,sex=sex,bmi=bmi,children=children,
                  smoker=smoker,region=region,charges=charges,id=seq(1:nrow(test)))
summary(model)
test_response_predict<-ggplot(test,aes(x=id))+geom_point(aes(y=charges),col=rgb(0,0,1,1/2))+
                       geom_line(aes(y=charges,col="yellow"))+
                       geom_point(aes(y=fitted_value_test),col=rgb(0,1,0,1/2))+
                       geom_line(aes(y=fitted_value_test,col="red"))+
                       labs(title="Response vs Fitted value in Test dataset",
                            subtitle="Using MLR",y="Transformed charges")
plot(test_response_predict)
detach(test)



#tips:
#多重共线和有没有intersection不是一回事：比如对age和bmi的intersection interpretion:
#when age is large, increasing per unit bmi gives more increase in charges
#in general, people include main effects when the intersection of them is included



















