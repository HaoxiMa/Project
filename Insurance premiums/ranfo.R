setwd("/Users/mahaoxi/Desktop/5605/project")
library(ggplot2)
library(randomForest)


data<-read.csv("insurance.csv",header=TRUE)
str(data)
data$children<-factor(data$children)


#divide data into two groups
set.seed(123456)
index<-sample(1:nrow(data),round(0.75*nrow(data))) 
train<- data[index,] 
test<- data[-index,]

#build model
r<-randomForest(charges~age+sex+bmi+children+smoker+region,
                data=train,importance=TRUE,ntree=10000)
print(r)
varImpPlot(r,main="Variable importance")

fitted_value_test<-predict(r,test)

attach(test)
test<-data.frame(age=age,sex=sex,bmi=bmi,children=children,
                 smoker=smoker,region=region,charges=charges,id=seq(1:nrow(test)))
detach(test)

test_response_predict<-ggplot(test,aes(x=id))+geom_point(aes(y=charges),col=rgb(0,0,1,1/2))+
  geom_line(aes(y=charges,col="yellow"))+
  geom_point(aes(y=fitted_value_test),col=rgb(0,1,0,1/2))+
  geom_line(aes(y=fitted_value_test,col="red"))+
  labs(title="Response vs Fitted value in Test dataset",
       subtitle="Using Randomforest")
plot(test_response_predict)
