setwd("/Users/mahaoxi/Desktop/project/Insurance premiums")
library(mice)
library(neuralnet)
library(ggplot2)

data<-read.csv("insurance.csv",header=TRUE)
str(data)

#check NA value
md.pattern(data)

#scaled the data
maxs <- apply(data[,c(1,3,7)], 2, max)
mins <- apply(data[,c(1,3,7)], 2, min)
data_scaled <- as.data.frame(scale(data[,c(1,3,7)], center = mins, scale = maxs - mins)) 
#scale function give a matrix, we should transform into data.frame
data_scaled<-cbind(data_scaled,data[,c(2,4,5,6)])
data_scaled$female=ifelse((data$sex=="female"),1,0)
data_scaled$childern0=ifelse((data$children==0),1,0)
data_scaled$childern1=ifelse((data$children==1),1,0)
data_scaled$childern2=ifelse((data$children==2),1,0)
data_scaled$childern3=ifelse((data$children==3),1,0)
data_scaled$childern4=ifelse((data$children==4),1,0)
data_scaled$smokeryes=ifelse((data$smoker=="yes"),1,0)
data_scaled$region_ne<-ifelse((data$region=="northeast"),1,0)
data_scaled$region_nw<-ifelse((data$region=="northwest"),1,0)
data_scaled$region_se<-ifelse((data$region=="southeast"),1,0)

#divide data into two groups
set.seed(12345)
index<-sample(1:nrow(data_scaled),round(0.75*nrow(data_scaled))) 
train<- data_scaled[index,] 
test<- data_scaled[-index,]
train<-train[,-c(4:7)]
test<-test[,-c(4:7)]

#build neural net
n<-names(train)
f<-as.formula(paste("charges~",paste(n[!n %in% c("charges")],collapse="+")))
nn<-neuralnet(f,data=train,hidden=c(8,4),linear.output=T)
#plot(nn)

#calculate R^2
resp.train<-train$charges;pred.train<-compute(nn,train)
SStotal<-var(resp.train)*(nrow(train)-1)
SSE<-sum((resp.train-pred.train$net.result)^2)
SSR<-SStotal-SSE
R2<-SSR/SStotal

#For plot
resp.test<-test$charges;pred.test<-compute(nn,test)
fitted_value_test<-pred.test$net.result

attach(test)
test<-data.frame(charges=charges,id=seq(1:nrow(test)))
detach(test)

test_response_predict<-ggplot(test,aes(x=id))+geom_point(aes(y=charges),col=rgb(0,0,1,1/2))+
  geom_line(aes(y=charges,col="yellow"))+
  geom_point(aes(y=fitted_value_test),col=rgb(0,1,0,1/2))+
  geom_line(aes(y=fitted_value_test,col="red"))+
  labs(title="Response vs Fitted value in Test dataset",
       subtitle="Using Neural net",y="Scaled charges")
plot(test_response_predict)














