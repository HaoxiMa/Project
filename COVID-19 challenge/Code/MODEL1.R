##=========================Final Project======================##
##
##
##Required Packages
library(dplyr)     ##to do some data manipulation
library(data.table)##to read/write data in a fast way
library(tidyverse) ##to do data processing
library(caret)     #for easy machine learning workflow
library(glmnet)    #for computing penalized regression
library(maptools)  ##Set of tools for manipulating geographic data.
library(ggmap)     ##visualize spatial data and models on top of static maps from various online sources
library(plotly)    ##Visualize plot interactively
library(neuralnet) ##For Netual Network
###Data Processing
#set working path
setwd("/Users/mahaoxi/Desktop/competition/Useful dataset")
#Import dataset 
final<-fread('dataset_MODEL1.csv')
##remain variables which will use in our model
# Set Seed so that same sample can be reproduced in future also
set.seed(1234) 
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(final), size = floor(.75*nrow(final)), replace = F)
train <- final[sample, ]
test  <- final[-sample, ]
#delete useless variable 
train<-train%>%
  select(-c(V1,Neighborhood,FIPS,Longitude,Latitude))
test<-test%>%
  select(-c(V1,Neighborhood,FIPS,Longitude,Latitude))


########====Modeling Process===##
set.seed(1234)

# Predictor variables
x <- model.matrix(Adjusted.death_rate~., train)[,-1]

# Outcome variable
y <- train$Adjusted.death_rate

############Multiple Linear Regression
Deathlm <- lm(Adjusted.death_rate~., data = train)

summary(Deathlm)

# Make predictions
predictions <- Deathlm %>% 
  predict(test)

# Model performance
# (a) Compute the prediction error, RMSE
Linear<-data.frame(
  RMSE = RMSE(predictions, test$Adjusted.death_rate),
  Rsquare = R2(predictions, test$Adjusted.death_rate)
)
rownames(Linear) <- 'Multiple Linear Regression'
Linear
############Ridge Regression
set.seed(1234)
cv <- cv.glmnet(x, y, alpha = 0)
# Display the best lambda value
#cv$lambda.min
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(Adjusted.death_rate~., test)[,-1]
predictions <- model %>% 
  predict(x.test) %>% 
  as.vector()
# Model performance metrics
Ridge<-data.frame(
  RMSE = RMSE(predictions, test$Adjusted.death_rate),
  Rsquare = R2(predictions, test$Adjusted.death_rate)
)
rownames(Ridge) <- 'Ridge Regression'
Ridge
###########Lasso
# Find the best lambda using cross-validation
set.seed(1234) 
cv <- cv.glmnet(x, y, alpha = 1)
# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
coef(model)
summary(model)
#model
# Make predictions on the test data
x.test <- model.matrix(Adjusted.death_rate~., test)[,-1]
predictions <- model %>% 
  predict(x.test) %>% 
  as.vector()
# Model performance metrics
Lasso<-data.frame(
  RMSE = RMSE(predictions, test$Adjusted.death_rate),
  Rsquare = R2(predictions, test$Adjusted.death_rate)
)

rownames(Lasso) <- 'Lasso'
Lasso
###Knn 
set.seed(1234)
model <- train(
  Adjusted.death_rate~., 
  data = train, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)
# Plot model error RMSE vs different values of k
plot(model)
# Best tuning parameter k that minimize the RMSE
model$bestTune
# Make predictions on the test data
predictions <- model %>% predict(test)
head(predictions)
# Model performance metrics
Knn<-data.frame(
  RMSE = RMSE(predictions, test$Adjusted.death_rate),
  Rsquare = R2(predictions, test$Adjusted.death_rate)
)

rownames(Knn)<-'Knn'
Knn

##########Neural Networks
#select useful variables
py.train<-train[,c(2,1,3:16)]
py.test<-test[,c(2,1,3:16)]
maxs <- apply(py.train[,c(2:16)], 2, max)
mins <- apply(py.train[,c(2:16)], 2, min)
data_scaled <- as.data.frame(scale(py.train[,c(2:16)], center = mins, scale = maxs - mins)) 
ner_scaled<-cbind(py.train$Adjusted.death_rate,data_scaled)
names(ner_scaled)[1]<-"Adjusted.death_rate"
data_scaled_test<- as.data.frame(scale(py.test[,c(2:16)], center = mins, scale = maxs - mins)) 
ner_scaled_test<-cbind(py.test$Adjusted.death_rate,data_scaled_test)

#get the formula for nn
names(ner_scaled)[c(13,15)]<-c("Native.American","Other.population")
names(ner_scaled_test)[c(13,15)]<-c("Native.American","Other.population")
n<-names(ner_scaled)
f<-as.formula(paste("Adjusted.death_rate~",paste(n[!n %in% c("Adjusted.death_rate")],collapse="+")))

#choose different stopping criteria for nn
threshold<-seq(10,250,10)
R_square_train<-rep(0,length(threshold))
R_square_test<-rep(0,length(threshold))
MSE_test<-rep(0,length(threshold))
MSE_train<-rep(0,length(threshold))
index<-1
for (i in threshold){
  set.seed(1234)
  nn<-neuralnet(f,data=ner_scaled,hidden=15,linear.output=T,threshold = i)
  #calculate R square
  resp.train<-py.train$Adjusted.death_rate;pred.train<-compute(nn,ner_scaled)
  SStotal<-var(resp.train)*(nrow(ner_scaled)-1)
  SSE<-sum((resp.train-pred.train$net.result)^2)
  SSR<-SStotal-SSE
  R2<-SSR/SStotal
  R_square_train[index]<-R2
  MSE<-SSE/nrow(ner_scaled)
  MSE_train[index]<-MSE
  #test set
  resp.test<-ner_scaled_test$`py.test$Adjusted.death_rate`;pred.test<-compute(nn,ner_scaled_test)
  SStotal<-var(resp.test)*(nrow(ner_scaled_test)-1)
  SSE<-sum((resp.test-pred.test$net.result)^2)
  MSE<-SSE/nrow(ner_scaled_test)
  MSE_test[index]<-MSE
  SSR<-SStotal-SSE
  R2_test<-SSR/SStotal
  R_square_test[index]<-R2_test
  index<-index+1
}

#show result
plot(R_square_train,sqrt(MSE_test),main ="R square in Training set vs standard variance in Test set",
     xlab="R square",ylab = "Standard variance")
#choose R square==0.515 and standard variance==12.858 where threshold=110
nn<-neuralnet(f,data=ner_scaled,hidden=15,linear.output=T,threshold = 110)
#plot(nn)
########=====Visulaization Result===##
##LA Neighborhoods map
la<-qmap("los angeles", zoom = 10)

shpfile <- "geo_export_b2513739-0d15-4c3e-8bba-73c5f5eb4a7a.shp"
##Source: https://usc.data.socrata.com/dataset/Los-Angeles-Neighborhood-Map/r8qd-yxsr
sh <- readShapePoly(shpfile)
points <- fortify(sh)

la+geom_polygon(aes(x=long,
                    y=lat, 
                    group=group, 
                    alpha=0.25), 
                data=points, fill='white') +
  geom_polygon(aes(x=long,y=lat, group=group), 
               data=points, color='black', fill=NA)

##Define our risk score through using Boxplot
##Draw box-plot to find to find 5 summary numbers, such as Q1, Q2,etc
fig <- plot_ly( x= final$Adjusted.death_rate, 
               type = "box", 
               quartilemethod="inclusive", 
               name='Adjusted Death Rate') %>% 
  layout(title = "Adjusted Death Rate Quartiles")


