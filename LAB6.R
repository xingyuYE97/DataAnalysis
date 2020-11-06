# ISLR: Introduction to Statistical Learning with R (textbook that we use in this class)
# Validation set example with Auto dataset.
library(ISLR)
library(MASS)
library(boot)
set.seed(1)

# read the documentation for sample() function
help("sample")
train = sample(392,196)
# We use the subset option in the lm() function to fit a linear regression using,
# only the observations corresponding to the training set.
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
# Now we use predict() function to estimate the response for all 392 observations,
# and we use the mean() function to calculate the MSE of the 196 observations in the 
# validation set. Note that the -train selects only the observations that are not in,
# the training set.
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# Therefore the estimated test MSE for the linear regression fit is 26.14

# We can use the poly() function to estimate test error for the quadratic and cubic regression.
# Quadratic regression line
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
# Cubic regression line
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# The error rates are: 19.82 for quadratics and 19.78 for cubic
# If we choose different training set instead, then we will obtain somewhat different errors,
# on the validation set.
set.seed(2)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# the error rate is 23.29
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
# the error rate is 18.90
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# the error rate is 19.25
# Using this split of the observations into a training set and validation set, 
# we find that the validation set error rates for the models with linear, quadratic,
# and cubic terms are 23.29, 18.90 and 19.25 respectively.
# The model that predict mpg using a quadratic function of horsepower performs better,
# than a models that only involves only a linear function of horsepower, and there is a,
# little evidence in favor of a model that uses a cubic function of horsepower.

library(randomForest)
data1<-read.csv(file.choose(),header=TRUE)
head(data1)
colnames(data1)<-c("BuyingPrice","Maintenance","NumDoors","NumPerson","BootSpace","Safety","Condition")
head(data1)
sta(data1)
levels(data1$Condition)
summary(data1)

set.seed(100)
train<-sample(nrow(data1),0.7*nrow(data1),replace=FALSE)
TrainSet<-data1[train,]
ValidSet<-data1[-train,]
summary(TrainSet)
summary(ValidSet)

help("randomForest")
model1<-randomForest(Condition~., data= TrainSet,importance= TRUE)
model1

model2<-randomForest(Condition~., data= TrainSet,ntree=500,mtry=6,importance= TRUE)
model2

predTrain<-predict(model2,TrainSet,type="class")
table(predTrain,TrainSet$Condition)
predValid<-predict(model2,ValidSet,type="class")
table(predValid,ValidSet$Condition)

importance(model2)
varImpPlot(model2)
a=c()
i=5
for(i in 3:8){
  model3<-randomForest(Condition~., data= TrainSet,ntree=500,mtry=i,importance= TRUE)
  predValid<-predict(model3,ValidSet,type="class")
  a[i-2]<-mean(predValid==ValidSet$Condition)
}
a
plot(3:8,a)

library(rpart)
library(caret)
library(e1071)
model_dt<-train(Condition~.,data= TrainSet,method="rpart")
model_dt_1<-predict(model_dt,data=TrainSet)
table(model_dt_1,TrainSet$Condition)
mean(model_dt_1==TrainSet$Condition)

model_dt_vs<-predict(model_dt,data=ValidSet)
table(model_dt_vs,ValidSet$Condition)
mean(model_dt_vs==ValidSet$Condition)