data(Titanic)
library(rpart)
ti.rp<-rpart(Survived~.,data = Titanic)
ti.rp

library(zoo)
library(party)
ctree.model <- ctree(Survived~.,data = Titanic)
ctree.model

hc<-hclust(dist(Titanic,method = "euclidean"),method = "ward.D2")
hc


library(randomForest)
ntree_fit<-randomForest(Survived~.,data = Titanic,mtry=2,ntree=1000)
plot(ntree_fit)