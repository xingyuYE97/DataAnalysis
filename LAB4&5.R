# creating a matrix data with random numbers
# and plotting the matrix using the image() function
# you will see there, it does not have a real pattern in the plot.
set.seed(12345)
help(par)
# par can be used to set or query graphical parameters.
# Parameters can be set by specifying them as arguments 
# to par in tag = value form, or by passing them as a list of tagged values.
par(mar = rep(0.2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

# now we can run a hierarchical cluster analysis on the dataset 
# we will use the heatmap() function that is available in R
help("heatmap") # read the documentation for # the heatmap() function that is available in #RStudio 
#Read the documentation for rep()
help(rep)

par(mar=rep(0.2,4))
heatmap(data_Matrix)
# When we run the heatmap() here, we get the dendrograms printed on the both columns and the rows and still there is no real immerging pattern that is interesting to us, 
#it is because there is no real interesting pattern underlying in the data we generated.

# Now we will add a pattern to the data by doing a random coin flip.
# we will use the rbinom() function along with a for-loop.
help("rbinom") # read the documentation for the rbinom() function that 
# is available in RStudio

set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(coin_Flip){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}

# Now we will plot the data
# Now we can see that the right hand five columns have more yellow in them,
# which means they have a higher value and the left hand five columns that are little bit more in red color which means they have a lower value.
# it is because some of the rows have a mean of three in the right hand side, and
# some of the rows have mean of zero. Now we have introduced some pattern to it.

par(mar= rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

# now we will run the heatmap() function on the data, we can see that, two #sets of columns are easily separated. 
par(mar=rep(0.2, 4))
heatmap(data_Matrix)

hh<-hclust(dist(data_Matrix))
data_Matrix_Ordered<-data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1,xlab='The Row Mean',ylab="Row",pch=19)
plot(rowMeans(data_Matrix_Ordered),xlab='Column',ylab="Column Mean",pch=19)

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
