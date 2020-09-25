EPI_data <- read.csv('C:/Users/15189/Desktop/DataAnalysis/2010EPI_data.csv',header=TRUE)


plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

plot(ecdf(EPI_data$DALY),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$DALY),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)

plot(ecdf(EPI_data$CLIMATE),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$CLIMATE),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
help("qqnorm") # read the RStudio documentation for qqnorm
help("qqplot") # read the RStudio documentation for qqplot
qqnorm(EPI_data$CLIMATE)
qqline(EPI_data$CLIMATE) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)


# Multivariate Regression 
multivariate <-read.csv('C:/Users/15189/Desktop/DataAnalysis/multivariate.csv',header=TRUE)
head(multivariate)
attach(multivariate)
help(lm)
mm <-lm(Homeowners~Immigrant)
mm # mm here is a R object. 
summary(mm)$coef # The output above shows the estimate of the regression beta coefficients (column Estimate) and 
# their significance levels (column Pr(>|t|).
# The intercept is 107494.898 and the coefficient of Immigrant variable is -6656.839.
# The estimated regression equation can be written as follow:
# Homeowners = 107494.898 + (-6656.839)*Immigrant 
# We can rewrite it as: 
# Homeowners = 107494.898 - 6656.839*Immigrant.

plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
# Using this formula, for each new value in Immigrant, you can predict the value for Homeowners.
# As an example:
# For Immigrant value = 0, we will get: Homeowners = 107494.898 - 6656.839*0 = 107494.898
# for Immigrant value = 20, we will get: Homeowners = 107494.898 - 6656.839*20 = -25641.88
# Predictions can be easily made using the R function predict().
# In the following example, we predict Homeowners for two Immigrant values: 0 and 20.
# you can pass the 0 and 20 values as a concatenated list for Immigrants as follows:
newImmigrantdata <- data.frame(Immigrant = c(0,  20))
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
mm %>% predict(newImmigrantdata)

abline(mm)
abline(mm,col=3,lwd=3) # line color = green, line width = 3
attributes(mm)
mm$coefficients

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2,col='red')
points(pressure$temperature,pressure$pressure/2,col='blue')
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom='line')
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10)
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars.aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars.aes(x=mpg))+geom_histogram(binwidth=5)

plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len~supp, data=ToothGrowth)
boxplot(len~supp+dose, data=ToothGrowth)

qplot(ToothGrowth$supp, ToothGrowth$len,geom='boxplot')
qplot(supp,len, data=ToothGrowth,geom='boxplot')

ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()

qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom='boxplot')
qplot(interaction(supp,dose),len,data=ToothGrowth,geom='boxplot')
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()


