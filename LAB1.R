EPI_data <- read.csv('C:/Users/15189/Desktop/DataAnalysis/2010EPI_data.csv',header=TRUE)
#or
#EPI_data <- read.xlsx(â???<path>/2010EPI_data.xlsx")
# Note: replace default data frame name â???" cannot start with numbers!
View(EPI_data)
#
attach(EPI_data) 	# sets the â???~defaultâ???T object
fix(EPI_data) 	# launches a simple data editor
EPI<-EPI_data$EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

#other data
GRUMP_data <- read.csv('C:/Users/15189/Desktop/DataAnalysis/GPW3_GRUMP_SummaryInformation_2010.csv',header = TRUE)

#Cumulative Density Function
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
#Quantile-Quantile?
par(pty="s") 
qqnorm(EPI); qqline(EPI)
#Simulated data from t-distribution:
x <- rt(250, df = 5)
qqnorm(x); qqline(x)
#Make a Q-Q plot against the generating distribution by: x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

help(distributions)
# try different ones.....

#Landlock
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
#
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

summary(EPI) 	# stats
fivenum(EPI,na.rm=TRUE)
help(stem)
stem(EPI)		 # stem and leaf plot
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=â???oSJâ???
help(rug)
rug(EPI) 
