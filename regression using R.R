dataset=read.csv("Real estate.csv")
View(dataset)
library(tidyr)
dataset=drop_na(dataset)
str(dataset)# all the datset is of numerics type except number of convenience store
summary(dataset)# Use to find min 1st quartile 3rd quartile mean median max
cor(dataset)# since none of the variables have correlation greater than 8 multicillinearity is absent
#fitting the model
#fitting the model using variables having both  positive and negative correlation
model1=lm(Y.house.price.of.unit.area ~ X1.transaction.date+X2.house.age+X3.distance.to.the.nearest.MRT.station+X4.number.of.convenience.stores+X5.latitude+X6.longitude,data=dataset)
summary(model1)
#here the adjusted r square is57%
#fitting the model having only positive correlation
model2=lm(Y.house.price.of.unit.area ~ X1.transaction.date+X4.number.of.convenience.stores+X5.latitude+X6.longitude,data=dataset)
summary(model2)
#here the adjusted R2 is 48%
#since the model1 has greater adjusted R2 we will consider model 1 for further analysis
plot(model1)
# test for autocorrelation
library(car)
durbinWatsonTest(model1)
#thus autocorrelation is present
#testing for normality
hist(dataset$Y.house.price.of.unit.area,main="Histogram")
library(lmtest)
bptest(model1)
shapiro.test(dataset$Y.house.price.of.unit.area)
# Plot the chart.
boxplot(Y.house.price.of.unit.area ~ X1.transaction.date+X2.house.age+X3.distance.to.the.nearest.MRT.station+X4.number.of.convenience.stores+X5.latitude+X6.longitude,data=dataset)
boxplot(dataset)
plot(model1)

