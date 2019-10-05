rm(list=ls())
#load data
Auto<-read.csv("AutomobileData.csv")


#Y: Automobile normalized losses
loss<-Auto$normalized.losses
curbweight <-Auto$curb.weight
enginesize <-Auto$engine.size
horsepower <-Auto$horsepower
price <-Auto$price
rating <-Auto$symboling
height <- Auto$height

#summary statistics of dependent and explanatory variable
library(psych)
describe(Auto)

#relationship among the variables
cor(Auto)
#pairwise plots
pairs(cbind(loss, curbweight, enginesize, horsepower, price, rating, height), lower.panel = NULL , gap = 0)

#fit linear regression model
Model.0 <- lm(loss~curbweight+enginesize+horsepower+price+rating+height)
summary(Model.0)

#Variance Inflation Factor (VIF)
R.squared <- 0.4245
VIF <- 1/(1-R.squared)
VIF

#ANOVA table
anova(Model.0)

#model plot
plot(Model.0)

#leverage points
referencepoint <- (6+1)*3/163#(k+1)*3/n 
referencepoint
leverage <-which(hatvalues(Model.0)>referencepoint)
leverage

#Outliers
#residuals versus fitted values plot
residual<-Model.0$residuals
fit<-fitted.values(Model.0)
plot(residual~fit, xlab="Residuals", ylab = "Fitted Values", main = "Residuals versus Fitted Values")
#qq plot
qqplot(fit,residual,xlab="Residuals", ylab = "Fitted Values", main = "qq Plot")
#outliers
outlier<-hatvalues(Model.0)
plot(residual~outlier, main = "Residual Analysis")

