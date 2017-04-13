
# data <- read.csv('error.csv')
# str(data)
# summary(data)
# attach(data)
# data$WorkedHourIST <- as.factor(data$WorkedHourIST)
# 
# summary(data)
# tapply(error, WorkedHourIST, sum )
# fit <- lm(error ~ WorkedHourIST, data = data)
# anova(object = fit)
# 
# fit2 <- lm(no ~ WorkedHourIST, data=data)
# summary(fit2)


inddata <- read.csv("inderror.csv")
str(inddata)
summary(inddata)
attach(inddata)
inddata$WorkedHourIST <- as.factor(inddata$WorkedHourIST)
tapply(error, WorkedHourIST, sum )
table(inddata$WorkedHourIST)
fit <- lm(Error ~ WorkedHourIST, data = inddata)
summary(fit)
anova(object = fit)
plot(inddata$Error ~ inddata$WorkedHourIST)

library(car)
leveneTest (inddata$Error, inddata$WorkedHourIST)
bartlett.test(inddata$Error, inddata$WorkedHourIST)
fligner.test(inddata$Error, inddata$WorkedHourIST)
qchisq(0.95, 13)


##### Assumptions and what to do when violatied
# Independent Observations > blocked one way anova
# Normality  > non parametric test and transformations
# Unequal population variance (especially if the sample sizes are not approximately equal (unbalanced).)
# > 
# inddata$logerror <- log(inddata$Error)
# plot(inddata$logerror ~ inddata$WorkedHourIST)
inddata$stderror <- (inddata$Error)^0.5
plot(inddata$stderror ~ inddata$WorkedHourIST)
# inddata$experror <-  exp(inddata$Error)
# plot(inddata$experror ~ inddata$WorkedHourIST)
# inddata$log10error <- log10(inddata$Error)
# plot(inddata$log10error ~ inddata$WorkedHourIST)
# inddata$sterror <- (inddata$Error)^0.2
# plot(inddata$sterror ~ inddata$WorkedHourIST)


fit2 <- lm(stderror ~ WorkedHourIST, data = inddata)
summary(fit2)
anova(fit2)






hist(inddata2$Error)
hist(inddata2$stderror)
densityPlot(inddata2$stderror)
par(mfrow = c(3,5))

for ( i in levels(WorkedHourIST)) {
  hist(subset(inddata, WorkedHourIST == i)$Error, main=i, xlab = "Error" )
}
par(mfrow = c(1,1))


par(mfrow = c(3,5))
for ( i in levels(WorkedHourIST)) {
  densityPlot(subset(inddata, WorkedHourIST == i)$Error, main=i, xlab = "Error" )
}
par(mfrow = c(1,1))
par(mfrow = c(3,5))
   for ( i in levels(inddata2$WorkedHourIST)) {
       hist(subset(inddata2, WorkedHourIST == i)$Error, main = i, xlab="Error" )
     }
par(mfrow = c(1,1))

par(mfrow = c(3,5))
for ( i in levels(inddata2$WorkedHourIST)) {
  densityPlot(subset(inddata2, WorkedHourIST == i)$Error, main = i, xlab="Error" )
}
par(mfrow = c(1,1))



par(mfrow = c(3,5))
for ( i in levels(inddata2$WorkedHourIST)) {
  qqnorm(subset(inddata2, WorkedHourIST == i)$Error, main = i, xlab="Error" )
  qqline(subset(inddata2, WorkedHourIST == i)$Error)
  }
par(mfrow = c(1,1))



par(mfrow = c(3,5))
for ( i in levels(inddata2$WorkedHourIST)) {
  qqnorm(subset(inddata2, WorkedHourIST == i)$stderror, main = i, xlab="Error" )
  qqline(subset(inddata2, WorkedHourIST == i)$stderror)
}
par(mfrow = c(1,1))

leveneTest(inddata2$stderror, inddata2$WorkedHourIST)
leveneTest(inddata$Error, inddata$WorkedHourIST)
qchisq(0.95, 12)
fit3 <- lm(stderror ~ WorkedHourIST, data = inddata2)
summary(fit3)
anova(fit3)



