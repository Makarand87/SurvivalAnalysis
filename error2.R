setwd("/\\shares\\shares\\Business Analytics\\Internal Analytics\\Medical Coding\\CD03 - Hypothesis Testing\\Functional Document")
getwd()



inddata <- read.csv("inderror.csv")
inddata2 <- subset(inddata, Error != 0)
inddata3 <- subset(inddata2, WorkedHourIST != 7 & WorkedHourIST != 18 & WorkedHourIST != 19 & WorkedHourIST != 20)

summary(inddata3)


inddata3$WorkedHourIST <- as.factor(inddata3$WorkedHourIST)

round(tapply(inddata3$Error, inddata3$WorkedHourIST, mean ),2)
table(inddata3$WorkedHourIST)
inddata3$stderror <- (inddata3$Error)^0.5
inddata3$logerror <- log(inddata3$Error)
boxplot(inddata3$stderror ~ inddata3$WorkedHourIST)
boxplot(inddata3$Error ~ inddata3$WorkedHourIST)
boxplot(inddata3$logerror ~ inddata3$WorkedHourIST)

library(car)
leveneTest(inddata3$logerror, inddata3$WorkedHourIST)
bartlett.test(inddata3$logerror, inddata3$WorkedHourIST)
fligner.test(inddata3$logerror, inddata3$WorkedHourIST)


leveneTest(inddata3$Error, inddata3$WorkedHourIST)
bartlett.test(inddata3$Error, inddata3$WorkedHourIST)
fligner.test(inddata3$Error, inddata3$WorkedHourIST)

hist(inddata3$Error)
hist(inddata3$logerror)
densityPlot(inddata3$logerror)
par(mfrow = c(2,5))

for ( i in levels(inddata3$WorkedHourIST)) {
  hist(subset(inddata3, WorkedHourIST == i)$Error, main=i, xlab = "Error" )
}
par(mfrow = c(1,1))

par(mfrow = c(2,5))
for ( i in levels(inddata3$WorkedHourIST)) {
  hist(subset(inddata3, WorkedHourIST == i)$logerror, main=i, xlab = "Error" )
}
par(mfrow = c(1,1))


fit2 <- lm(logerror ~ WorkedHourIST, data = inddata3)
summary(fit2)
anova(fit2)




