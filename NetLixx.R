# install.packages("survival")
library("survival")

net_lixx <- read.csv("NetLixx.csv")

head(net_lixx)

str(net_lixx)
summary(net_lixx)

table(net_lixx$female, net_lixx$churned)

net_lixx$survival <- Surv(net_lixx$time, net_lixx$churned==1)

fit1 <- survfit(survival ~ 1, data=net_lixx)
fit1
# summary(fit)
plot(fit1, lty=1, mark.time = FALSE, ylim=c(.75,1), xlab = 'Days since Subscribing', ylab = 'Percent Surviving')
plot(fit1, ylim=c(.75,1), mark.time = TRUE)

fit2 <- survfit(survival ~ female, data=net_lixx)
fit2
plot(fit2, mark.time=TRUE, ylim = c(0.75,1), lty=1:2,xlab = "Days since Subscribing", ylab="Percent Surviving", col=2:3)
legend("bottomleft", c('Male', 'Female'), lty=1:2, ncol=2, bty='n', cex = 0.5, col=2:3)
title(main="Netlixx Survival Curve by Gender")

survdiff(survival ~ female, data=net_lixx)


################### 2 Series 2 ##############


netlixx_cox <- read.csv("NetLixxCox.csv")
summary(netlixx_cox)
netlixx_cox$survival <- Surv(netlixx_cox$followtime, netlixx_cox$churn ==1)

results <- coxph(survival ~ female + age + coupon, data=netlixx_cox)
results
summary(results)
cox.zph(results)

newdata <- read.csv("NetLixxCoxPredict.csv")

predict <- survfit(results, newdata = newdata)

head(predict)

############ Series 3 ################
library(survival)
netlix_rmst <- read.csv("NetLixxRMST.csv")
head(netlix_rmst)
netlix_rmst$survival  <- Surv(netlix_rmst$time, netlix_rmst$churned == 1)

fitted <- survfit(survival ~ credit, data=netlix_rmst)
fitted
# summary(fitted)
print(fitted, print.rmean = getOption("survfit.print.rmean"), rmean=180)
(163-123)*5/30

################# Project Mean Customer Lifetime by Modeling Churn ############
net_lixx <- read.csv("NetLixx.csv")

head(net_lixx)

str(net_lixx)
summary(net_lixx)

table(net_lixx$female, net_lixx$churned)

net_lixx$survival <- Surv(net_lixx$time, net_lixx$churned==1)

fit1 <- survfit(survival ~ 1, data=net_lixx)

str(fit1)
fit1
summary(fit1)
plot(fit1, ylim=c(0.75,1))


curve <- fit1$surv

## Modelling
days <- as.numeric(length(curve))
xvar <- 1:days
plot(xvar, curve, "l", c(1,days+365), c(0,1), col="red")
model_fit <- lm(log(curve) ~ xvar)
eq <- function(x) {exp(model_fit$coefficients[[1]] + model_fit$coefficients[[2]]*x)}
curve(eq, add=TRUE, col='blue')
title('Projected NetLixx Survival Curve')
legend("bottomleft", legend = c('Actual', 'Projected'), col = c('red', 'blue'), lty = 1)


integrate(eq,0, Inf)
integrate(eq,0, 730) # 365+365


# Analyzing Customer Churn – Time-Dependent Covariates
data <- read.csv("time_varying_covariate.csv")
str(data)

fit <- coxph(Surv(start_time, end_time, churned) ~ contacted_support, data=data)
summary(fit)
str(fit)

cox.zph(fit)

curve$strata


# time-dependent-coefficient ##########################

tdc <- read.csv("tdc_data.csv")

fit <- coxph(Surv(time, churn)~ male, data=tdc)

summary(fit)

cox.zph(fit)
plot(cox.zph(fit))

#### non time depedent 
netlixx_cox <- read.csv("NetLixxCox.csv")
netlixx_cox$survival <- Surv(netlixx_cox$followtime, netlixx_cox$churn ==1)

results <- coxph(survival ~ female, data=netlixx_cox)
cox.zph(results)
plot(cox.zph(results))


results2 <- coxph(survival ~ age, data=netlixx_cox)
cox.zph(results2)
plot(cox.zph(results2))

fit3 <- coxph(Surv(time, churn) ~ male + tt(male), data=tdc, tt = function(x,t,...)x*t)
summary(fit3)
# (It's worth noting that it's most common to use x * log(t) rather than x * t as the time transform function, but that failed miserably with this dummy data.)