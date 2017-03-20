library(help=survival)
library()
library(survival)
data(aml)
View(aml)

stime <- c(2.2, 3, 8.4, 7.5)
status <- c(1, 0, 1, 0)
Surv(aml$time, aml$status)




###  Load the package Survival
lifetimes <- rexp(25, rate = 0.2) # exponential distri wioth rate 0.2
censtimes <- 5 + 5*runif(25) # unifomr distri
ztimes <- pmin(lifetimes, censtimes)
status <- as.numeric(censtimes > lifetimes)
head(ztimes)
head(status)
head(cbind(lifetimes, censtimes, ztimes, status))


# The Kaplan-Meier estimator
fit1 <- survfit(Surv(aml$time[1:11], aml$status[1:11])~1)
summary(fit1)
plot(fit1)

# cumulative hazard function by Nelson-Aalen estimator
    #  Fleming and Harrington  estimator
fit2 <- survfit(Surv(aml$time[1:11], aml$status[1:11])~1, type="fh")
plot(fit2)
summary(fit2)
list(fit2$time, -log(fit2$surv))

# 3 The Log Rank Test and relatives ----

survdiff(Surv(time, status) ~ x, data = aml) #This does a two sample log-rank test,


# 4 Parametric regression models -----
model1 <- survreg(Surv(time, status) ~ x, data = aml, dist = "exponential")
model2 <- survreg(Surv(time, status) ~ x, data = aml, dist = "weibull")

# for iid model
modeliid <- survreg(Surv(time, status) ~ 1, data = aml)


summary(model1)
summary(model2)

predict(model1, type = "quantile", p=c(0.1, 0.5, 0.9))


# 4.1 Confidance Region 
amlnew <- data.frame(aml, x1=c(rep(0,11), rep(1,12))) #we need to convert x to numerical.
View(amlnew)

model3 <- survreg(Surv(time, status) ~ offset(0.3*x1), data = amlnew, dist="weibull")

exploglik <- function(time, status, lam)  {
  sum(status*log(lam)) - lam*sum(time)
}
exploglik(aml$time, aml$status, lam = exp(-3.628))



extremeloglik <- function(time, status, mu) {
  sum(status*(time-mu)) - sum( exp(time -mu))
}
extremeloglik(time= log(aml$time), status=aml$status, mu= 3.628776)

# -2*(exploglik(time, status, lam=theta)
   
data(cancer)

cmodel1 <- survreg(Surv(time, status)~ age + sex,data=cancer, dist="weibull")
betas <- 1:95/100
llik <- rep(0,95)
for (i in 1:95) {
  llik[i] <- survreg(Surv(time, status) ~ age+offset(betas[i]*sex),
                     data=cancer, dist="weibull")$loglik[2]
}


plot( betas, llik, type="l" )
abline( h = max(llik) - 3.84/2 )


# 4.2 Interval censored data

Surv(time = 2, time2 = 3, event = 3, type = "interval")


# 5 Cox regression models. ----

coxfit1 <- coxph(Surv(time, status)~x, data=aml)
summary(coxfit1)

# To obtain the (cumulative) baseline hazard estimator:
  basehaz(coxph(Surv(time, status)~x, data=aml))

plot(coxfit1)


# 5.2 Cox regression model with time change covariates. II