us <- read.table("http://data.princeton.edu/pop509/us2013s.dat", header=TRUE)
head(us)

library(foreign)


co <- read.dta("http://data.princeton.edu/eco572/datasets/co3539.dta")
head(co)
names(co) <- c("age", "agemar")
head(co)
summary(co)
str(co)
hist(co$age, breaks = seq(35,39,1), labels = TRUE, ylim=c(0,300))
hist(co$agemar , labels = TRUE, xlim = c(10,90), breaks =  seq(9,88,1))#, xlim = c(10,100))
# d <- density(co$agemar )
# plot(d)
# polygon(d, col="red", border="blue")

library(dplyr)

cog <- mutate(co, agemar  = ifelse(agemar  >= age, NA, agemar )) %>%
  group_by(age, agemar ) %>% summarize(n=n())
cog
data.frame(cog)

table(as.factor(co$age), as.factor(co$agemar ))

plot(table(as.factor(co$agemar ), as.factor(co$age)))
plot(table(as.factor(cog$age), as.factor(cog$agemar )))


table(as.factor(cog$age), as.factor(cog$agemar ))


cog <- group_by(cog, age) %>% mutate(f = n/sum(n), F = cumsum(f) - f)
cog
mean(cog$F)


age <- seq(11, 36,1)
# library(devtools)
# install_github("grodri/nuptfer")
library(nuptfer)
Fits <- data.frame(age=age, 
                   Coale = pnupt(age, 20.436, 5.377, 0.885), 
                   Hernes = phernes(age, 0.6526, 0.1506, 0.8873))
dens <- function(x) c(diff(x), NA)
fits <- mutate_each(Fits, funs(dens), -age)

library(ggplot2)
(g1 <- ggplot(cog, aes(agemar, F)) + 
  geom_point(col="green") + 
  geom_line(data=Fits, aes(age, Coale), linetype=1) + 
  geom_line(data=Fits, aes(age, Hernes), linetype=2))


(g2 <- ggplot(cog, aes(agemar+0.5,f)) + xlab("age") +
  geom_point() +
  geom_line(data=fits, aes(age, Coale), linetype=1) + 
  geom_line(data=fits, aes(age, Hernes), linetype=2)
)
# install.packages("gridExtra")
library(gridExtra)

g <- arrangeGrob(g1, g2, ncol=2)

ggsave("co3539ar.png", plot=g, width = 10, height=5, dpi=72)


plot(cog$agemar, cog$F)

cog <-    mutate(cog,  observed = factor(agemar < age - 15, labels=c("no","yes")))


Fits <- data.frame(age=age, 
                   Coale = pnupt(age = age,mean = 20.1528, stdev = 5.0955, pem = 0.8740),
                   Hernes = phernes(age = age, A = 0.7321, r = 0.1875, pem = 0.8296))
fits <- mutate_each(Fits, funs(dens), -age)


(g1 <- ggplot(cog, aes(agemar,F, color=observed)) + xlab("age") + geom_point() +
  geom_line(data = Fits, aes(age, Coale, color=NULL), linetype=1) +
  geom_line(data = Fits, aes(age, Hernes, color=NULL), linetype=2) +
  theme(legend.position = "none") 
)
(g2 <- ggplot(cog, aes(agemar + 0.5, f, color=observed)) + xlab("age") + geom_point() +
    geom_line(data = fits, aes(age, Coale, color=NULL), linetype=1) +
            geom_line(data = fits, aes(age, Hernes, color=NULL), linetype=2) + 
    theme(legend.position = "none")
)
(g <- arrangeGrob(g1, g2, ncol = 2))

ggsave("co3539cr.png", plot = g, width = 10, height = 5, dpi = 72)













########### Infant and Child Mortality in Colombia ----

# Data Preparation

require(foreign)
somoza <- read.dta("http://data.princeton.edu/wws509/datasets/somoza.dta")
str(somoza)
summary(somoza)
somoza
tapply(somoza$alive, somoza$sex, summary)

s <- aggregate(dead ~ cohort + age, data = somoza, sum)
s

s$alive <- aggregate(alive ~ cohort + age, data=somoza, sum)[,"alive"]
s
s <- s[order(s$cohort, s$age), ]
s
s$exposure <- 0
s

w <- c(1,2,3,6,12,24,36,60)/12
# Table 7.1
for(cohort in levels(s$cohort)) {
  i <- which(s$cohort == cohort)
             data <- s[i,]
             n <- sum(data$alive + data$dead)
             exit <- n - cumsum(data$alive + data$dead)
             enter <- c(n, exit[-length(exit)])
             s[i,"exposure"] <- w*(enter+exit)/2
}

s

# After calculating exposure I dropped kids older than ten, as we are only interested in survival to age ten. 
# I also renamed "dead" to "deaths", which makes more sense

co <- subset(s, age != "10+ years")
co$age <- factor(co$age)
names(co)[3] <- "deaths"
co


#########Poisson Model (NULL) ##############
co$os <- log(co$exposure)
#NULL Model
em <- glm(deaths ~ offset(os), family=poisson, data=co)
summary(em)
deviance(em)

em0 <- glm(deaths ~ 1, family=poisson, data=co)
summary(em0)
# Age
em2 <- glm(deaths ~ offset(os) + age, family = poisson, data=co)
summary(em2)
#Cohort
em3 <- glm(deaths ~ offset(os) + cohort, family=poisson, data=co)
summary(em3)
#Additive
em4 <- glm(deaths ~ offset(os) + age + cohort, family=poisson, data=co)
summary(em4)

em5 <- glm(deaths ~ offset(os) + age*cohort, family=poisson, data=co)
summary(em5)


exp(coef(em))
# exp(-3.996449 ) * 1000

sum(co$deaths) / sum(co$exposure)
# identical((sum(co$deaths) / sum(co$exposure)), exp(coef(em)))

exp(coef(em2))

exp(coef(em4))
pchisq(deviance(em4), em4$df.residual, lower.tail = FALSE)

j <- grep("cohort", names(coef(em4)))
j
exp(coef(em4)[j]) - 1


# Estimating Survival Probabilities

co$hazard <- predict(em4, type="response")/co$exposure
co
w
w <- w[-8]
w
