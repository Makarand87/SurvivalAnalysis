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
