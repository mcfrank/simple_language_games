detach(data)
rm(list=ls())
library(car)
library(plotrix)
library(bootstrap)
source("~/Projects/R/simple_pragmatics/science_final/filter_participants.R")

data <- read.csv("~/Projects/R/simple_pragmatics/data/ostensive_learner.csv")
data <- remove.repeats(data)
data <- filter.responses.adj(data)
attach(data)

### PREDICTIONS ####
n1 <- Input.num_objs1
n2 <- Input.num_objs2
b1 <- Answer.adj1val
b2 <- Answer.adj2val

reverse <- n2 < n1
pred <- 100* (1/n1) / ((1/n1) + (1/n2))
pred[reverse] <- 100 - pred[reverse]
b1[reverse] <- b2[reverse]

sem <- function(x) {sd(x) / sqrt(length(x))}

theta <- function(x) {mean(x)}

ci.low <- function(x) {
	mean(x) - quantile(bootstrap(x,10000,theta)$thetastar,.025)}
ci.high <- function(x) {
	quantile(bootstrap(x,10000,theta)$thetastar,.975) - mean(x)}

agg.mean <- aggregate(b1 ~ n1 + n2 + pred,FUN=mean)
agg.sem <- aggregate(b1 ~ n1 + n2,FUN=sem)
agg.n <- aggregate(b1 ~ n1 + n2,FUN=length)
agg.ci.low <- aggregate(b1 ~ n1 + n2,FUN=ci.low)
agg.ci.high <- aggregate(b1 ~ n1 + n2,FUN=ci.high)

trial.types <- c("1/1","2/2","3/3","2/3","1/2","1/3")
pdf("~/Projects/Pragmatics/writeup/ICOM Theory/figures/ostensive_learner.pdf",
	width=5,height=5)
plotCI(agg.mean$pred,agg.mean$b1,agg.ci.low$b1,agg.ci.high$b1,	xlim=c(40,90),ylim=c(40,90),bty="n",
	xlab="Model predictions",ylab="Mean bet",
	plt=c(.1,.9,.1,.9))
text(agg.mean$pred,agg.mean$b1,trial.types,pos=4)
title("Experiment 3: Ostensive Learner")
lines(c(50,80),c(50,80),lty=2)
lines(c(40,90),c(50,50),lty=3)
dev.off()

summary(lm(b1 ~ pred,data=agg.mean))

agg.n
colSums(agg.n)