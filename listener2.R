rm(list=ls())

library(car)
library(plotrix)
library(bootstrap)

source("~/R/simple_pragmatics/filter_participants.R")
source("~/R/simple_pragmatics/listener_helper.R")
source("~/R/simple_pragmatics/bootstrap_CIs.R")

## get listener data
ldata <- read.csv("~/R/simple_pragmatics/data/listener.csv")
ldata <- remove.repeats(ldata)
ldata <- filter.responses.obj(ldata)
ld <- return.sorted.bets(ldata)

## get coordination data
cdata <- read.csv("~/R/simple_pragmatics/data/coordination.csv")
cdata <- remove.repeats(cdata)
cdata <- filter.responses.obj(cdata)
cd <- return.sorted.bets(cdata)

## aggregate within conditions
l.ms <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=mean)
c.ms <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=cd,FUN=mean)
l.ch <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=ci.high)
l.cl <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=ci.low)

## now create new predictions with E
epreds <- c("e.pred1","e.pred2","e.pred3")
bets <- c("bet1","bet2","bet3")
preds <- c("pred1","pred2","pred3")

l.ms[,epreds] <- (l.ms[,preds] / 100) * (c.ms[,bets] / 100)
l.ms[,epreds] <- 100 * l.ms[,epreds] / rowSums(l.ms[,epreds])

## 
plotCI(l.ms$e.pred1,l.ms$bet1,liw=l.cl$bet1,uiw=l.ch$bet1,
	pch=20,bty="n",xlim=c(0,100),ylim=c(0,100),
	xlab="Model predictions",ylab="Participant bets")
plotCI(l.ms$e.pred2,l.ms$bet2,liw=l.cl$bet2,uiw=l.ch$bet2,pch=20,add=T)
plotCI(l.ms$e.pred3,l.ms$bet3,liw=l.cl$bet3,uiw=l.ch$bet3,pch=20,add=T)
x <- c(l.ms$e.pred1,l.ms$e.pred2,l.ms$e.pred3)
y <- c(l.ms$bet1,l.ms$bet2,l.ms$bet3)
regLine(lm(y ~ x))

## now plot
nf <- layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2, byrow = TRUE))
x <- c(1,2,3)
for (i in 1:7) {
    plot(x,l.ms[i,bets],lty=2,type="l",
         bty="n",xlim=c(1,3),ylim=c(0,100))
    lines(x,l.ms[i,preds],col="blue")
    lines(x,l.ms[i,epreds],col="red")
}

## stats
summary(lm(c(l.ms[,5],l.ms[,6],l.ms[,7])~c(l.ms[,8],l.ms[,9],l.ms[,10])))
#summary(lm(as.vector(mean.bets)~as.vector(mean.c.bets)))
#summary(lm(as.vector(mean.bets)~as.vector(model.c.preds)))

## other exploration
summary(lm(bet1 ~ tt + f1 + f2,
           data=ld))
