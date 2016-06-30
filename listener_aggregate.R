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
l.ch <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=ci.high)
l.cl <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=ci.low)

c.ms <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=cd,FUN=mean)
c.ch <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=cd,FUN=ci.high)
c.cl <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=cd,FUN=ci.low)

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
nf <- layout(matrix(c(1,2,3), 3, 1, byrow = TRUE))
plotCI(c(1,2,3),c.ms[4,5:7],liw=c.cl[4,5:7],uiw=c.ch[4,5:7],pch=20,
	bty="n",ylim=c(0,60),xlab="object",ylab="Bet",xaxp=c(1,3,2),col="red",
	main="Contexual salience")
# lines(c(1,2,3),c.ms[4,5:7],lty=1)	
	
plot(c(1,2,3),l.ms[4,2:4],type="l",bty="n",ylim=c(0,60),
	xlab="object",ylab="Bet",xaxp=c(1,3,2),col="red",lty=2,
	main="Informative communication")
plot(c(1,2,3),l.ms[4,8:10],type="l",lty=2,xaxp=c(1,3,2),
	bty="n",ylim=c(0,60),xlab="object",ylab="Bet",col="red",
	main="Full model with human data")
plotCI(c(1,2,3),l.ms[4,5:7],liw=l.cl[4,5:7],uiw=l.ch[4,5:7],pch=20,add=T)
legend(2.5,70,pch=c(20,20),lty=c(NA,2),col=c("black","red"),
	c("data","model"),xpd="n",bty="n")

