rm(list=ls())

library(car)
library(plotrix)
library(bootstrap)

source("~/R/simple_pragmatics/science_final/filter_participants.R")
source("~/R/simple_pragmatics/listener_speaker_helper.R")
source("~/R/simple_pragmatics/science_final/bootstrap_CIs.R")
source("~/R/simple_pragmatics/science_final/get_speaker.R")

## get coordination data
cdata <- read.csv("~/R/simple_pragmatics/science_final/coordination.csv")
cdata <- remove.repeats(cdata)
cdata <- filter.responses.obj(cdata)
cd <- return.sorted.bets(cdata)

## get speaker data too 
speaker <- get.speaker()

## get listener data
ldata <- read.csv("~/R/simple_pragmatics/science_final/listener.csv")
ldata <- remove.repeats(ldata)
ldata <- filter.responses.obj(ldata)
ld <- return.sorted.bets(ldata,speaker)

## aggregate within conditions
l.ms <- aggregate(cbind(pred1,pred2,pred3,spred1,spred2,spred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=mean)
c.ms <- aggregate(cbind(pred1,pred2,pred3,bet1,bet2,bet3) ~ tt,data=cd,FUN=mean)
l.ch <- aggregate(cbind(pred1,pred2,pred3,spred1,spred2,spred3,pred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=ci.high)
l.cl <- aggregate(cbind(pred1,pred2,pred3,pred3,spred1,spred2,spred3,bet1,bet2,bet3) ~ tt,data=ld,FUN=ci.low)

## now create new predictions with E (contextual salience)
epreds <- c("e.pred1","e.pred2","e.pred3")
espreds <- c("es.pred1","es.pred2","es.pred3")
bets <- c("bet1","bet2","bet3")
preds <- c("pred1","pred2","pred3")
spreds <- c("spred1","spred2","spred3")

l.ms[,epreds] <- (l.ms[,preds] / 100) * (c.ms[,bets] / 100)
l.ms[,epreds] <- 100 * l.ms[,epreds] / rowSums(l.ms[,epreds])

# combined with speaker also
l.ms[,espreds] <- (l.ms[,spreds] / 100) * (c.ms[,bets] / 100)
l.ms[,espreds] <- 100 * l.ms[,espreds] / rowSums(l.ms[,espreds])


###### FIG 1B FOR PAPER ######
## plot of all data
layout(matrix(1:2,nrow=1,ncol=2))
plotCI(l.ms$e.pred1,l.ms$bet1,liw=l.cl$bet1,uiw=l.ch$bet1,
	pch=20,bty="n",xlim=c(0,100),ylim=c(0,100),
	xlab="Model predictions",ylab="Participant bets")
plotCI(l.ms$e.pred2,l.ms$bet2,liw=l.cl$bet2,uiw=l.ch$bet2,pch=20,add=T)
plotCI(l.ms$e.pred3,l.ms$bet3,liw=l.cl$bet3,uiw=l.ch$bet3,pch=20,add=T)
x <- c(l.ms$e.pred1,l.ms$e.pred2,l.ms$e.pred3)
y <- c(l.ms$bet1,l.ms$bet2,l.ms$bet3)
regLine(lm(y ~ x))
summary(lm(y ~ x))
title('Speaker model')

plotCI(l.ms$es.pred1,l.ms$bet1,liw=l.cl$bet1,uiw=l.ch$bet1,
	pch=20,bty="n",xlim=c(0,100),ylim=c(0,100),
	xlab="Model predictions",ylab="Participant bets")
plotCI(l.ms$es.pred2,l.ms$bet2,liw=l.cl$bet2,uiw=l.ch$bet2,pch=20,add=T)
plotCI(l.ms$es.pred3,l.ms$bet3,liw=l.cl$bet3,uiw=l.ch$bet3,pch=20,add=T)
x <- c(l.ms$es.pred1,l.ms$es.pred2,l.ms$es.pred3)
y <- c(l.ms$bet1,l.ms$bet2,l.ms$bet3)
regLine(lm(y ~ x))
summary(lm(y ~ x))
title('Human speaker data')

# # add speaker data
# plotCI(speaker[[1]]$pred,speaker[[1]]$b1,liw=speaker[[2]]$b1,uiw=speaker[[3]]$b1,add=T)

# # regression line
# regLine(lm(c(y,speaker[[1]]$b1) ~ c(x,speaker[[1]]$pred)))
# legend("bottomright",pch=c(20,1),c("listener","speaker"),bty="n")


###### CORRELATION VALUES FOR PAPER ######
# speaker
cor.test(speaker[[1]]$b1,speaker[[1]]$pred)

# salience with listener
cor.test(c(c.ms$bet1,c.ms$bet2,c.ms$bet3),c(l.ms$bet1,l.ms$bet2,l.ms$bet3))

# preds without salience
cor.test(c(l.ms$pred1,l.ms$pred2,l.ms$pred3),c(l.ms$bet1,l.ms$bet2,l.ms$bet3))

# preds with salience
cor.test(y,x)

# nonzero points
cor.test(y[x>0 & x<100],x[x>0 & x<100])
cor.test(y[x>0 & x<100],x[x>0 & x<100])


###### FIG 1C FOR PAPER ######
## condition by condition, can extract panel 1C
nf <- layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2, byrow = TRUE))
x <- c(1,2,3)
for (i in 1:7) {
    plot(x,l.ms[i,bets],lty=2,type="l",
         bty="n",xlim=c(1,3),ylim=c(0,100))
    lines(x,l.ms[i,preds],col="blue")
    lines(x,l.ms[i,epreds],col="red")
}