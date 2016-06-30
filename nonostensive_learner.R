detach(data)
rm(list=ls())
library(car)
library(plotrix)
library(bootstrap)
source("~/R/simple_pragmatics/filter_participants.R")
source("~/R/simple_pragmatics/get_coordination.R")
source("~/R/simple_pragmatics/bootstrap_CIs.R")

data <- read.csv("~/R/simple_pragmatics/data/nonostensive_learner.csv")
data <- remove.repeats(data)
data <- filter.responses.2adj(data)
mean.c.bets <- get.coordination()

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
agg.n <- aggregate(b1 ~ n1 + n2 + pred,FUN=length)


pred = matrix(nrow=length(HITId),ncol=4)
c.pred = matrix(nrow=length(HITId),ncol=4)
v1 = array(dim=3)
v2 = array(dim=3)
like <- array(dim=3)

idx <- 1:7
trial.types <- c("1 1","2 1","3 1","2 2","2 2","3 2","3 3")
# for each situation
for (i in 1:length(HITId)) {
	this.pred <- matrix(0,nrow=4,ncol=3)
	f1 <- Input.feature1[i]
	f2 <- Input.feature2[i]	
	
	# for each object
	for (j in 1:3) {
		v1[j] <- Input.adj1[i] == as.character(eval(as.name(
			paste("Input.obj",as.character(j),as.character(f1),
			sep="")))[i])
		v2[j] <- Input.adj2[i] == as.character(eval(as.name(
			paste("Input.obj",as.character(j),as.character(f2),
			sep="")))[i])
	}	
	
	p <- data$Input.position[i]
	# now figure reference probs
	for (j in 1:3) { # obj
		# place scores in correct entries (based on whether this is an adj or notadj object
		this.pred[1 + ((1- v1[j])*2),j] <- (1/sum(v1==v1[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))
		this.pred[2 + ((1-v2[j])*2),j] <- (1/sum(v2==v2[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))
	}

	pred[i,] <- rowSums(this.pred) / sum(rowSums(this.pred))
	
	# figure out what kind of a trial we are
	tt <- idx[trial.types == paste(n1[i],n2[i])]
	
	# adjust for overlap
	if (length(tt)  > 1 & sum(v1 == v2) == 2) {
		tt <- 4 
		# 2 2 A
	} else if(length(tt) > 1)  {
		tt <- 5	# 2 2 B
	}  

	# rearrange coordination bet by the canonical object order (undoing how we averaged it) 
	c.bet <- mean.c.bets[tt,]
	
	# get reference probability
	for (j in 1:3) {like[j] <- v1[j] * (1/sum(v1==v1[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))}
	r.pred <- like / sum(like)
	
	sorted <- sort(r.pred,index.return=TRUE,decreasing=TRUE)
	
	new.ind <- 1:length(r.pred)
	new.ind[sorted$ix] <- new.ind
	
	rearr.c.bet <- c.bet[new.ind] # inverse of sorting
	
	# multiply in the appropriate coordination probabilities
#	this.pred <- this.pred + .05
	
	this.c.pred <- this.pred * matrix(rearr.c.bet/100,nrow=4,ncol=3,byrow=TRUE)
	c.pred[i,] <- rowSums(this.c.pred) / sum(rowSums(this.c.pred))
}

bets <- cbind(Answer.adj1val,Answer.adj2val,Answer.adj1notval,Answer.adj2notval)


#### CONSOLIDATE ####
c <- 1
model.preds <- matrix(nrow=7,ncol=4)
model.c.preds <- matrix(nrow=7,ncol=4)
mean.bets <- matrix(nrow=7,ncol=4)
bet.low.cis <- matrix(nrow=7,ncol=4)
bet.high.cis <- matrix(nrow=7,ncol=4)
for (i in 1:3) {
  for (j in i:3) {	  	
  		ind = ((n1==j & n2==i)) #| (n1==j & n2==i))
		these.bets = bets[ind,]
		these.preds = pred[ind,]
		these.c.preds = c.pred[ind,]
		sc <- c()

		s.bets = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))
		s.preds = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))
		s.c.preds = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))
		
		# sort rows
		sorted <- sort(these.preds[,1],index.return=TRUE)
		s.preds <- these.preds[sorted$ix,]
		s.bets <- these.bets[sorted$ix,]
		s.c.preds <- these.c.preds[sorted$ix,]
		
		# find special cases
		for (k in 1:nrow(these.preds)) {
			sc[k] <- !all(s.preds[k,] == s.preds[length(s.preds[,1]),]) # second one is 2 2 A
		}

		# add to larger matrices
		mean.bets[c,] <- colMeans(s.bets[!sc,])
		bet.low.cis[c,] <- ci.low(s.bets[!sc,])
		bet.high.cis[c,] <- ci.low(s.bets[!sc,])
		
		model.preds[c,] <- s.preds[1,]*100
		model.c.preds[c,] <- s.c.preds[1,]*100 
		c <- c+1
		
		# special case (2 2 B), then add to larger matrices
		if (any(sc)) {
			mean.bets[c,] <- colMeans(s.bets[sc,])
			bet.low.cis[c,] <- ci.low(s.bets[sc,])
			bet.high.cis[c,] <- ci.low(s.bets[sc,])
			model.preds[c,] <- s.preds[1,]*100
			model.c.preds[c,] <- s.c.preds[1,]*100
			c <- c + 1
		}
						
	}
}


### GRAPHIC
titles <- c("1/1","1/2","1/3","2/2 - A","2/2 - B","2/3","3/3")
pdf("~/Projects/Pragmatics/writeup/ICOM Theory/figures/nonostensive_learner.pdf",
	width=6,height=6)
nf <- layout(matrix(c(1,1,2,2,3,3,8,4,4,5,5,9,10,6,6,7,7,11), 3, 6, byrow = TRUE))
for (i in 1:7) {
	plot(NA,xlim=c(1,4),ylim=c(0,60),bty="n",xaxp=c(1,4,3),		xlab="Adjective",ylab="Bet",yaxp=c(0,60,2))
	lines(c(1,2,3,4),mean.bets[i,],col="black",lty=2)
	plotCI(c(1,2,3,4),mean.bets[i,],bet.low.cis[i,],bet.high.cis[i,],pch=NA,add=TRUE)
	lines(c(1,2,3,4),model.preds[i,],col="blue",lty=1)
	lines(c(1,2,3,4),model.c.preds[i,],col="red",lty=1)

	title(paste("Condition",titles[i]))
	
	if (i == 7) {
		legend(3,70,lty=c(1,2,3),col="black",
			c("Data","Model","Model+coord"),bty="n",xpd="n")
	}
}
dev.off()

### CORRELATIONS
summary(lm(as.vector(mean.bets)~as.vector(model.preds)))
summary(lm(as.vector(mean.bets)~as.vector(model.c.preds)))

pdf("~/R/simple_pragmatics/plots/long_nonostensive_learner.pdf",height=12,width=2.5)
nf <- layout(matrix(c(1,2,3,4,5,6,7), 7, 1, byrow = TRUE))
for (i in 1:7) {
	par(mar=c(4,5,2,2))
	plot(NA,xlim=c(1,4),xaxp=c(1,4,3),ylim=c(0,100),xlab="Adjective",ylab="Bet",
		yaxp=c(0,100,2),bty="n",cex.axis=1.5,cex.lab=1.5,xaxt="n")
	axis(1,at=c(1,2,3,4),cex.axis=1,labels=c("blue","square","green","circle"))

	lines(c(1,2,3,4),model.preds[i,],col="orange",lty=1)
	lines(c(1,2,3,4),model.c.preds[i,],col="red",lty=1)
	lines(c(1,2,3,4),mean.bets[i,],col="black",lty=2)
	plotCI(c(1,2,3,4),mean.bets[i,],bet.low.cis[i,],bet.high.cis[i,],pch=NA,add=TRUE)
	
	if (i == 1) {
		legend(1.5,120,lty=c(2,1,1),col=c("black","red","orange"),
			c("Data","Model","Model-E"),bty="n",xpd="n",cex=1.5)
	}
}
dev.off()

agg.n
colSums(agg.n)