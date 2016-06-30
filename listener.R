detach(data)
rm(list=ls())
library(car)
library(plotrix)
library(bootstrap)
source("~/R/simple_pragmatics/science_final/filter_participants.R")
source("~/R/simple_pragmatics/get_coordination.R")
source("~/R/simple_pragmatics/science_final/bootstrap_CIs.R")

data <- read.csv("~/R/simple_pragmatics/science_final/listener.csv")
data <- remove.repeats(data)
data <- filter.responses.obj(data)
mean.c.bets <- get.coordination()

attach(data)


### PREDICTIONS ####
n1 <- Input.num_objs1
n2 <- Input.num_objs2
agg.n <- aggregate(data$Input.feature1 ~ n1 + n2,FUN=length)


pred = matrix(nrow=length(HITId),ncol=3)
v1 = array(dim=3)
v2 = array(dim=3)
like <- array(dim=3)

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
	
	# now figure reference probs
	for (j in 1:3) {
		like[j] <- v1[j] * (1/sum(v1==v1[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))
#		this.pred[1 + ((1- v1[j])*2),j] <- (1/sum(v1==v1[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))
#		this.pred[2 + ((1-v2[j])*2),j] <- (1/sum(v2==v2[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))
	}

	pred[i,] <- like / sum(like)
}

bets <- cbind(Answer.obj1val,Answer.obj2val,Answer.obj3val)


#### CONSOLIDATE ####
c <- 1
model.preds <- matrix(nrow=7,ncol=3)
model.c.preds <- matrix(nrow=7,ncol=3)
mean.bets <- matrix(nrow=7,ncol=3)
cond.n <- matrix(nrow=1,ncol=7)
bet.low.cis <- matrix(nrow=7,ncol=3)
bet.high.cis <- matrix(nrow=7,ncol=3)

for (i in 1:3) {
  for (j in i:3) {	  	
  		ind = ((n1==j & n2==i)) 
		these.bets = bets[ind,]
		these.preds = pred[ind,]
		sc <- c()

		s.bets = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))
		s.preds = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))

		# note that this ignores overlap
		# special case: i = 1, j = 3, sort opposite
		for (k in 1:nrow(these.bets)) {
			if (i == 1 & j == 3) {
				sorted <- sort(these.preds[k,],index.return=TRUE,decreasing=FALSE)
			} else {
				sorted <- sort(these.preds[k,],index.return=TRUE,decreasing=TRUE)
			}
			s.preds[k,] <- sorted$x
			s.bets[k,] <- these.bets[k,sorted$ix]
		}
		
		sorted <- sort(s.preds[,1],index.return=TRUE)
		s.preds <- s.preds[sorted$ix,]
		s.bets <- s.bets[sorted$ix,]
		
		# find special cases
		for (k in 1:nrow(these.preds)) {
			sc[k] <- !all(s.preds[k,] == s.preds[1,])
		}

		# add to larger matrices
		mean.bets[c,] <- colMeans(s.bets[!sc,])
		bet.low.cis[c,] <- ci.low(s.bets[!sc,])
		bet.high.cis[c,] <- ci.low(s.bets[!sc,])
		
		model.preds[c,] <- s.preds[1,]*100
		
		# calculate coordiantion adjusted model and renormalize
		# special case, i = 1, j = 3, reverse mean.c.bets
		if (i == 1 & j == 3) {
			c.adj.bets <- (s.preds[1,] * mean.c.bets[c,c(3,2,1)]/100)
		} else {
			c.adj.bets <- (s.preds[1,] * mean.c.bets[c,]/100)
		}
		c.adj.bets <- 100 * (c.adj.bets / sum(c.adj.bets))
		model.c.preds[c,] <- c.adj.bets
		cond.n[c] <- sum(!sc)
		c <- c+1
		
		# special cases
		if (any(sc)) {
			c.adj.bets <- (s.preds[length(s.preds[,1]),] * mean.c.bets[c,]/100)
			c.adj.bets <- 100 * (c.adj.bets / sum(c.adj.bets))
			
			# add to larger matrices
			mean.bets[c,] <- colMeans(s.bets[sc,])
			bet.low.cis[c,] <- ci.low(s.bets[sc,])
			bet.high.cis[c,] <- ci.low(s.bets[sc,])
			model.preds[c,] <- s.preds[length(s.preds[,1]),]*100
			model.c.preds[c,] <- c.adj.bets		
			cond.n[c] <- sum(sc)
			c <- c + 1
		}
						
	}
}


### GRAPHIC
titles <- c("1/1","2/1","3/1","2/2 - A","2/2 - B","3/2","3/3")
pdf("~/Projects/Pragmatics/writeup/ICOM Theory/figures/listener.pdf",
	width=6,height=6)
nf <- layout(matrix(c(1,1,2,2,3,3,8,4,4,5,5,9,10,6,6,7,7,11), 3, 6, byrow = TRUE))
for (i in 1:7) {
	plot(NA,xlim=c(1,3),ylim=c(0,100),bty="n",xaxp=c(1,3,2),		xlab="Object",ylab="Bet",yaxp=c(0,100,2))
	lines(c(1,2,3),mean.bets[i,],col="black")
	plotCI(c(1,2,3),mean.bets[i,],bet.low.cis[i,],bet.high.cis[i,],pch=NA,add=TRUE)
	lines(c(1,2,3),model.preds[i,],col="black",lty=2)
	lines(c(1,2,3),model.c.preds[i,],col="black",lty=3)

	title(paste("Condition",titles[i]))
	
	if (i == 7) {
		legend(2,110,lty=c(1,2,3),col="black",
			c("Data","Model","Model+coord"),bty="n",xpd="n")
	}
}
dev.off()

### CORRELATIONS
summary(lm(as.vector(mean.bets)~as.vector(model.preds)))
summary(lm(as.vector(mean.bets)~as.vector(mean.c.bets)))
summary(lm(as.vector(mean.bets)~as.vector(model.c.preds)))


### PLOT #2
pdf("~/R/simple_pragmatics/plots/long_listener.pdf",height=12,width=2.5)
nf <- layout(matrix(c(1,2,3,4,5,6,7), 7, 1, byrow = TRUE))
for (i in 1:7) {
	par(mar=c(4,5,2,2))
	plot(NA,xlim=c(1,3),xaxp=c(1,3,2),ylim=c(0,100),xlab="Object",ylab="Bet",
		yaxp=c(0,100,2),bty="n",cex.axis=1.5,cex.lab=1.5)
#	plot(NA,xlim=c(1,3),xaxp=c(1,3,2),ylim=c(0,100),xlab="Object",ylab="Bet",
#		yaxp=c(0,100,2),bty="n")


#	points(NA,)
	lines(c(1,2,3),model.preds[i,],col="orange",lty=1)
	lines(c(1,2,3),model.c.preds[i,],col="red",lty=1)
	lines(c(1,2,3),mean.bets[i,],col="black",lty=2)
	plotCI(c(1,2,3),mean.bets[i,],bet.low.cis[i,],bet.high.cis[i,],pch=NA,add=TRUE)

	#title(paste("Condition",titles[i]))
	#text(2.75,100,paste("N =",cond.n[i]),xpd="n")
	if (i == 1) {
		legend(1.5,120,lty=c(2,1,1),col=c("black","red","orange"),
			c("Data","Model","Model-E"),bty="n",xpd="n",cex=1.5)
	}
}
dev.off()

agg.n
colSums(agg.n)