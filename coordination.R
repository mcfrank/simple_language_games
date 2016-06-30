detach(data)
rm(list=ls())
library(car)
library(plotrix)
library(bootstrap)
source("~/R/simple_pragmatics/filter_participants.R")

data <- read.csv("~/R/simple_pragmatics/data/coordination_mumble.csv")
data <- remove.repeats(data)
data <- filter.responses.obj(data)
attach(data)


### PREDICTIONS ####
n1 <- Input.num_objs1
n2 <- Input.num_objs2
pred = matrix(nrow=length(HITId),ncol=3)
v1 = array(dim=3)
v2 = array(dim=3)
like <- array(dim=3)

# for each situation
for (i in 1:length(HITId)) {
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
	}

	pred[i,] <- like / sum(like)
}

bets <- cbind(Answer.obj1val,Answer.obj2val,Answer.obj3val)

c <- 1
mean.bets <- matrix(nrow=7,ncol=3)
#### GRAPHIC ####
nf <- layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))
for (i in 1:3) {
  for (j in i:3) {	  	
  		ind = ((n1==j & n2==i)) #| (n1==j & n2==i))
		these.bets = bets[ind,]
		these.preds = pred[ind,]
		sc <- c()

		s.bets = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))
		s.preds = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))

		# note that this ignores overlap
		for (k in 1:nrow(these.bets)) {
			sorted <- sort(these.preds[k,],index.return=TRUE,decreasing=TRUE)
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

		plot(NA,xlim=c(1,3),ylim=c(0,100),bty="n",xaxp=c(1,3,2),			xlab="object",ylab="bet")
		#lines(c(1,2,3),s.preds[1,]*100,col="black",lty=2)
		lines(c(1,2,3),colMeans(s.bets[!sc,]),col="black")

		# save the mean bets
		mean.bets[c,] <- colMeans(s.bets[!sc,])
		c <- c + 1
		
		# special cases
		if (any(sc)) {
			#lines(c(1,2,3),s.preds[length(s.preds[,1]),]*100,col="red",lty=2)
			lines(c(1,2,3),colMeans(s.bets[sc,]),col="red")
		
			# save the mean bets
			mean.bets[c,] <- colMeans(s.bets[sc,])
			c <- c + 1
		}
						
		title(paste(j,i))
#		if (i == 1 & j == 1) { legend(2,110,lty=c(1,2),col="black",c("data","model"),bty="n")}
#		if (i == 2 & j == 2) { legend(2,110,lty=c(1,2,1,2),xpd="n",
#			col=c("black","black","red","red"),c("data","model","data case 2","model case 2"),bty="n")}
	}
}
