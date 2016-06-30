## this function gets the mean coordination scores

get.coordination <- function () {
	
cdata <- read.csv("~/R/simple_pragmatics/science_final/coordination.csv")
cdata <- remove.repeats(cdata)
cdata <- filter.responses.obj(cdata)

# do this because eval doesn't work right
Input.obj1shape <- cdata$Input.obj1shape
Input.obj2shape <- cdata$Input.obj2shape
Input.obj3shape <- cdata$Input.obj3shape
Input.obj1color <- cdata$Input.obj1color
Input.obj2color <- cdata$Input.obj2color
Input.obj3color <- cdata$Input.obj3color
Input.obj1pattern <- cdata$Input.obj1pattern
Input.obj2pattern <- cdata$Input.obj2pattern
Input.obj3pattern <- cdata$Input.obj3pattern

### PREDICTIONS ####
n1 <- cdata$Input.num_objs1
n2 <- cdata$Input.num_objs2
pred = matrix(nrow=length(cdata$HITId),ncol=3)
v1 = array(dim=3)
v2 = array(dim=3)
like <- array(dim=3)

# for each situation
for (i in 1:length(cdata$HITId)) {
  f1 <- cdata$Input.feature1[i]
  f2 <- cdata$Input.feature2[i]	
  
  ## for each object
  for (j in 1:3) {
    v1[j] <- cdata$Input.adj1[i] == as.character(eval(as.name(
                               paste("Input.obj",as.character(j),as.character(f1),
                                     sep="")))[i])
    v2[j] <- cdata$Input.adj2[i] == as.character(eval(as.name(
                               paste("Input.obj",as.character(j),as.character(f2),
                                     sep="")))[i])
  }	
  
  ## now figure reference probs
  for (j in 1:3) {
    like[j] <- v1[j] * (1/sum(v1==v1[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))
  }
  
  pred[i,] <- like / sum(like)
}

bets <- cbind(cdata$Answer.obj1val,cdata$Answer.obj2val,cdata$Answer.obj3val)

c <- 1
mean.bets <- matrix(nrow=7,ncol=3)

for (i in 1:3) {
  for (j in i:3) {	  	
    ind = ((n1==j & n2==i)) #| (n1==j & n2==i))
    these.bets = bets[ind,]
    these.preds = pred[ind,]
    sc <- c()
    
    s.bets = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))
    s.preds = matrix(nrow=nrow(these.bets),ncol=ncol(these.bets))
    
    ## note that this ignores overlap
    for (k in 1:nrow(these.bets)) {
      sorted <- sort(these.preds[k,],index.return=TRUE,decreasing=TRUE)
      s.preds[k,] <- sorted$x
      s.bets[k,] <- these.bets[k,sorted$ix]
    }
    
    sorted <- sort(s.preds[,1],index.return=TRUE)
    s.preds <- s.preds[sorted$ix,]
    s.bets <- s.bets[sorted$ix,]
    
    ## find special cases
    for (k in 1:nrow(these.preds)) {
      sc[k] <- !all(s.preds[k,] == s.preds[1,])
    }
    
    ## save the mean bets
    mean.bets[c,] <- colMeans(s.bets[!sc,])
    c <- c + 1
    
    ## special cases
    if (any(sc)) {
      
      ## save the mean bets
      mean.bets[c,] <- colMeans(s.bets[sc,])
      c <- c + 1
    }					
  }
}

print(paste("number of records =",as.character(length(cdata$Approve))))
return(mean.bets)
}
