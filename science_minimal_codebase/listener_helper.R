## ----------------------------------------------------------------------
## sort predictions and bets according to trial type and prediction order
return.sorted.bets <- function(data) {
    v1 = array(dim=3)
    v2 = array(dim=3)
    sdata <- data.frame(WorkerId=data$WorkerId,
                     pred1=NA,pred2=NA,pred3=NA,
                     bet1=NA,bet2=NA,bet3=NA)

    ## for each situation
    for (i in 1:length(data$HITId)) {
        sdata$f1[i] <- as.character(data$Input.feature1[i])
        sdata$f2[i] <- as.character(data$Input.feature2[i])
        
        ## for each object
        for (j in 1:3) {
            v1[j] <- data$Input.adj1[i] == as.character(data[i,paste("Input.obj",
                                      as.character(j),sdata$f1[i],sep="")])
            v2[j] <- data$Input.adj2[i] == as.character(data[i,paste("Input.obj",
                                      as.character(j),sdata$f2[i],sep="")])
        }	

        ## get predictions and trial type
        like <- get.listener.preds(v1,v2)
        sdata$tt[i] <- get.trial.type(v1,v2)

        ## now sort into canonical order
        sorted <- sort(like,index.return=TRUE,decreasing=TRUE)
        
        ## now store sorted preds and bets
        sorted.like <- like[sorted$ix]
        sorted.bets <- c(data$Answer.obj1val[i],data$Answer.obj2val[i],
                         data$Answer.obj3val[i])[sorted$ix]
        sdata[i,c("pred1","pred2","pred3")] <- sorted.like
        sdata[i,c("bet1","bet2","bet3")] <- sorted.bets
    }

    return(sdata)
}

## ----------------------------------------------------------------------
## get predictions for the listener
## this will allow us to find a single canonical ordering
get.listener.preds <- function(v1,v2) {
    like <- c(NA,NA,NA)
    for (j in 1:3) {
        like[j] <- v1[j] * (1/sum(v1==v1[j])) / (1/sum(v1==v1[j]) + 1/sum(v2==v2[j]))
    }

    return(100 * like / sum(like))
}

## ----------------------------------------------------------------------
## figure out what kind of a trial we are
get.trial.type <- function(v1,v2) {
    idx <- 1:7
    trial.types <- c("1 1","1 2","1 3","2 2","2 2","2 3","3 3")
    trial.type.names <- c("1/1","1/2","1/3","2/2 sym","2/2 asym","2/3","3/3")

    sv <- c(sum(v1),sum(v2))
    
    tt <- idx[trial.types == paste(min(sv),max(sv))]
    
    ## adjust for overlap
    if (length(tt) > 1 & sum(v1==T & v2==T) == 2) {
        tt <- 4 # 2 2 A
        
    } else if(length(tt) > 1)  {
        tt <- 5 # 2 2 B
    }

    return(trial.type.names[tt])
}
