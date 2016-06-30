get.speaker <- function () {

data <- read.csv("~/Projects/R/simple_pragmatics/science_final/speaker.csv")
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
agg.n <- aggregate(b1 ~ n1 + n2 + pred,FUN=length)
agg.sem <- aggregate(b1 ~ n1 + n2,FUN=sem)
agg.ci.low <- aggregate(b1 ~ n1 + n2,FUN=ci.low)
agg.ci.high <- aggregate(b1 ~ n1 + n2,FUN=ci.high)

speaker <- list()
speaker[[1]] <- agg.mean
speaker[[2]] <- agg.ci.low
speaker[[3]] <- agg.ci.high
print(length(n1))
return(speaker)

}