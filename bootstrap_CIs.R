theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
	mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
	quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}

