# Draw approximation of binomial distribution
# Songpeng Zu
# 2015-04-03

meanbinomial <- function(n,x,m){
    return(mean(rbinom(m,n,x)))
}

samplemeanbinormial <- function(n,x,m,times){
    array <- rep(0,times)
    for (i in 1:times) {
        array[i] <- meanbinomial(n,x,m)
    }
    return(array)
}

# Draw one distribution,
drawbinomial <- function(n,x,m,times,alphastar){
    array <- samplemeanbinormial(n,x,m,times)
    d <- density(array)
    plot(d,main="Density of mean of alpha",xlab= paste(("m is"),toString(m)))
    par(new=TRUE)
    polygon(d,col="grey",border="black")
    abline(v = alphastar, col="blue",lwd=2,lty=2)
    abline(v = n*x, col="red",lwd=2,lty=2)
}

# Draw two distributions in one figure.
# Use sm.density.compare in the sm package.
library(sm) # If you don't have this package, try install.packages("sm")
drawtwobinomial <- function(n,x,m1,m2,times,alphastar){
    array1 <- samplemeanbinormial(n,x,m1,times)
    array2 <- samplemeanbinormial(n,x,m2,times)
    group <- factor(c(rep(0,times),rep(1,times)),labels=c("m is 5","m is 20"))
    sm.density.compare(c(array1,array2),group,0.7,xlab="Different values of m.",kernal="gaussian")
    legend("topright",levels(group),fill=c(2:(2+length(levels(group)))))
    abline(v = alphastar, col="blue",lwd=2,lty=2)
    abline(v = n*x, col="red",lwd=2,lty=2)
}

# Draw two cdf in one figure.
library(ggplot2) # If you don't have this package,try install.package("ggplot2")
drawtwocdfnomial <- function(n,x,m1,m2,times,alphastar){
    array1 <- samplemeanbinormial(n,x,m1,times)
    array2 <- samplemeanbinormial(n,x,m2,times)
    group <- factor(c(rep(0,times),rep(1,times)),labels=c("m is 5","m is 20"))
    df <- data.frame(x = c(array1,array2),team=group)
    ggplot(df,aes(x,colour=team)) +
        stat_ecdf() +
        scale_colour_hue(name="Groups", labels=c("m is 5","m is 20"))
}
# Set default parameters
n <- 30
x <- 0.7
times <- 5000
alphastar <- 22
m1 <- 5
m2 <- 20

#par(mfrow=c(1,2))
#drawbinomial(n,x,5,times,alphastar)
drawtwobinomial(n,x,m1,m2,times,alphastar)
drawtwocdfnomial(n,x,m1,m2,times,alphastar)
