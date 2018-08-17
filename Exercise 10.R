##10-1
par(mfrow=c(3,3))

set.seed(779)
for (i in 1:9) {
	hist(rnorm(100), probability=TRUE, main=paste("Histogram",i))
	curve(dnorm, add=TRUE, col="red", lwd=3)
}

##10-2
my.ozone <- airquality$Ozone[!is.na(airquality$Ozone) & airquality$Ozone>1]
mean.1 <-mean(my.ozone)
sd.1 <-sd(my.ozone)

my.ozone.sim <- rnorm(length(my.ozone), mean=mean.1, sd=sd.1)

qqplot(my.ozone.sim, my.ozone)
lines(0:max(my.ozone)+10,
	0:max(my.ozone)+10)

mean.2 <- mean(log(my.ozone))
sd.2 <- sd(log(my.ozone))

my.ozone.log.sim <- rnorm(length(my.ozone), mean.2, sd.2)
qqplot(exp(my.ozone.log.sim), my.ozone)
lines(0:max(my.ozone)+1,
	0:max(my.ozone)+1)

##10-3
one.roll <- function() {
	x <- sum(sample(1:6,2,replace=TRUE))
	y <- sum(sample(1:6,x,replace=TRUE))
	y
}

set.seed(457778)
y.values <-replicate(1000,one.roll())
hist(y.values)
