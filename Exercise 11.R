##11.1

par(mfrow=c(2,1))
ozone.lm <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data=airquality[airquality>1,])

hist(ozone.lm$residuals, probability = TRUE)
qqnorm(ozone.lm$residuals)
lines((-3):3, ((-3):3)*sd(ozone.lm$res), type="l", col="red")

##11.2

my.analysis <- lm(log(Ozone) ~ 
	Solar.R + Wind + Temp + 
	Solar.R:Wind + Solar.R:Temp + Wind:Temp, data=airquality[airquality>1,])

drop1(my.analysis, test="F")

my.analysis <- update(my.analysis, ~. -Solar.R:Wind)

drop1(my.analysis, test="F")

my.analysis <- update(my.analysis, ~. -Wind:Temp)

drop1(my.analysis, test="F")

ozone.lm.WST <- lm(log(Ozone) ~ Wind + Solar.R:Temp, data=airquality[airquality>1,])

hist(ozone.lm.WST, probability=TRUE)
qqnorm(ozone.lm.WST$residuals)

##11.3

crab.data <-data.frame(satellite=1*(crabs$Satellites>0),width=crabs$Width)

crab.analysis <-glm(satellite~width, family=binomial, data=crab.data)

my.linear.predictor <- data.frame(prediction=predict(crab.analysis, se.fit=TRUE)$fit,
				lower=predict(crab.analysis, se.fit=TRUE)$fit - 1.96*predict(my.analysis,se.fit=TRUE)$se.fit,
				upper=predict(crab.analysis, se.fit=TRUE)$fit + 1.96*predict(my.analysis,se.fit=TRUE)$se.fit)

my.linear.predictor <- my.linear.predictor[order(crab.data$width),]

logistic <-function(x) {exp(x)/(1+exp(x)) }
my.predictor <- logistic(my.linear.predictor)

plot(sort(crab.data$width), my.predictor$prediction, type="l", xlab='width', ylab='p(satellite)' )
lines(sort(crab.data$width),my.predictor$upper, type="l", lty=2)
lines(sort(crab.data$width),my.predictor$lower, type="l", lty=2)
