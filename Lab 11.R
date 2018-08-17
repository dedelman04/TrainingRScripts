##1
wine.lm <- lm(price ~ temp + year + h.rain + w.rain + h.rain:w.rain, data=wine.df)
summary(wine.lm)
##2
drop1(wine.lm, test="F")

##3
coef.vec <- coef(wine.lm)
regr.coeff.800 <- coef.vec[4]+800*coef.vec[6]
regr.coeff.800

##4
wine.1985 <- data.frame(year=1985,
				temp=mean(wine.df$temp),
				h.rain=mean(wine.df$h.rain),
				w.rain=mean(wine.df$w.rain))

predict(wine.lm, wine.1985)

##5
wine.log.lm <- lm(log(price) ~ temp + year + h.rain + w.rain + h.rain:w.rain, data=wine.df)
summary(wine.log.lm)
##6
drop1(wine.log.lm, test="F")

##7
wine.log.lm <- update(wine.log.lm, ~. -h.rain:w.rain)
summary(wine.log.lm)

##8
wine.1985.log <- data.frame(year=1985,
				temp=mean(wine.df$temp),
				h.rain=mean(wine.df$h.rain),
				w.rain=mean(wine.df$w.rain))

exp(predict(wine.log.lm, wine.1985.log))


