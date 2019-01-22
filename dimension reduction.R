###Dimension Reduction
#General idea is to reduces the number of dimensions (usu 100+) while preserving key characteristics
#such as distance between features or observations
#
#Will focus on Principal component Analysis (PCA)

#Preserving distance
#Simulate bivariate normal data
set.seed(1)
lim <- c(48,78)
library(MASS)
n <- 100
x <- rbind(mvrnorm(n/2, c(69,69), matrix(c(9, 9*0.9, 9*0.92, 9*1),2,2)),
           mvrnorm(n/2, c(55,55), matrix(c(9, 9*0.9, 9*0.92, 9*1),2,2)))

#Plot to see clusters
data.frame(x) %>% ggplot(aes(x=X1, y=X2)) + geom_point()
plot(x)

#Imagine x contains 2 features (heights) and N observations
#Want to reduce dimensionality from 2 to 1
#can presume that one cluster is children, the other is adults
#Challenge: get a one-dimensional summary of predictors from which we approximate distance between
#any two
#Plot showing D(1-2) (blue) and D(2-51) (red)
plot(x, xlim=lim, ylim=lim)
lines(x[c(1,2),], col = "blue", lwd = 2)
lines(x[c(2,51),], col = "red", lwd = 2)
points(x[c(1,2,51),], pch = 16)

#Use dist function to compute distances
d <- dist(x)
as.matrix(d)[1,2]
as.matrix(d)[2,51]
#But this is a two-dimensional distance and we want one-dimensional
#Computer distance with just the 1st dimension and plot differences
z <- x[,1]
plot(dist(x), dist(z))
abline(0,1, col="red")
#Looks the same if we use only 2nd dimension
z <- x[,2]
plot(dist(x), dist(z))
abline(0,1, col="red")

#In general this is an underesimation because we are only using positive differences
#Instead use an average like sqrt[(1/2)*sum(Xij - Xij)^2] and underestimation goes away
plot(dist(x), dist(z)*sqrt(2))
abline(0,1, col="red")

#Std deviation is small
sd(dist(x) - dist(z)*sqrt(2))

#Now compute distance differences between two predictors vs average of predictors
z <- cbind((x[,2]+x[,1])/2, x[,2] - x[,1])
plot(z, xlim=lim, ylim=lim-mean(lim))
lines(z[c(1,2),], col = "blue", lwd = 2)
lines(z[c(2,51),], col = "red", lwd = 2)
points(z[c(1,2,51),], pch = 16)
#This shows that 2nd predictor in this new matrix can be ignored w/o losing much information
#(the flatter the line, less info lost)

#plot only 1st dimension
plot(dist(x), dist(z[,1]*sqrt(2)))
abline(0,1)

#std deviation is even smaller
sd(dist(x) - dist(z[,1])*sqrt(2))

