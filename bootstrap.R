#Bootstrapping
set.seed(1)
n <- 10^6
income <- 10^(rnorm(n, 4.656786, 0.4394738))
hist(log10(income))

m <- median(income)
m

#Used when you don't have access to the entire population to determine median
# sample median M estimates population median m
set.seed(1)
N <- 250
X <- sample(income, N)
M <- median(X)
M

# Monte Carlo simulation shows M is approximately normal
B <- 10^5
Ms <- replicate(B, {
  X <- sample(income, N)
  M <- median(X)
})
par(mfrow=c(1,2))
hist(Ms)
qqnorm(Ms)
qqline(Ms)
mean(Ms)

# without access to the entire distribution, used to rely on CLT, but CLT is for mean, not median

#Bootstrap approximates a Monte Carlo simulation on the entire distribution
# We act as if the observed sample is the entire population
B <- 10^5
M_stars <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  M_star <- median(X_star)
})

par(mfrow=c(1,1))
qqplot(Ms, M_stars)
abline(0,1) 

quantile(Ms, c(0.05, 0.95))
quantile(M_stars, c(0.05, 0.95))
median(X) + 1.96 * sd(X)/sqrt(N) * c(-1,1)  #CLT quantiles

#CLT means derived from MC Medians
mean(Ms) + 1.96*sd(Ms)*c(-1,1)
mean(M_stars) + 1.96*sd(M_stars)*c(-1,1)

############
#Exercises
############
data("mnist_27")
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

#How many times do 3, 4, 7 appear in the first re-sample?
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

#How many times does 3 appear across all re-samples?
j <- 0
for (i in 1:10) {
  j <- j + sum(indexes[[i]]==3)
}
j

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)


#Run 10,000 Monte Carlo to get EV and SD of the 75th quantile of a NRV
B <- 10000
set.seed(1)
quantiles <- replicate(B, { 
                       y <- rnorm(100, 0, 1)
                       q_star <- quantile(y, 0.75)
                   })

mean(quantiles)
sd(quantiles)

#use 10 bootstrap samples to estimate EV/SD on initial y
set.seed(1)
y <- rnorm(100, 0, 1)
B <- 10

stars <- replicate(B, {
  y_star <- sample(y, replace=TRUE)
  q_star <- quantile(y_star, 0.75)
})

mean(stars)
sd(stars)

set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)