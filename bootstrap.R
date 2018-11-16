#Bootstrapping

#Used when you don't have access to the entire population to determine median
# sample median M estimates population median m
# Monte Carlo simulation shows M is approximately normal

# without access to the entire distribution, used to rely on CLT, but CLT is for mean, not median

#Bootstrap approximates a Monte Carlo simulation on the entire distribution
# We act as if the observed sample is the entire population

data("mnist_27")
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

#How many times do 3, 4, 7 appear in the first re-sample?
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

#How many times does 3 appear across all re-samples?
j <- 0
for (i in 1:10) {
  j <- j + sum(indexes[[i]]==3)
}
j

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