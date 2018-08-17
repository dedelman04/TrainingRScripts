B <- 10000
#n <- 50

compute_prob <- function(n, B=10000) {
  same_bday <- replicate(B, 
                         {bdays <- sample(1:365, n, replace=TRUE)
                          any(duplicated(bdays))
                          })
  mean(same_bday)
}
#results <- replicate(B, same_bday(n))
#mean(results)
n <- seq(1, 60)

prob <- sapply(n, compute_prob)

plot(n, prob)

ex_prob <- function(n) {
  prob <- seq(365, 365-n+1)/365
  1-prod(prob)
}

eprob <- sapply(n, ex_prob)

plot(n, prob)
lines(n, eprob, col="red")

B <- round(10^seq(1, 5, len=100),0)
n <- 22
compute_prob <- function(B, n=22) {
  same_bday <- replicate(B, 
                         {bdays <- sample(1:365, n, replace=TRUE)
                         any(duplicated(bdays))
                         })
  mean(same_bday)
}
#head(B) tail(B)
#length(B)

prob <- sapply(B, compute_prob)
plot(log10(B), prob)

#histogram, qq of Monte Carlo
B <- 10000
N <- 1000
p <- .45
X_hat <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob=c(1-p, p))
  mean(X)
})

library(gridExtra)
p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat)) + geom_histogram(binwidth = 0.005)
p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat))+
      stat_qq(dparams = list(mean=mean(X_hat), sd=sd(X_hat)))+
      geom_abline()+ylab("X_hat")+xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

sd(X_hat)
mean(X_hat)