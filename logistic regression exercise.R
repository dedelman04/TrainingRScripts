#logistic regression exercise

set.seed(2)
make_data <- function(mu_1){
  n <- 1000
  p <- 0.5
  mu_0 <- 0
  sigma_0 <- 1
  sigma_1 <- 1
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

#Generate 25 datasets changing the difference between the two classes
#using delta <- seq(0,3, len=25) and plot accuracy vs mu_1

delta <- seq(0,3, len=25)

#seq_data <- data.frame(sapply(delta, make_data), ncol=2)

#i <- 1
mu_1 <- function(x) { #for (i in 1:length(delta)) {
  dat <- make_data(x)
  
  glm_fit <- dat$train %>% glm(y ~ x, data=., family = "binomial")
  
  #must use type = "response" for conditional probabilities (default is logistic transform values)
  p_hat_logit <- predict(glm_fit, newdata = dat$test, type = "response")

  #Accuracy
  y_hat_logit <- ifelse(p_hat_logit < 0.5, 0, 1) %>% factor(levels = levels(dat$test$y))
  mean((y_hat_logit == dat$test$y)^2)
}

acc <- sapply(delta, mu_1)

data.frame(delta, acc) %>% ggplot(aes(x=delta, y=acc))+geom_point()

