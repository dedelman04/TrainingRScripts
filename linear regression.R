#Linear regression on continuous data
#Most basic ML method
library(HistData)

galton_heights <- GaltonFamilies %>% filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>% rename(son = childHeight)

#Predict son's height Y given father's height X
#start by making training & test sets
library(caret)

#set.seed(1)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p=0.5, list=FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#With no prediction, we would guess the average
avg <- mean(train_set$son)
avg
#R^2 loss
mean((avg-test_set$son)^2)

#If xy follows bivariate normal dist
#Conditional expectation is the regression like y = B0 + B1x
#use lm function to estimate slope (B1) and intercept (B0)
fit <- lm(son ~ father, data = train_set)
fit$coef

#use coefficients to show better R^2 loss
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

######
##predict function
######
#Takes fitted models and takes a new data frame as input
#keeps from having to write out the regression equation as b+ax
y_hat <- predict(fit, test_set)  #fitted model, input df
mean((y_hat - test_set$son)^2)

##################
###Comp Check 1###
###set data
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Partition into test and train sets, then train a linear model & 
#calculate RMSE = SQRT(R^2 loss) 100 times

#####NOTE RMSE <> R^2 loss

set.seed(1)
RMSE_Results <- replicate(100, {
                      #data prep
                          test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
                          train_set <- dat[-test_index,]
                          test_set <- dat[test_index,]
                      #Train & analyze
                         fit <- lm(y ~ x, data = train_set)
                         y_hat <- predict(fit, test_set)
                         sqrt(mean((y_hat-test_set$y)^2))
                         })

mean(RMSE_Results)
sd(RMSE_Results)

####Perform again across multiple size data sets
n <- c(100, 500, 1000, 5000, 10000)
#results <- c("N", "Mean", "SD")

RMSE_function <- function(n, B=100) {
      
      Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
      dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))

      RMSE_Results <- replicate(B, {
          #data prep
          test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
          train_set <- dat[-test_index,]
          test_set <- dat[test_index,]
          #Train & analyze
          fit <- lm(y ~ x, data = train_set)
          y_hat <- predict(fit, test_set)
          sqrt(mean((y_hat-test_set$y)^2))
        })
      c(n, mean(RMSE_Results), sd(RMSE_Results))
  }

set.seed(1)
sapply(n, RMSE_function)

####Perform again with higher correlation between x and y (note params in matrix)
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
RMSE_Results <- replicate(100, {
  #data prep
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  #Train & analyze
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(RMSE_Results)
sd(RMSE_Results)

####Now add another independent variable x2
#Do analysis on x1 only, x2 only, both x1 & x2
#####  ONLY RUN ONE INSTANCE  #####

set.seed(1)
n <- 100
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

###x1
  #data prep
set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  #Train & analyze
  fit <- lm(y ~ x_1, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))

###x2
set.seed(1)
  #data prep
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  #Train & analyze
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))

###both x1 and x2
set.seed(1)
  #data prep
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  #Train & analyze
  fit <- lm(y ~ x_1 + x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))

###Finally, highly correlate x_1 and x_2
###Note the changes again in the matrix

  set.seed(1)
  n <- 1000
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  
  ###x1
  #data prep
  set.seed(1)
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  #Train & analyze
  fit <- lm(y ~ x_1, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  ###x2
  set.seed(1)
  #data prep
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  #Train & analyze
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  ###both x1 and x2
  set.seed(1)
  #data prep
  test_index <- createDataPartition(dat$y, times = 1, p=0.5, list=FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  #Train & analyze
  fit <- lm(y ~ x_1 + x_2, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))