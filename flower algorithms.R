#Data load/prep
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Split data
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) # line of code
test <- iris[test_index,]
train <- iris[-test_index,]

#Find best single feature for greatest accuracy
#Look at summary statistics
train %>% group_by(Species) %>% summarize(SepLM = mean(Sepal.Length), SepLSD = sd(Sepal.Length),
  SepWM = mean(Sepal.Width), SepWSD = sd(Sepal.Width),
  PetLM = mean(Petal.Length), PetLSD = sd(Petal.Length),
  PetLM = mean(Petal.Width), PetWSD = sd(Petal.Width))

#Plot histograms
train %>% ggplot(aes(x=Sepal.Length))+geom_histogram(bins=10)+facet_wrap(~Species)
train %>% ggplot(aes(x=Sepal.Width))+geom_histogram(bins=10)+facet_wrap(~Species)
train %>% ggplot(aes(x=Petal.Length))+geom_histogram(bins=10)+facet_wrap(~Species)
train %>% ggplot(aes(x=Petal.Width))+geom_histogram(bins=10)+facet_wrap(~Species)

#Get ranges
RangeSL <- c(min(train$Sepal.Length), max(train$Sepal.Length))
RangeSW <- c(min(train$Sepal.Width), max(train$Sepal.Width))
RangePL <- c(min(train$Petal.Length), max(train$Petal.Length))
RangePW <- c(min(train$Petal.Width), max(train$Petal.Width))

#####
#Build the algorithms
#####
SLCO <- seq(round(RangeSL[1], 0), round(RangeSL[2], 0), 0.2 )

SLAcc <- map_dbl(SLCO, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

data.frame(SLCO, SLAcc) %>% ggplot(aes(SLCO, SLAcc))+geom_point()+geom_line()+geom_text(label=SLCO)
SLResults <- c("Sepal.Length", SLCO[which.max(SLAcc)], max(SLAcc))

SWCO <- seq(round(RangeSW[1], 0), round(RangeSW[2], 0), 0.2 )

SWAcc <- map_dbl(SWCO, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

data.frame(SWCO, SWAcc) %>% ggplot(aes(SWCO, SWAcc))+geom_point()+geom_line()+geom_text(label=SWCO)
SWResults <- c("Sepal.Width", SWCO[which.max(SWAcc)], max(SWAcc))

#PLCO <- seq(round(RangePL[1], 0), round(RangePL[2], 0), 0.2 )
PLCO <- round(seq(round(RangePL[1], 0), round(RangePL[2], 0), length.out = 10 ), 1)

PLAcc <- map_dbl(PLCO, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

data.frame(PLCO, PLAcc) %>% ggplot(aes(PLCO, PLAcc))+geom_point()+geom_line()+geom_text(label=PLCO)
PLResults <- c("Petal.Length", PLCO[which.max(PLAcc)], max(PLAcc))

#PWCO <- seq(round(RangePW[1], 0), round(RangePW[2], 0), 0.2 )
PWCO <- round(seq(round(RangePW[1], 0), round(RangePW[2], 0), length.out = 10 ), 1)

PWAcc <- map_dbl(PWCO, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

data.frame(PWCO, PWAcc) %>% ggplot(aes(PWCO, PWAcc))+geom_point()+geom_line()+geom_text(label=PWCO)
PWResults <- c("Petal.Width", PWCO[which.max(PWAcc)], max(PWAcc))

train_results <- data.frame(SLResults, SWResults, PLResults, PWResults)

#Compare to test set
y_hat_test <- ifelse(test$Petal.Length > PLCO[which.max(PLAcc)], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat_test == test$Species)

#####
#Repeat using test set for training to determine if overfit
#####
RangeSL <- c(min(test$Sepal.Length), max(test$Sepal.Length))
RangeSW <- c(min(test$Sepal.Width), max(test$Sepal.Width))
RangePL <- c(min(test$Petal.Length), max(test$Petal.Length))
RangePW <- c(min(test$Petal.Width), max(test$Petal.Width))

SLCO <- seq(round(RangeSL[1], 0), round(RangeSL[2], 0), 0.2 )

SLAcc <- map_dbl(SLCO, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(SLCO, SLAcc) %>% ggplot(aes(SLCO, SLAcc))+geom_point()+geom_line()+geom_text(label=SLCO)
SLResults <- c("Sepal.Length", SLCO[which.max(SLAcc)], max(SLAcc))

SWCO <- seq(round(RangeSW[1], 0), round(RangeSW[2], 0), 0.2 )

SWAcc <- map_dbl(SWCO, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(SWCO, SWAcc) %>% ggplot(aes(SWCO, SWAcc))+geom_point()+geom_line()+geom_text(label=SWCO)
SWResults <- c("Sepal.Width", SWCO[which.max(SWAcc)], max(SWAcc))

PLCO <- seq(round(RangePL[1], 0), round(RangePL[2], 0), 0.2 )

PLAcc <- map_dbl(PLCO, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(PLCO, PLAcc) %>% ggplot(aes(PLCO, PLAcc))+geom_point()+geom_line()+geom_text(label=PLCO)
PLResults <- c("Petal.Length", PLCO[which.max(PLAcc)], max(PLAcc))

PWCO <- seq(round(RangePW[1], 0), round(RangePW[2], 0), 0.2 )

PWAcc <- map_dbl(PWCO, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(PWCO, PWAcc) %>% ggplot(aes(PWCO, PWAcc))+geom_point()+geom_line()+geom_text(label=PWCO)
PWResults <- c("Petal.Width", PWCO[which.max(PWAcc)], max(PWAcc))

test_results <- data.frame(SLResults, SWResults, PLResults, PWResults)

#####
#Now combine Petal.Length and Petal.Width in algorithm
#For simplicity, say that if both features are gt respective cutoff then virginica
#####

y_hat_2F <- ifelse(train$Petal.Length > 4.8 & train$Petal.Width > 1.6, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))
mean(y_hat_2F == train$Species)

