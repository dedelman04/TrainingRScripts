#k-nearest neighbors
data("mnist_27")

#knn similar to bin smoothing, but more easily adapted to mulitple dimensions
#  define distances between observations based on features
#   Define the "k" nearest points and take an average of these points - "neighborhood"
#   Give us estimated conditional probability (bin smoothing gives estimated trend)
#Large K gives smaller estimates & smooth, small K larger estimates & wiggly

#Compare to logistic regression

#LR
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#KNN
#knn3 function knn3(formula, data_frame)
# "formula" uses regular tilde notation (y ~ x_1 + x_2 + x_3...)
#   or use . for all predictors (y ~ .)
knn_fit <- knn3(y ~ ., data = mnist_27$train)

#Can also call knn3(x, y) where x is matrix of predictors and y is vector of outcomes
knn_fit_2 <- knn3(as.matrix(mnist_27$train[,2:3]), y = mnist_27$train$y)
# Matrix version is more useful when data sets are large (many predictors with some not useful)

#knn3 has a parameter k that defines the number of neighbors to use (default 5)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

#predict maximizes probablility for each class (2, 7)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#Note that the model may be overtrained - accuracy is higher against training set than test set
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

#Overtraining is worst when k = 1
# (you are your closest neighbor; each point is used to predict itself)
knn_fit_1 <- knn3(y~., data=mnist_27$train, k=1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn_1, reference = mnist_27$train$y)$overall["Accuracy"]
#but accuracy is worse on test set (even worse than logistic regression)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn_1, reference = mnist_27$test$y)$overall["Accuracy"]

#larger ks should reduce over-training
knn_fit_401 <- knn3(y~., data=mnist_27$train, k=401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn_401, reference = mnist_27$test$y)$overall["Accuracy"]
# this k is too large to allow for flexibility - very close to logistic regression
### oversmoothing ###

#repeat for many different values of k and see where the inflection point is
ks <- seq(3, 251, 2)  #all odd numbers 3 <= x <= 251

#run against both training and test sets just to show the differences
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  
  train_error <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)$overall["Accuracy"]

  y_hat <- predict(fit, mnist_27$test, type = "class")
  
  test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
  
  list(train = train_error, test = test_error)
})

#plot accuracy vs k
data.frame(ks, test=accuracy$test, train=accuracy$train) %>%
  ggplot(aes(x = ks, y = test, color="green"))+geom_point()+geom_line()+
  geom_point(aes(x = ks, y = train, color = "red"))+geom_line(aes(x = ks, y = train, color = "red"))

data.frame(k=ks, train = accuracy$train, test = accuracy$test) %>% 
  gather(type, accuracy, "train":"test") %>%
  ggplot(aes(x=k, y=accuracy, color = type))+geom_point()+geom_line()

######Exercise 1

#Balanced Accuracy - harmonic average of specificity and sensitivity (since they are rates)
#   = 1 / (.5 * ( 1/recall + 1/precision)) = 2*(Pre*Rec/(Pre+Rec))
# aka F1 score

#Predict sex based on height using knn
#Get F1 = Balanced Accuracy from confusion Matrix

data("heights")
y <- heights$height  ####THIS IS WHERE I SCREWED UP - should be SEX
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)
train_set <- heights[test_index,] # %>% slice(test_index)
test_set <- heights[-test_index,] #%>% slice(-test_index)

train_set <- train_set %>% mutate(y = as.numeric(sex=="Female"))
test_set <- test_set %>% mutate(y = as.numeric(sex=="Female"))

ks <- seq(1, 101, 1)  #all numbers 1 <= x <= 101

#accuracy <- map_df(ks, function(k){
#  fit <- knn3(y ~ height, data = train_set, k = k)
#  
#  p_hat <- predict(fit, test_set, type = "prob")
#  
#  y_hat <- ifelse(p_hat[,1] > 0.5, "Female", "Male") %>% factor()
#
#  recall <- confusionMatrix(data = y_hat, reference = test_set$sex)$byClass["Recall"][[1]]
#  precision <- confusionMatrix(data = y_hat, reference = test_set$sex)$byClass["Precision"][[1]]
#  F_1 <- 2*((precision*recall)/(precision+recall))
#  
#  list(F_1 = F_1)
#})

#data.frame(ks, accuracy) %>%
#  ggplot(aes(x = ks, y = accuracy))+geom_point()+geom_line()

F_1 <- map_dbl(ks, function(x) {
  fit <- knn3(y ~ height, data = train_set, k=x)
  p_hat <- predict(fit, test_set, type = "prob")
    
  y_hat <- ifelse(p_hat[,1] > 0.5, "Female", "Male") %>% factor(levels = levels(test_set$sex))
  
  
  #  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
  #  factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(test_set$sex))  #F_meas instead of mean
})

data.frame(ks, F_1) %>% ggplot(aes(x = ks, y = F_1))+
  geom_point()+geom_line()

max(F_1)  #best accuracy proportion
best_cutoff <- ks[which.max(F_1)]  #height of maximum accuracy
best_cutoff

####Official answer
set.seed(1)
data("heights")
library(caret)
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
  test_set <- heights[test_index, ]
  train_set <- heights[-test_index, ]
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)


######Exercise 2
##Report accruacy on tissue_gene_expression set for k = 1:11 odd
library(dslabs)
data("tissue_gene_expression")

ks <- seq(1, 11, 2)
pred_matrix <- tissue_gene_expression$x
class_vec <- as.factor(tissue_gene_expression$y)
set.seed(1)

test_index <- createDataPartition(class_vec, times = 1, p = 0.5, list = FALSE)
train_pred <- pred_matrix[test_index, ]
train_class <- class_vec[test_index]
test_pred <- pred_matrix[-test_index, ]
test_class <- class_vec[-test_index]

acc <- sapply(ks, function(k){
  fit <- knn3(x = train_pred, y = train_class, k = k)
  y_hat <- predict(fit, test_pred, type = "class") %>% factor(levels = levels(train_class))
  confusionMatrix(data = y_hat, reference = test_class)$overall["Accuracy"]
})

data.frame(ks, acc)
