##Case Study - MNIST data set
mnist <- read_mnist()

#basic exploration
names(mnist)
dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)

#Create a 10,000 record train, 1000 test subset of the data to account for small processing power
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index, ]
y <- factor(mnist$train$labels[index])

#Not sure if this is supposed to be $test or $train
index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index, ]
y_test <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test_test <- mnist$test$images[index, ]
y_test_test <- factor(mnist$test$labels[index])

##
#Perform some pre-processing
##

#look for pixels that have very little to zero variability (empty space on the image)
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256)

#remove "near zero variance" predictors
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))  #yellow shows pixels being removed

#set column index for pixels to be *kept*
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#add column names, as required by caret
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)
colnames(x_test_test) <- colnames(mnist$test$images)


##Train a knn model across 3, 5, 7 neighbors, using k-fold cross-validation (k=10)
control <- trainControl(method="cv", number=10, p=.9)
train_knn <- train(x[, col_index], y, 
                   method = "knn",
                   tuneGrid = data.frame(k=c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)  #Note accuracy peaks at 3 neighbors


#Method for training against smaller subsets
n <- 1000
b <- 2  #Slowly increase n and b 
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

#fit the entire dataset to the best knn model (k=3)
fit_knn <- knn3(x[, col_index], y, k=3)

#predict and show accuracy
y_hat <- predict(fit_knn, x_test[, col_index], type="class")
cm <- confusionMatrix(y_hat, factor(y_test))
cm$overall["Accuracy"]

#Show sensitivity and specificity
cm$byClass[,1:2]

###Predict against test set
y_hat_test <- predict(fit_knn, x_test_test[, col_index], type="class")
cm_test <- confusionMatrix(y_hat_test, factor(y_test_test))
cm_test$overall["Accuracy"]
cm_test$byClass[,1:2]


##Train and fit random forest with Rborist
#use 5-fold cross validation, CV across 2 minimum node values and 5 random selection values
#use 50 trees for all runs (nTree); random subset of 5000 for each run (nSamp)
library(Rborist)
control <- trainControl(method = "cv", number = 5, p= 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10,15,25,35,50))

set.seed(123)
train_rf <- train(x[, col_index],
                  y,
                  method="Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)

ggplot(train_rf)
train_rf$bestTune

#rm(mnist, x_test_test)

#Optimize final tree structure
fit_rf <- Rborist(x[, col_index], y,
  ntree = 1000,
  minNode = train_rf$bestTune$minNode,
  predFixed = train_rf$bestTune$predFixed)

#Predict and look at confusion matrix
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[, col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

#Show images and their predicted values
par(mfrow=c(3,4))
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}
par(mfrow=c(1,1))
#Variable importance
#Rborist does not support it; use randomForest instead, with ALL predictors
rf <- randomForest(x,y, ntree=50)

imp <- importance(rf)

#plot importance as an image
image(matrix(imp, 28, 28))

#compare knn with random forest to visualize where we predicted incorrectly
p_max <- predict(fit_knn, x_test[, col_index])
p_max <- apply(p_max, 1, max)
ind <- which(y_hat != y_test)
ind <- ind[order(p_max[ind], decreasing=TRUE)]

par(mfrow=c(3,4))
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat[i],")=",p_max[i]," is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

#and same for random forest
p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",p_max[i]," is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

par(mfrow=c(1,1))

#Ensembles combine results from different algorithms to provide a stronger result
#We will average the results of KNN and RF
p_rf <- predict(fit_rf, x_test[, col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[, col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)
