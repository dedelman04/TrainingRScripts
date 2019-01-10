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

##Train a knn model across 3, 5, 7 neighbors, using k-fold cross-validation (k=10)
control <- trainControl(method="cv", number=10, p=.9)
train_knn <- train(x[, col_index], y, 
                   method = "knn",
                   tuneGrid = data.frame(k=c(3,5,7)),
                   trControl = control)
ggplot(train_knn)


#Method for training against smaller subsets
n <- 1000
b <- 2  #Slowly increase n and b 
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
