#Case study with 3 classes
#Set up data set for 1, 2 and 7 as classes
if(!exists("mnist")) 
  mnist <- read_mnist()

set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
#Train on 80% of the data
index_train <- createDataPartition(y, p=0.8, list = FALSE)

## get the quandrants
#temporary object to help figure out the quandrants
row_column <- expand.grid(row=1:28, col=1:28) 
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

#binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

#cbind proportion of pixels in upper right quandrant and
##proportion of pixes in lower rigth quandrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x), 
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

#Training data visualized
train_set %>% 
  ggplot(aes(x_1, x_2, color=y)) + 
  geom_point()

#Train a QDA model
train_qda <- train(y ~ .,
                   method = "qda",
                   data = train_set)

#Conditional probablilties (note they must add up to 1)
predict(train_qda, test_set, type = "prob") %>% head()

#Class predictions (default type of QDA is class)
predict(train_qda, test_set)

#Confusion matrix is now 3x3
confusionMatrix(predict(train_qda, test_set), test_set$y)

#Visualize the outcomes
GS <- 150
new_x <- expand.grid(x_1 = seq(min(train_set$x_1), max(train_set$x_1), len=GS),
                     x_2 = seq(min(train_set$x_2), max(train_set$x_2), len=GS))
new_x %>% mutate(y_hat = predict(train_qda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))

#Compare to LDA
train_lda <- train(y ~ .,
                   method = "lda",
                   data = train_set)

confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]

#Note that LDA is much more rigid and inaacurate
new_x %>% mutate(y_hat = predict(train_lda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))

#LDA fails because the categories do not show normal distribution of
#predictors (esp class "1")
train_set %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") 

#KNN has more accuracy than QDA
train_knn <- train(y ~ .,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)

confusionMatrix(predict(train_knn, test_set), test_set$y)$overal["Accuracy"]

#And graphically shows more detail than QDA
new_x %>% mutate(y_hat = predict(train_knn, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))
