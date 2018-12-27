#Classification trees (categorical outcomes)
# Like regression trees, but value at the end of the node comes not from average, but
# from class that has majority vote within the region,
#  i.e. class that appears most in the region 
#
# Also cannot use RSS error, since the outcome is not a number
# More advanced metrics are needed (P is proportion of a class in a node)
#  - Gini Index  sum of P*(1-P) over all observations in a node
#  - Entropy  neg sum of P * log(P) over all observations in a node
# As both approach 0, the "purity" is improved and hence accruacy

#Train a classification tree for the 2-7 digits
data("mnist_27")
train_rpart <- train(y ~ ., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, len = 25)),
                     data = mnist_27$train)
ggplot(train_rpart)

#Note cp for max accuracy
#Accuracy is better than regression, but still not up to kernel method
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#Conditional probability plot emphasizes the limitation of trees
# (can't have smooth boundary)
plot_cond_prob(predict(train_rpart, mnist_27$true_p, type = "prob")[,2])
