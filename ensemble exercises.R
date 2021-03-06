#Train 23 models thru caret package
#Q1 prep models
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")


set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Q2 make predictions against all models; what are dimensions
preds <- sapply(fits, function(fit){predict(fit, newdata=mnist_27$test)})
dim(preds)

#Q3 compute accuracy for all models, give the mean
preds <- preds %>% as.data.frame()
sapply(models, function(model){preds[,model] %>% factor()})
acc <- sapply(models, 
              function(model){
                confusionMatrix(data = preds[, model],
                                reference = mnist_27$test$y)$overall["Accuracy"]
})

mean(acc)

###Official code (got exact same answer)
acc <- colMeans(preds == mnist_27$test$y)
mean(acc)

#Q4 Build an ensemble by majority vote across all models, compute accuracy
maj_vote <- ifelse(rowSums(preds=="2") >= 12, "2", "7") %>% factor()

e_acc <- confusionMatrix(data = maj_vote, reference = mnist_27$test$y)$overall["Accuracy"]

###Official code
votes <- rowMeans(preds == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5 How many and which models are better than the ensemble
acc[ acc > e_acc]

#Official code, but something is still jacked up
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

#Q6 cross validate all of the models and get new accuracy figures
control <- trainControl(method="cv", number=10, p=.9)
acc_cv <- lapply(models, function(model){ 
  print(model)
  fit <- train(y ~ ., method = model, data = mnist_27$train, trControl=control)
  fit$results$Accuracy
}) 

acc_means <- sapply(seq(1:23), function(i){min(acc_cv[[i]])})
mean(acc_means)

##Official code
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#Q7 ensemble with accuracy > 0.8 only
pred_acc <- preds[, which(acc >= 0.8)]

maj_vote <- ifelse(rowSums(pred_acc=="2") >= 12, "2", "7") %>% factor()

e_acc <- confusionMatrix(data = maj_vote, reference = mnist_27$test$y)$overall["Accuracy"]

###official code
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
