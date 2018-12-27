#Random Forests
# Solves some shortcomings of decision trees by averaging many trees together
#
# Essentially bootstrap the training set, get a prediction via trees over 
#  each bootstrap sample, and average/majority vote over all trees
data("polls_2008")
library(randomForest)
fit <- randomForest(margin~., data = polls_2008)
plot(fit)

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

train_rf <- randomForest(y ~ ., data=mnist_27$train)

confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob(predict(train_rf, mnist_27$true_p, type = "prob")[,2])

#caret will allow for training of parameters in forests
fit <- train(y ~ .,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 2, minNode = seq(3, 50)),
             data = mnist_27$train)
confusionMatrix(predict(fit, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob(predict(fit, mnist_27$true_p, type = "prob")[,2])
