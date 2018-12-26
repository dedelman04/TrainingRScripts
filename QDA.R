#Quadratic Disrciminant Analysis - a version of Naive Bayes
# where the distributions are assumed to be *multivariate* normal
#Height example from before was actually QDA

#Better example from MNIST data set
data("mnist_27")

#Since 2 predictors - assume each is bivariate normal

params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), 
            sd_1= sd(x_1), sd_2 = sd(x_2), 
            r = cor(x_1,x_2))
params

#Scatter plot the data and draw ellipses that contain 95% of the points that
#map to the category

mnist_27$train %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm", lwd = 1.5)

#Train an algorithm and check for accuracy
library(caret)
train_qda <- train(y ~ ., 
                   method = "qda",
                   data = mnist_27$train)

y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#Compare to true probability
plot_cond_prob()
plot_cond_prob(predict(train_qda, mnist_27$true_p, type = "prob")[,2])

#QDA does not work as well as kernel methods, probably due to normality
#not being true (note slight curvature in 7)

mnist_27$train %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") +
  facet_wrap(~y)

#QDA starts to be computationally difficult as predictors increase
# num of correlations = K*P(P-1)/2 for P predictors, K classes
# leads to overfitting

