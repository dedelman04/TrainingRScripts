#Linear Discriminant Analysis
# Assume correlation structures for all classes are the same
# Therefore only need one set of SD and R (take mean across all classes)

params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1= sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))

params <-params %>% mutate(sd_1 = mean(sd_1), sd_2=mean(sd_2), r=mean(r))
params 

#Note how distros are now equivalent (shape and direction of ellipse)
tmp <- lapply(1:2, function(i){
  with(params[i,], MASS::mvrnorm(1000, mu = c(avg_1, avg_2), Sigma = matrix(c(sd_1^2, sd_1*sd_2*r, sd_1*sd_2*r, sd_2^2), 2, 2))) %>%
    as.data.frame() %>% 
    setNames(c("x_1", "x_2")) %>% 
    mutate(y  = factor(c(2,7)[i]))
})
tmp <- do.call(rbind, tmp)
mnist_27$train %>% mutate(y = factor(y)) %>% 
  ggplot() + 
  geom_point(aes(x_1, x_2, color=y), show.legend = FALSE) + 
  stat_ellipse(aes(x_1, x_2, color = y), data = tmp, type="norm", lwd = 1.5)

#The decision boundary is then a line
train_lda <- train(y ~ ., 
                   method = "lda",
                   data = mnist_27$train)
plot_cond_prob(predict(train_lda, mnist_27$true_p, type = "prob")[,2])

#This method is more inflexible but we can still fit a model and check accuracy
train_lda <- train(y ~ .,
                   method = "lda",
                   data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
