#linear regression can have values < 0 or > 1 <-- Probabilities are always 0<p<1
#logistic regression assures that the probability
#remains between 0 and 1
#
# g(p) = log(p/(1-p))
#
#convert probabilities to log of odds
#
#Cannot use R^2 loss or RMSE - use instead Maximum Likelihood Estimate
#Use GLM function (instead of LM) <- Generalized Linear Models

glm_fit <- train_set %>% mutate(y = as.numeric(sex=="Female")) %>%
  glm(y ~ height, data=., family = "binomial")

#use predict.glm
#must use type = "response" for conditional probabilities (default is logistic transform values)
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

#confusion matrix for logistic regression
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat_logit, test_set$sex)

