#Bayes' Theorem
#P(Y|x) = P(x)*P(Y=1) / [ P(x|Y=1)*P(Y=1) + P(x|Y=0)*P(Y=0)]
#
#Can estimate P(Y|x) when X is small (no more than 2-3 predictors
#even if many categories)

library(caret)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#Because we know distributions are approx normal, can use Naive Bayes
#to estimate mean/sd of data
params <- train_set %>% 
  group_by(sex) %>% 
  summarize(avg = mean(height), sd = sd(height))
params

#Prevalence = pi = Pr(Y=1)
pi <- train_set %>% 
  summarize(pi=mean(sex=="Female")) %>% 
  .$pi
pi

#Use estimated mean/sd along with prevalence to derive a decision rule
#if pi is % of female, 1-pi is % of male
x <- test_set$height

x <- x %>% group_by()

#dnorm gives density function of normal dist
f0 <- dnorm(x, params$avg[2], params$sd[2])  #Male
f1 <- dnorm(x, params$avg[1], params$sd[1])  #Female

#From Bayes theorem
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

data.frame(x=x, prob=p_hat_bayes) %>% 
  ggplot(aes(x=x, y=prob))+geom_point()

#Prevalence affects sensitivity and specificity
#Our data set has a very small prevalence of females (vs gen pop)
#so the algorithm is affected
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

#Naive Bayes allows us to set pi to overcome a biased data set

pi_ub <- 0.5
p_hat_bayes_unbiased <- f1*pi_ub / (f1*pi_ub + f0*(1-pi_ub)) 
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased> pi_ub, "Female", "Male")

sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

#Shows more intuitive cutoff @ ~ 66-67"
qplot(x, p_hat_bayes_unbiased, geom = "line") + 
  geom_hline(yintercept = 0.5, lty = 2) + 
  geom_vline(xintercept = 67, lty = 2)
