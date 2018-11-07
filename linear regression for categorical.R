##regression for categorical variables
#predicting male/female based on heights

library(dslabs)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Y=1 --> Female; Y=0 --> Male
# Pr(Y = 1/0 | X = x) <- conditional probability of 1 or 0 given height x

#Pr(Female | x = 66)
train_set %>% filter(round(height)==66) %>%
  summarize(mean(sex=="Female"))

#look at all heights 60-76"
hts <- c(60:76)
avgHt <- function(x){
  train_set %>% filter(round(height)==x) %>% summarize(cp = mean(sex=="Female")) %>%.$cp
}
y_hat <- sapply(hts, avgHt)

data.frame(hts, y_hat) %>% ggplot(aes(x = hts, y=y_hat)) + geom_point()

#Set factors to 0/1, estimate b0 and b1
lm_fit <- mutate(train_set, y=as.numeric(sex=="Female")) %>%
  lm(y ~ height, data = .)


heights %>% mutate(x=round(height)) %>% group_by(x) %>%
  filter(n() >= 10) %>% summarize(prop = mean(sex=="Female")) %>%
  ggplot(aes(x, prop)) + geom_point() + 
    geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

#define a decision rule, in our case p(x) > 0.5 = female

#use confusion matrix
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)
