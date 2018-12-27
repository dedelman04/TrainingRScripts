#Regression Trees - used for continuous outcomes
data("polls_2008")
qplot(day, margin, data = polls_2008)

#Partition the data space into J non-overlapping regions R1...Rj
# For each observation with Rj, predict Y-hat as being the average (mean) of
# all training observations within Rj

#Partitions are made recursively: After using predictor j, need to determine
#the next predictor and where to make the next partition
#
#Predictor j and value s define 2 partitions R1 and R2
# We use the algorithm to split the data
#  R1(j, s) = {X | xj < s}
#  R2(j, s) = {X | xj >= s}
#
#Each set then has an average y-bar that will server as a predictor for that set
#
#We want to minimize Residual Sum of Squares error across all j, s

# function rpart does this
fit <- rpart(margin ~ ., data = polls_2008)

plot(fit, margin=0.1)
text(fit, cex=.75)

#Superimpose f-hat(x) on the data
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#Why does algorithm stop at 8 partitions?
# If left unchecked, each observation will be its own partition and RSS
# will be 0.  But the model will be overtrained.
# A parameter called "complexity parameter" (cp) states that the RSS
# must improve by a factor of cp before a new partition is created
# Other parameters include 
# - minimum number of observations to be partitioned
#     (rpart "minsplit", default = 20)
# - minimum number of observations in each partition
#     (rpart "minbucket", default = round(minsplit/3))

#Set minsplit = 2 and minbucket = 0
fit2 <- rpart(margin ~ ., data = polls_2008, 
              control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit2)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
#This basically maps just our original data = 0 error but overtrained

#rpart allows us to grow a large tree then prune branches below a certain
#threshold
pruned_fit <- prune(fit2, cp=0.01)
polls_2008 %>% 
  mutate(y_hat = predict(pruned_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

#Optimal CP should be cross-validated like any other parameter
#train over 0 <= CP <= 0.05 with 25 intervals 
library(caret)
train_rpart <- train(margin ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)

#Show the optimized tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

#Plot F-hat(x) since there is only one predictor
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")