####
#Predict gender based on height
###

data(heights)

#Define outcome (y) and predictors (x's)
y <- heights$sex
x <- heights$height

#exploratory data analysis
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

#caret function createDataPartition creates a randomized vector of indexes
#that can be used to split data into training and test sets
# y --> vector of outcomes
# times --> how many random samples to return
# p --> what proportion of the indexes will be assigned to training set
# list --> do you wnat the outcome as a list or not (vector)

#set.seed() makes the outcome repeatable

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

#split entire data set into training and test sets
train_set <- heights[-test_index, ]  #Rows *not* in test_index, all columns
test_set <- heights[test_index, ]

#simplest LM algorithm - guessing using sample function
#Notice there are *no* references to predictors
y_hat <- sample(c("Male", "Female"), length(test_index), replace=TRUE)

#most ML algorithms require that categorical outcomes are coded as factors
y_hat <- sample(c("Male", "Female"), length(test_index), replace=TRUE) %>%
  factor(levels = levels(test_set$sex))

#Overall Accuracy = Count of Correct / Total Count of Test set
mean(y_hat == test_set$sex)

#First attempt at improving accuracy
#If heights within 2 sd of average male height --> male
#Note that this really means that height > mean - 2sd because it makes
#no sense to have the upper extreme be female
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)  #using y this time since we used x, not test_set

#Run the same through 10 different cutoffs
#Remember to evaluate on training set
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

data.frame(cutoff, accuracy) %>% ggplot(aes(x = cutoff, y = accuracy))+
  geom_point()+geom_line()

#Where is the peak
max(accuracy)  #best accuracy proportion
best_cutoff <- cutoff[which.max(accuracy)]  #height of maximum accuracy
best_cutoff

#Now test the max on the test data set and determine accuracy
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(test_set$sex == y_hat)

#simple accuracy is deceptive
#Confusion Matrix tabulates each combination of predicted and actual value
table(predicted = y_hat, actual = test_set$sex)

#something is off here
#calculate accuracy by sex
test_set %>% mutate(y_hat = y_hat) %>% group_by(sex) %>% summarize(accuracy = mean(y_hat==sex))

#too many females are being predicted as males
#this is due to the high prevalence of males in the data set (77%)
######biased training data leads to a biased ML algorithm######
#this is why accuracy is rarely used

#Confusion Matrix can be used to determine
#sensitivity - ability to detect true positives (predict = 1 when actual = 1)
#and
#specificity - ability to detect true negatives (predict = 0 when actual = 0)

#sensitivity = TP / (TP + FN) = True Positive Rate (TPR) or Recall (Pr(Y^=1|Y=1))
#specificity = TN / (TN + FP) = True Negative Rate (TNR) or 1-FPR (Pr(Y^=0|Y=0))
#            = TP / (TP + FP) = Positive Predictive Value (PPV) or Precision (Pr(Y=1|Y^=1))

#The first factor will be representing Y^=1; e.g. "female" in this example
confusionMatrix(data = y_hat, reference = test_set$sex)

#Balanced Accuracy - harmonic average of specificity and sensitivity (since they are rates)
#   = 1 / (.5 * ( 1/recall + 1/precision)) = 2*(Pre*Rec/(Pre+Rec))
# aka F1 score

#Often either sensitivity (plane safety - don't want false negatives)
#or specificity (capital murder case - don't want false positives) is preferred bc
#of the real world outcomes
#Beta can be used to weight either rate to get a weighted harmonic average
# 1 / ( (beta^2/(1+beta^2))*(1/recall) + (1/(1+beta^2))*(1/precision) )

#F_meas computes the summary with beta defaulting to 1
#Rebuild the guessing algorithm but maximizing F-score instead of accuracy
cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x) {
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))  #F_meas instead of mean
})

data.frame(cutoff, F_1) %>% ggplot(aes(x = cutoff, y = accuracy))+
  geom_point()+geom_line()

max(F_1)  #best accuracy proportion
best_cutoff <- cutoff[which.max(F_1)]  #height of maximum accuracy
best_cutoff

#now check specificity and sensitivity
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)

#changing parameters, e.g. proportion at which guessing occurs, different cutoffs, etc
#lead to different levels of specificity and sensitivity
#to compare two methods across all rates of specificity and sensitivity,
#can use Receiver Operating Characteristic (ROC) curves and Precision-Recall curves

#ROC curve - FPR (1 - Specificity) ~ TPR
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), length(test_index), replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height Cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
       
})

bind_rows(guessing, height_cutoff) %>%
ggplot(aes(x=FPR, y=TPR, color=method))+
  geom_point() + geom_line()#+geom_text(aes(label = cutoffs))


#precision-recall plot - used when prevalence matters
# Pre ~ Rec
guessing <- map_df(probs,
                   function(p){
      y_hat <- sample(c("Male", "Female"), length(test_index), replac= TRUE, prob=c(p, 1-p)) %>%
        factor(levels = c("Female", "Male"))
      list(method="Guess",
           recall = sensitivity(y_hat, test_set$sex),
           precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Female", "Male"))
  list(method = "Height Cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex) )
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color=method))+
  geom_point() + geom_line()#+geom_text(aes(label = cutoffs))

#If we change positives to be Male instead of Female, ROC doesn't change, but P-R does
guessing <- map_df(probs,
                   function(p){
                     y_hat <- sample(c("Male", "Female"), length(test_index), replace= TRUE, prob=c(p, 1-p)) %>%
                       factor(levels = c("Male", "Female"))
                     list(method="Guess",
                          recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
                          precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")) )
                   })

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
    factor(levels = c("Male", "Female"))
  list(method = "Height Cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")) )
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color=method))+ylim(0, 1)+
  geom_point() + geom_line()#+geom_text(aes(label = cutoffs))
