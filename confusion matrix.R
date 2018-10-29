library(dslabs)
library(dplyr)
library(lubridate)
data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


#Proportion of females in class and online
dat %>% group_by(sex, type) %>% count()

#use type as variable to predict sex
#hint - use what we know about proportions to determine best approach
y_hat <- ifelse(x == "inclass", "Female", "Male") %>% factor(levels = levels(y))
mean(y_hat == y)

#write code with table to show confusion matrix of above
table(y_hat, y)

#Find statistics about the confusion matrix
confusionMatrix(y_hat, y)


