###Section 1
#data prep
###

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#Prob of test positive
#P(pos) = P(dis)*P(pos|dis) + P(dis')*P(pos|dis')

p_dis <- mean(disease==1) #sum(disease==1)/length(disease)
p_not_dis <- mean(disease==0) #sum(disease==0)/length(disease)

p_pos_dis <- .85
p_pos_not_dis <- .1

p_pos <- p_dis*p_pos_dis + p_not_dis*p_pos_not_dis

#Prob of dis given neg
#P(dis|neg) = P(neg|dis)*(P(dis)/P(neg))
#P(neg) = 1 - P(pos)

p_neg <- 1 - p_pos
p_neg_dis <- 1 - p_pos_dis

p_dis_neg <- p_neg_dis*(p_dis/p_neg)

#Prob of dis given pos
#P(dis|pos) = P(pos|dis)*(P(dis)/P(pos))

p_dis_pos = p_pos_dis*(p_dis/p_pos)

#Relative Risk of having disease
#RR = P(dis|pos) / P(dis)

rr <- p_dis_pos / p_dis

###
###Section 2
###

#plot x vs P(Male|height = x)
library(dslabs)
data("heights")
heights %>% mutate(height = round(height)) %>% group_by(height) %>% 
                     summarize(p = mean(sex == "Male")) %>%  #MISSING CODE
qplot(height, p, data =.)

#cut by quantiles instead of each rounded height
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>% #MISSING CODE
group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#Generate bivariate normal
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

#Estimate conditional expectations and plot
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%#MISSING CODE	
qplot(x, y, data =.)
