#Data Setup
#1000 schools with random normal school sizes
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

#quality measure per school, based on random T-distribution with 5 degrees of freedom
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

#Test score means with the tests being random normal across all students 
#with mean of quality and SD of 30
set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1 - What are the top schools based on average score?
schools %>% top_n(25, score) %>% arrange(desc(score)) %>% select(id, size, score)
#report ID of top school and score of 10th

#Q2 - Compare median school size vs median size of top 10 score
median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

#Q3 - What is the median size for bottom 10 scores
schools %>% top_n(10, -score) %>% .$size %>% median()

#Q4 - plot average score vs log school size
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) + scale_x_log10()

#Q5 - Use regularization to pick the best schools
#Get overall mean = mu
overall <- mean(sapply(scores, mean))

#Estimate score above average regularized by n + a where n is school size and a = 25
a <- 25
schools <- schools %>% mutate(b_i = (score-overall)*size/(size+a)) %>%
  mutate(reg_score = overall+b_i)
schools %>% top_n(10, reg_score) %>% arrange(desc(reg_score))

##Official code - don't rely on school means, regularize *all* scores
score_reg <- sapply(scores, function(x) { overall + sum(x-overall)/(length(x)+a)})
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q6 find a that minimizes RMSE
a <- seq(10, 250, 1)


alphas <- sapply(a, function(a){ 
  score_reg <- sapply(scores, function(x) overall + sum(x-overall)/(length(x)+a))
  schools_reg <- schools %>% mutate(score_reg = score_reg)
#  schools_reg <- schools_reg %>% mutate(q_est = rank(-score_reg))
  return((1/100)*sum((schools_reg$quality - schools_reg$score_reg)^2) )
})


qplot(a, alphas)

a[which.min(alphas)]

#Q7 use best alpha to find top 10
a_best <- a[which.min(alphas)]

schools %>% mutate(score_reg = sapply(scores, function(x) {
  overall + sum(x-overall)/(length(x)+a_best)
})) %>% top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q8 re-run alpha cross-validation without subtracting out overall average
a <- seq(10, 250, 1)


alphas2 <- sapply(a, function(a){ 
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+a))
  schools_reg <- schools %>% mutate(score_reg = score_reg)
  #  schools_reg <- schools_reg %>% mutate(q_est = rank(-score_reg))
  mean((schools_reg$quality - schools_reg$score_reg)^2) 
})


qplot(a, alphas2)

a[which.min(alphas2)]

