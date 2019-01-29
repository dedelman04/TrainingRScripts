#Regularization (data setup is on "recommendation systems")
#Show largest sources of error for just adding movie effect
test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>% slice(1:10)

#Top 10 and bottom 10 movies based on estimated movie effect
movie_titles <- movielens %>% select(movieId, title) %>% distinct()
#Top 10
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% select(title, b_i) %>% slice(1:10)
#Bottom 10
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% select(title, b_i) %>% slice(1:10)

#How often were they rated?
train_set %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% select(title, b_i, n) %>%
  slice(1:10)

train_set %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% select(title, b_i, n) %>%
  slice(1:10)
#Note small number of total ratings
#Small number of users leads to large abs(b_i)
#Noisy estimates that need to be handled or filtered out
#Will need to add 'penalty' for large b's, and minimize the equation that has this penalty added
#
#(1/N)*RMSE + lambda*sum(b(i)^2);   lambda*sum*(b(i)^2) is the penalty
#
#Calculus shows that the values of b that minimize the penalty are
#
#b(lambda) = (1/(lambda+n(i)))*sum(Y(u,i) - mu)
#
#So if n(i) is large, lambda is ignored becuase n(i)+lambda ~ n(i)
#If n(i) is small, the estimate of b(i) shrinks towards zero, with larger lambda shrinking more

#Show regularized estimates with lambda = 3
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating-mu)/(n()+lambda), n_i = n())

#Plot regularized estimate RMSE vs original RMSE
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#Top 10 movies based on b(lambda)
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

#Bottom 10
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

#Check RMSE compared to other methods
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))

#Cross validate to choose correct lambda
lambdas <- seq(0, 10, .25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

#Cross validate with train set to determine lambda when using movie and user effect
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)
lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))


