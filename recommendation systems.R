#Recommendation Systems
#Predicting a rating for an item based on other ratings
data("movielens")

#Look at distinct movies and users
movielens %>% summarize(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId))

#We will think of the data as a large matrix with users on rows and movies on columns,
#with many empty cells
#Use gather to put it in that format for a small subset of rows
#(whole data set has too many for R to handle)

#data for a few users
keep <- movielens %>% 
  count(movieId) %>% 
  top_n(5, n) %>% 
  .$movieId

tab <- movielens %>% 
  filter(movieId%in%keep) %>% 
  filter(userId %in% c(13:20)) %>% 
  select(userId, title, rating) %>% 
  mutate(title = str_remove(title, ", The"),
         title = str_remove(title, ":.*")) %>%
  spread(title, rating)

tab

#Task of a recommendation system is basically to fill in the NAs
#sparsity of the matrix for random sample of 100 movies and 100 users
#yellow indicates where a user rated a movie
users <- sample(unique(movielens$userId), 100)
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

#ML problem is more complicated because each Y has a different set of predictors
#For each movie i and user u, there are a different number of ratings for movie i across all users
#and a different number of ratings for user u across all movies
#Can also use information from other movies similar to i or other users similar to u
#This means it is possible to use the entire matrix as predictors for each cell!!

#Exploratory analysis
#Movies vs times rated (x is log scale)
movielens %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

#some blockbusters that are reviewing by many, many "artsy" reviewed by few

#Users vs number of ratings completed
movielens %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")

#Construct train/test set to test accuracy
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times=1, p=0.2, list=FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
#Make sure movies and user that are not in the train set do not appear in the training set
test_set <- test_set %>% semi_join(train_set, by="movieId") %>% semi_join(train_set, by="userId")

#Going to rate the algorithm on residual mean squared error (RMSE)
#RMSE = sqrt[(1/N)*sum(yhat - y)^2]
# N is a number of user/movie combinations
# > 1 is not good

RMSE = function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Netflix challenge winners implemented KNN-based and Matrix Factorization-based models
#We will look at matrix factorization

#Simplest solution - predict same rating for all movies regardless of movie or user
#Model based solution: Y = mu + epsilon (epsilon is independent error from same distro, cetnered at 0)
#Estimate that minimizes the RSME is the least square estimate of mu --> mean
mu_hat <- mean(train_set$rating)
mu_hat

#Assume epsilon is 0 and compute RMSE against the test set
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#Goal is RMSE around 0.857
#Store results in a table for future reference
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#Next step - add a factor that takes into account the average rating for movie i
# Y = mu + b(i) + epsilon; "b" represents bias (statistics generally calls them "effects")

#Again, use least squares to estimate b's 
fit <- lm(rating ~ as.factor(movieId), data=movielens)  #DO NOT RUN THIS - too many iterations
# bhat(i) = average of y(ui) - mu
#(going to stop using "hat" notation to make code cleaner)
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

#Distribution of b_i
movie_avgs %>% ggplot(aes(x=b_i))+geom_histogram(bins=10)

#Recall that mu = 3.5, so b(i) = 1.5 implies a perfect 5-star rating
#Now predict using mu +b(i) model
predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by="movieId") %>% .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                         data_frame(method = "Movie Effect Model", RMSE = model_1_rmse))
rmse_results

#How about user effect?
#Explore data - histogram of average user ratings for users with over 100 movies rated
train_set %>% group_by(userId) %>% 
              summarize(b_u = mean(rating)) %>% 
  filter(n() >= 100) %>% ggplot(aes(x=b_u))+geom_histogram()

#Next improvement is y = mu + b(i) +b(u) + epsilon

fit <- lm(rating ~ as.factor(movieId) + as.factor(userId), data=movielens)  #DO NOT RUN THIS - too many iterations
#Approximation is that b(u) is average of residuals left from y(ui)-mu-b(i)
user_avgs <- test_set %>% left_join(movie_avgs, by="movieId") %>% group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#predict and calculate error
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User effect, test b(u)",
                                                   RMSE = model_2_rmse))
#Now do it by setting b(u) with the training set
user_avgs <- train_set %>% left_join(movie_avgs, by="movieId") %>% group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User effect, train b(u)",
                                                   RMSE = model_2_rmse))

