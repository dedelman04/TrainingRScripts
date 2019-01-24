#Q1 plot number of ratings vs movie year; use square root transformation on counts
data("movielens")

movielens %>% filter(year >= 1985) %>% group_by(movieId, year) %>% summarize(cnt = n()) %>%
  ggplot(aes(x=year, y=sqrt(cnt), group=year)) + geom_boxplot()

##Official code
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Q2 - for movies after 1993, what are the 25 movies with most ratings per year and what is the
#average rating of each?

movielens %>% filter(year > 1993) %>% group_by(movieId, year, title) %>% 
  summarize(cnt = n(), avg_rating = mean(rating)) %>%
  arrange(desc(cnt))

###Official code
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 

#Q3 stratify post 1993 movies by ratings per year and compute average ratings

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%  mutate(rate_strat = floor(rate)) %>% group_by(rate_strat) %>%
  summarize(avg_rat = mean(rating)) %>% ggplot(aes(x=rate_strat, y=avg_rat))+geom_point()+geom_smooth()


#Q6 - trend of average rating by day of week
movielens <- movielens %>% mutate(date = as_datetime(timestamp))

movielens %>% mutate(week = round_date(date, unit="week"), day = round_date(date, unit="day")) %>%
  group_by(week) %>% summarize(avg_rat = mean(rating), day = first(day)) %>%
  ggplot(aes(day, avg_rat)) + geom_point() + geom_smooth()

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#Q8 group by genre/category, Mean, std error for each category with > 1000 ratings
movielens %>% group_by(genres) %>% 
  summarize(cnt = n(), mean = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(cnt > 1000) %>% mutate(genres = reorder(genres, mean)) %>%
  ggplot(aes(x=genres, y=mean, ymin=mean-2*se, ymax=mean+2*se)) + geom_point() + 
  geom_errorbar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
