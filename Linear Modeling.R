
#Galton Heights Data
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#Determining LSE coefficient (beta1) with fixed intercept (beta0)
rss <- function(beta0, beta1, data) {
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 <- seq(0, 1, len=nrow(galton_heights))
results <- data.frame (beta1 = beta1,
                       rss = sapply(beta1, rss, beta0 = 36))

results %>% ggplot(aes(beta1, rss)) + geom_line(aes(beta1, rss), col=2)

#Monte Carlo for coefficient/intercept
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

lse %>% ggplot(aes(beta_0)) + geom_histogram()
lse %>% ggplot(aes(beta_1)) + geom_histogram()

#Monte Carlo for normalized X
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef
})

#Different method for drawing geom_smooth(method="lm")
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


#Lahman baseball data
baseball_data <- Teams %>% filter(yearID >= 1961 & yearID <= 2001) %>%
  mutate(RunsPerGame = R/G, BBPerGame = BB/G, HRPG = HR/G, HRPerGame_Strata = round(HR/G, 1)) %>%
  filter(HRPerGame_Strata >= 0.4 & HRPerGame_Strata <= 1.2)

rpg_lm <- lm(RunsPerGame ~ BBPerGame + HRPG, baseball_data)

rpg_lm

summary(rpg_lm)

#do function
baseball_data %>% group_by(HRPerGame_Strata) %>%
  do(fit = lm(RunsPerGame ~ BBPerGame, data = .))
#Must specify column name ("fit") or will return results of lm which is not a data frame

get_slope <- function(data) {
  fit <- lm(RunsPerGame ~ BBPerGame, data = data)
  data.frame(slope = fit$coefficients[2],
              se = summary(fit)$coefficient[2,2])
}

baseball_data %>% group_by(HRPerGame_Strata) %>%
  do(get_slope(.))
#No output naming needed since function get_slope() is already returning a data frame

get_lse <- function(data) {
  fit <- lm(RunsPerGame ~ BBPerGame, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[, 2])
}

baseball_data %>% group_by(HRPerGame_Strata) %>% do(get_lse(.))

#broom library
#3 main functions
#   tidy() <- return estimates and related info of lm() as a data frame
#   glance() <- returns model specific summaries
#   augment() <- returns observation specific summaries

#DF of summary data
fit <- lm(RunsPerGame ~ BBPerGame, data = baseball_data)
tidy(fit)

#Append lm detail to grouped tibble
baseball_data %>% group_by(HRPerGame_Strata) %>%
  do(tidy(lm(RunsPerGame ~ BBPerGame, data = .), conf.int = TRUE))

#get only desired columns
baseball_data %>% group_by(HRPerGame_Strata) %>%
  do(tidy(lm(RunsPerGame ~ BBPerGame, data = .), conf.int = TRUE)) %>%
  filter(term == "BBPerGame") %>%
  select(HRPerGame_Strata, estimate, conf.low, conf.high)

#plot slope estimates with error bars (CIs)
baseball_data %>% group_by(HRPerGame_Strata) %>%
  do(tidy(lm(RunsPerGame ~ BBPerGame, data = .), conf.int = TRUE)) %>%
  filter(term == "BBPerGame") %>%
  select(HRPerGame_Strata, estimate, conf.low, conf.high) %>%
  ggplot(aes(x=HRPerGame_Strata, y=estimate, ymin=conf.low, ymax=conf.high)) +
    geom_errorbar() +
    geom_point()

#Note that CIs all overlap at y ~ 0.4, so H0 that slopes do not change stratified
#by HR per game is not rejected

#HR vs Run per league
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>% group_by(lgID) %>%
  do(tidy(lm(R ~ HR, data = .), conf.int = TRUE)) %>%
  filter(term == "HR")
#show plot
# %>% ggplot(aes(x=lgID, y=estimate, ymin=conf.low, ymax=conf.high)) + 
#   geom_errorbar() + geom_point()
