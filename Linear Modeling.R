
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

#####
#linear model for Runs vs BB, 1B, 2B, 3B, HR
# - assumes each is approximately normal
# - assumes they are jointly normal, i.e. if hold all but one fixed, relationship is linear and
#    slope beta does not depend on any of the other 4
#####

#set up linear model
fit <- Teams %>% filter(yearID %in% 1961:2001) %>%
        mutate(BB = BB/G,
               singles = (H-X2B-X3B-HR)/G,
               doubles = X2B/G,
               triples = X3B/G,
               HR = HR/G,
               R = R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

coefs <- tidy(fit, conf.int = TRUE)
coefs

#predict for 2002
Teams %>% filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(x=R_hat, y=R, label=franchID))+geom_point()+geom_label()+geom_abline()

#average plate appearances (simplified) per team per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  .$pa_per_game %>%
  mean

#select player data from 1999-2001, adjusting for average PA per game
#predict runs per game if all 9 players were a single one from the population
players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = AB + BB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
         singles = sum(H-X2B-X3B-HR)/G,
         doubles = sum(X2B)/G,
         triples = sum(X3B)/G,
         HR = sum(HR)/G,
         AVG = sum(H)/sum(AB), #get batting average
         PA = sum(PA)) %>%
  filter(PA >= 300) %>%     #only players with 300+ PA
  select(-G) %>%          #all data except games played
  mutate(R_hat = predict(fit, newdata = .)) #same LM from above
  
players %>% ggplot(aes(x=R_hat)) + geom_histogram(binwidth = 0.5)

#Add salaries
players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

#Add primary position
players <- Fielding %>% filter(yearID == 2002) %>%
  filter(! POS %in% c("OF", "P") ) %>%  #Remove all outfielders (???) and pitchers
  group_by(playerID) %>% 
  top_n(1, G) %>%    #Only most G at that position
  filter(row_number(G) == 1) %>%  #Only take 1st row in case of ties
  ungroup() %>% 
  select(playerID, POS) %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS) & !is.na(salary))

#Add name and year of debut
players <- Master %>% select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID")

#Top 10 in R_hat
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>% top_n(10)

#plot salary(log) vs r_hat, by position
players %>% ggplot(aes(salary, R_hat))+geom_point(aes(color=POS))+scale_x_log10()

#plot only players likely to be available (4+ years of service)
players %>% filter(debut < 1998) %>%
  ggplot(aes(salary, R_hat))+geom_point(aes(color=POS))+scale_x_log10()

######
#Regression Fallacy (aka "Sophomore Slump")
######

#Find all players that have won rookie of the year
playerinfo <- Fielding %>% group_by(playerID) %>%
  arrange(desc(G)) %>% slice(1) %>%   #arrange by games per position and take "top" row
  ungroup() %>% left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerinfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

#Filter only rookie and sophomore stats
ROY <- ROY %>% filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#Pivot to have one column for rookie and one for sophomore BA
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY
#In the top 10, the slump can be easily seen
#over entire ROY dataset, 68% had lower BA in year 2

######
#Now expand to all players 2013-2014 with 130+ AB
#####

two_year <- Batting %>% filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerinfo, by="playerID") %>%
  filter(POS != "P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(-playerID)

#plot & summarize 2013 vs 2014
two_year %>% ggplot(aes(`2013`, `2014`))+geom_point()
summarize(two_year, cor(`2013`, `2014`))
#correlation is there, but weak

#####
#Spurious Correlation
####

#Monte Carlo simulation showing spurious correlation
N <- 25
G <- 1000000
#simulate 1,000,000 groups with 25 members each
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))

#determine correlations between RNV per group
res <- sim_data %>% group_by(group) %>% summarize(r = cor(X,Y)) %>% arrange(desc(r))
res

#plot top 25
sim_data %>% filter(group == res$group[which.max(res$r)]) %>% 
  ggplot(aes(X, Y)) + geom_point() + geom_smooth(method="lm")

#recall that correlation itself is a NRV
res %>% ggplot(aes(x=r))+geom_histogram(binwidth=0.1)

#perform regression and look at p-value  --> "P-hacking"
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))

####
#Cause and effect reversal
####

#Regress father heights based on son heights
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>%
  do(tidy(lm(father ~ son, data=.)))
#model is technically correct, but the interpretation of 
#son height causes father height is incorrect

####
#Confounding
####

#Berkeley admissions data, men v women
data(admissions)
admissions %>% group_by(gender) %>%
  summarize(percentage = round(sum(admitted*applicants)/sum(applicants), 1))

#Chi-squared test of total admitted vs not admitted
admissions %>% group_by(gender) %>%
  summarize(total_admitted = round(sum(admitted/100*applicants)),
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>%
  do(tidy(chisq.test(.)))

#examine data by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)
#note that 4 of 6 majors favor women
#differences all smaller than 14% as totals would indicate
###confounding paradox###

#plot major admission% (i.e. selectivity) vs women applicants, faceted by major
#note that lower selectivity = harder to get into
admissions %>% group_by(major) %>%
  summarize(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants*(gender=="women")/sum(applicants))*100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label=major)) + geom_text()
         
#stack plot for percent admitted by gender by major
admissions %>% mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y=percent_admitted, fill=major))+
  geom_bar(stat = "identity", position = "stack")
#still does not show percent admission by major

#stratify by major, i.e. control for the confounder
admissions %>%
  ggplot(aes(major, admitted, col=gender, size=applicants)) + geom_point()
#differences seems to go away - high admission rate for small # admissions in A & B

#stratify by major, compute difference, then average
admissions %>% group_by(gender) %>% summarize(average=mean(admitted))


