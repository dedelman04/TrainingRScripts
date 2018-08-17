library(tidyverse)
library(dslabs)
data(heights)

#eCDF: empirical continuous distribution function
F <- function(a) {mean(x<=a)}

x <- heights %>% filter(sex == "Male") %>% .$height

#proportion of heights > 70 in == 1 - proportion < 70
a <- 70
1 - F(a)

#pnorm -> normal distibution
#value a mean avg, sd s -> pnorm(a, avg, s)

#prob > 70.5 in == 1 - prob(< 70.5)
1 - pnorm(70.5, mean(x), sd(x))

#dnorm -> probability density curve
dnorm(70.5, mean(x), sd(x))

######
#Prefixes for distributions
#
#p - probability
#d - prob denisty
#q - quantile
#r - random (Monte Carlo)
#
# e.g. pnorm, dgamma, qt, rchisq
######