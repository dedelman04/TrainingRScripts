data("polls_2008")
qplot(day, margin, data = polls_2008)

resid <- ifelse(lm(margin ~ day, data=polls_2008)$resid > 0, "+", "-")
polls_2008 %>% mutate(resid = resid) %>% 
  ggplot(aes(x=day, y=margin)) + geom_point(aes(color = resid), size=3) +
  geom_smooth(method="lm", se=FALSE, color = "black")

#ksmooth - take mean of points within a span around x0
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
          #kernel can be box (no weighting) or normal (weighting via normal dist); 
          #normal tends to be smoother
fit_box <- with(polls_2008, 
                ksmooth(day, margin,  x.points = day, kernel="box", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y, smooth_box = fit_box$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")+
  geom_line(aes(day, smooth_box), color="blue")


#loess (Local weighted regression)
#create local lines b0 + b1(xi - x0) around each x0; spans are generally larger
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit_loess <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
      #loess kernel is specified by degree = degree of polynomial to fit
      #####NOTE - default degree is 2 (parabola)#####
      #weighting is done by Tukey tri-weight instead of normal/Gaussian distro
      #loess uses constant number of data points, not constant bin size
      #span parameter in loess is a proportion, not an integer (bandwidth = span * # data points)
      #can fit more robustly, where outliers are identified during first pass and
      #down-weighted for second pass (parameter: family = "symmetric")
polls_2008 %>% 
  mutate(smooth = fit$y, smooth_box = fit_box$y, smooth_loess = fit_loess$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")+
  geom_line(aes(day, smooth_box), color="blue")+
  geom_line(aes(day, smooth_loess), color="black")

#different spans give different smoothness
spans <- c(0.1, 0.15, 0.25, 0.66)
loess_curves <- function (x) {
    fit <- loess(margin ~ day, degree = 1, span = x, data=polls_2008)
    lc <- cbind(day = polls_2008$day, margin = polls_2008$margin,
                span=rep(x, length(polls_2008$margin)), smooth=fit$fitted)
#    df <- rbind(df, lc)
  
}

df <- do.call(rbind, lapply(spans, loess_curves))

data.frame(df) %>% ggplot(aes(day, margin))+geom_point(size=3, alpha=.5, color="grey")+
  facet_wrap(~ span)+
  geom_line(aes(day, smooth), color="black")

  
#loess with degree = 2 vs 1
total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 

#geom_smooth uses loess as default method with default parameters
#best practice to specify parameter within call to geom_smooth()
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth() +
  geom_smooth(color="red",  span = 0.15,
              method.args = list(degree=1))

###Exercises

library(tidyverse)
library(purrr)
library(pdftools)

#data prep
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

#more data prep (NA on this row, need number for loess)
dat <- dat %>% filter(date != "2016-02-29") %>%
  mutate(dt = as.integer(year)*10000+month*100+day)

#set the span proportion to approx 2 months
total_days <- as.integer(diff(range(dat$date)))
span <- 60/total_days

####fit a loess model and plot
pr_fit <- loess(deaths ~ dt, degree = 1, span = span, data = dat)

dat %>% mutate(fit = pr_fit$fitted) %>% 
  ggplot(aes(date, deaths)) + geom_point() + 
  geom_line(aes(date, fit), color = "red", size = 3)

####plot fit per day of year, one line for each year
dat %>% mutate(fit = pr_fit$fitted) %>% 
  ggplot(aes(yday(date), deaths)) + #geom_point() + 
  geom_line(aes(yday(date), fit, color = year), size = 2)

#Official answer
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#predict 2 vs 7 by x2 only
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

#fit loess to mnist_27$train
span = 50/length(mnist_27$train)

fit27 <- loess(as.integer(y) ~ x_2, degree = 1, span = span, data = mnist_27$train)

mnist_27$train %>% mutate(fit = fit27$fitted) %>% ggplot(aes(x_2, fit))+geom_line()


#official answer (using geom_smooth - can't use loess bc binary)
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

