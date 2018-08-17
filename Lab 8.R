##Lab 8
##Q1
set.seed(449)
your.dates<-as.Date(sample(18000:20000,20), origin = "1960-01-01")

your.days<-c(julian(your.dates,origin=as.Date("1960-01-01")))

your.days[3]

##Q2
weekdays(your.dates)

old.locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")

weekdays(your.dates)

Sys.setlocale("LC_TIME", old.locale)

#Q3 & 4
set.seed(119) 
my.days<-sample(18000:20000,20)
my.days.structure<-month.day.year(my.days,origin=c(1,1,1960))
my.dates<-as.Date(my.days, origin = "1960-01-01")
my.date.info<-data.frame(Weekday=weekdays(my.dates),my.days.structure) 
tail(my.date.info)

