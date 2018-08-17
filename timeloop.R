
#loop:
time1<-as.numeric(Sys.time())
for (i in 1:k) {
	my.summary[1,i] <- min(x[,i])
	my.summary[2,i] <- median(x[,i])
	my.summary[3,i] <- mean(x[,i])
	my.summary[4,i] <- max(x[,i])
}
time2<-as.numeric(Sys.time())
# using object form in R:
sapply(x,summ)
time3<-as.numeric(Sys.time())
# run time increase factor:
time2-time1
time3-time2
