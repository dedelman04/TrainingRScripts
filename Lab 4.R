set.seed(9852)
my.data <- list()
for (i in 1:100) { my.data[[i]] <- matrix(rnorm(16), nrow = 4) }

my.index<-list()
for (i in 1:100) {
	my.index[[i]] <- my.data[[i]] < 0
}

my.negatives<-matrix(rep(0,16),nrow=4)
for(i in 1:100){
  my.negatives <- my.negatives+my.index[[i]]
}
my.negatives

my.negative.values<-numeric(0)
for (i in 1:100) {
	my.negative.values <- c(my.negative.values,my.data[[i]][my.index[[i]]])
}

