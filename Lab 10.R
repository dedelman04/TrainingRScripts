##Lab 10
## read into data frame

my.data <- read.csv("Data/Lab10.txt")

## Split into genotypes
data1<-my.data$systolic.bp[my.data$Genotype=="BA"]
data2<-my.data$systolic.bp[my.data$Genotype=="BB"]

##run t-test
testResult <- t.test(data1,data2)

##plot
hist(data1)
densityplot(data2)

##simulation
total <- length(my.data)
n1 <- length(data1)

set.seed(1234)
my.new.data<-my.data
my.new.data$Genotype<-"BB"
index.temp<-sample(1:50,n1)  
my.new.data$Genotype[index.temp]<-"BA"
new.data1<-my.new.data$systolic.bp[my.new.data$Genotype=="BA"]
new.data2<-my.new.data$systolic.bp[my.new.data$Genotype=="BB"]
t.test(new.data1,new.data2)$statistic

set.seed(554)
Lab10sim <- function(my.data) {
	my.new.data<-my.data
	my.new.data$Genotype<-"BB"
	index.temp<-sample(1:50,n1)  
	my.new.data$Genotype[index.temp]<-"BA"
	new.data1<-my.new.data$systolic.bp[my.new.data$Genotype=="BA"]
	new.data2<-my.new.data$systolic.bp[my.new.data$Genotype=="BB"]
	t.test(new.data1,new.data2)$statistic
}

my.t.values <- replicate(100000, Lab10sim(my.data))
my.extreme.t <- my.t.values[abs(my.t.values) > 2.027021]

length(my.extreme.t)/length(my.t.values)
