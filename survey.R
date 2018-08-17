survey.data <- read.csv(file="H:/DEdelman/Competing Values/Competing Values Scorecard-CLNG.csv", header=TRUE, nrows=18)

survey.data <- subset(survey.data, select = -X)
##survey.data <- cbind(survey.data[,1], as.numeric(survey.data[,2:20])

survey.data$Survey <- as.character(survey.data$Survey)

mean.vec <- c("AVG", " ")

for (i in 3:20) {
	survey.data[,i] <- as.numeric(survey.data[,i])
	mean.vec <- c(mean.vec, paste("mean(survey.data[," , as.character(i), "])"))
}


survey.data <- rbind(survey.data, c("AVG", "", mean(c(survey.data[,3:20]))))
