
setwd("H:/Dedelman/CustSat")
library(reshape)
library(dplyr)

###########User input variables###########

surveyDate <- as.Date("4/30/2018", format="%m/%d/%Y")  #input official survey date here

##########################################

#idx = as.Int(121) #input starting index value here

old=read.csv(file="Survey.csv", header=TRUE, sep=",")
idx <- max(old[,1])+1

cols <- c("Responder",
          "Collector",
          "StartDate",
          "EndDate",
          "IPAddress",
          "EmailAddress",
          "FirstName",
          "LastName",
          "Custom",
          "Q1",
          "Q2",
          "Q3",
          "Q4",
          "Q5",
          "Q6",
          "Q7",
          "Comment")


dataset=read.csv(file="IT Customer satisfaction Survey.csv", 
                 header=FALSE, 
                 sep=",", skip=2, col.names = cols, as.is=TRUE)


#dataset=read.table(file="H:/DEDELMAN/IT Customer satisfaction Survey.csv", 
#                 header=FALSE, 
#                 sep=",", skip=2, col.names = cols, stringsAsFactors=FALSE)

#extract name from email
atGrep <- function(x){regexpr("@", x)}
atVector <- sapply(dataset$EmailAddress, atGrep)
dataset$FirstName <- substr(dataset$EmailAddress, 1, atVector-1)

#remove any rows with no name
#dataset <- subset(dataset, FirstName!="")
dataset %>% filter(FirstName != "")


#set blank responses to "NONE"
dataset$Q1[dataset$Q1==""]<-"NONE"
dataset$Q2[dataset$Q2==""]<-"NONE"
dataset$Q3[dataset$Q3==""]<-"NONE"
dataset$Q4[dataset$Q4==""]<-"NONE"
dataset$Q5[dataset$Q5==""]<-"NONE"
dataset$Q6[dataset$Q6==""]<-"NONE"
dataset$Q7[dataset$Q7==""]<-"NONE"

#data frame for getting numerical score
responseDF <- data.frame(Q1=c("NONE", 
                              "Strongly disagree", 
                              "Disagree", 
                              "Neither agree nor disagree", 
                              "Agree", 
                              "Strongly agree"),
                         Q1Score=c(0:5))

#join data to score table multiple times to score each question
#dynamically build the "by" parameter to step through each Qx column
for (i in 1:7){
  nm <- paste("Q", i, sep="")
  name <- paste("Q", i, "Score", sep="")

  colnames(responseDF)<-c(nm, name)
  if (i > 1) { tempDF <- scoredDF
          scoredDF <- merge(tempDF, responseDF, by=nm, all.x=TRUE, sort=FALSE)}
  else scoredDF <- merge(dataset, responseDF, by=nm, all.x=TRUE, sort=FALSE)
}

#Rebuild DF with only needed columns; add index and survey date and placeholder for "INC" indicator
idxEnd <- idx+nrow(scoredDF)-1
cleanDF <- data.frame(SurveyIdx = c(idx:idxEnd) #+nrow(scoredDF))
                      ,SurveyDate = c(rep(surveyDate, nrow(scoredDF)))
                      ,Email = scoredDF$FirstName
                      ,Q6Comment = scoredDF$Comment
                      ,Q1 = scoredDF$Q1Score
                      ,Q2 = scoredDF$Q2Score
                      ,Q3 = scoredDF$Q3Score
                      ,Q4 = scoredDF$Q4Score
                      ,Q5 = scoredDF$Q5Score
                      ,Q6 = scoredDF$Q6Score
                      ,Q7 = scoredDF$Q7Score
                      ,Status = c(rep("", nrow(scoredDF)))
                      , stringsAsFactors = FALSE)

#Mark incompletes if any of the Q values are 0
cleanDF$Status[cleanDF$Q1==0 | 
                 cleanDF$Q2==0 |
                 cleanDF$Q3==0 |
                 cleanDF$Q4==0 |
                 cleanDF$Q5==0 |
                 cleanDF$Q6==0 |
                 cleanDF$Q7==0] <- "INC"

#Export the "surveys" tab
write.table(data.frame(SurveyIdx = cleanDF$SurveyIdx,
                       "Survey Date" = cleanDF$SurveyDate, 
                       Email = cleanDF$Email,
                       Status = cleanDF$Status,
                       "Q6 Comment" = cleanDF$Q6Comment),
          file = "Survey.csv",
          sep = ",",
          col.names = TRUE,
          row.names = FALSE)

#Unpivot to get each question on a row
cleanDF.up <- melt(cleanDF, c("SurveyIdx", "SurveyDate", "Email", "Status", "Q6Comment"),
                   variable_name = "QuestionNo")

#Change "Qx" to just "x" for question number;
#have to convert to char from factor, then remove the Q, then convert to integer
cleanDF.up$QuestionNo <- as.integer(sub("Q", "", as.character(cleanDF.up$QuestionNo)))


#Export the "responses" tab
write.table(data.frame(SurveyIdx = cleanDF.up$SurveyIdx,
                       Question.No = cleanDF.up$QuestionNo,
                       Answer = cleanDF.up$value),
            file = "Responses.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE)

#Export the EPM structure
#First set Incomplete = (blank) to 0
cleanDF.up$Status <- sub("^$", "0", cleanDF.up$Status)

write.table(data.frame(SurveyIdx = cleanDF.up$SurveyIdx,
                       Question.No = cleanDF.up$QuestionNo,
                       Answer = cleanDF.up$value,
                       Survey.Date = cleanDF.up$SurveyDate,
                       Incomplete = cleanDF.up$Status),
            file = "EPM.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE)

#Read in question texts only
qtext = read.csv(file="IT Customer satisfaction Survey.csv", 
                 header=FALSE, 
                 sep=",", nrows=1, stringsAsFactors = FALSE)

qtext_clean <- as.character(qtext[, 10:16])

q_length <- length(qtext_clean)

q.df <- data.frame(Survey.Date = as.Date(c(rep(surveyDate, q_length))),
                   "Q No" = c(1:q_length),
                   "Q Text" = qtext_clean)

#Write out question file
write.table(q.df,
            file = "QuestionText.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE)
 