# Input load. Please do not change #
dataset = read.csv('C:/Users/dedelman/REditorWrapper_cc2e396a-e58d-49f2-9ecf-67765c0fdd61/input_df_7fa4b36d-1826-44a9-8604-2543374a06f6.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
library(ggplot2)
library(reshape)
colnames(dataset) <- c("Idx", "Name", "Value", "Q.Name", "Question", "Time")


#cleanDF.up <- melt(cleanDF, c("SurveyIdx", "SurveyDate", "Email", "Status", "Q6Comment"),
#                   variable_name = "QuestionNo")
ds <- melt(dataset, c("Idx", "Name", "Q.Name", "Question", "Time", "Value"))
ds <- cast(ds, Name + Q.Name + Question ~ Time, fun.aggregation="sum")

ds$Q.Factor <- factor(ds$Q.Name, levels=c("Collaborate", "Create", "Compete", "Control"))

label.high <- c(25, 80, "Not enough emphasis")
label.low <- c(80, 40, "Too much emphasis")
label.jr <- c(90, 90, "Just right")

plot<-ggplot(ds, aes(x=Current, y=Future))+geom_point() + ylim(0,100) + xlim(0,100)
plot<-plot + facet_wrap(~ Q.Factor, ncol = 2) + xlab("Current") + ylab("Future") + ggtitle("Now vs Future - IT")
##plot+aes(color=Question)
plot<-plot+aes(color=Question)+geom_segment(aes(x=0, y=0, xend=100, yend=100), colour="black")
plot<-plot+theme(legend.position="bottom")+theme(legend.title = element_blank())
plot<-plot+annotate("text", x=as.numeric(label.high[1]), y=as.numeric(label.high[2]), label=label.high[3], size=3.5)
plot<-plot+annotate("text", x=as.numeric(label.low[1]), y=as.numeric(label.low[2]), label=label.low[3], size=3.5)
plot+annotate("text", x=as.numeric(label.jr[1]), y=as.numeric(label.jr[2]), label=label.jr[3], size=3.5)
