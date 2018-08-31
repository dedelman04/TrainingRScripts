#Filter top 50%
dataset <- dataset %>% group_by(ServerName) %>% filter(n() >= med) %>% ungroup()

counts <- dataset %>% group_by(ServerName) %>% tally() #tally is short hand for summarize and getting n()

dataset %>% ggplot(aes(x=ServerName, y=AttemptDur))+
  geom_boxplot()+
  geom_point(aes(color = Success))+ scale_color_manual(values=c("Green", "Red"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(data = counts, aes(label=paste("n =", n), y=0, angle = 90), size = 2.5)+ #annotation layer
  ylim(-5, max+5)