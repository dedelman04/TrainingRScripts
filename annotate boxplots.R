dataset <- dataset %>% group_by(ServerName) %>% filter(n() >= med) %>% 
  mutate(Success = Success, AttemptDur = AttemptDur) %>% ungroup()

counts <- dataset %>% group_by(ServerName) %>% tally()

dataset %>% ggplot(aes(x=ServerName, y=AttemptDur))+
  geom_boxplot()+
  geom_point(aes(color = Success))+ scale_color_manual(values=c("Green", "Red"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1))+
  geom_text(data = counts, aes(label=paste("n =", n), y=0, angle = 90), size = 2.5)+
  ylim(-5, max+5)