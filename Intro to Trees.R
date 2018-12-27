data("olive")

head(olive)

#Remove area from dataset
olive <- select(olive, -area)

#Going to use fatty acid composition to predict region
table(olive$region)

#predict using KNN with odd neighbors from 1 to 15
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

#visualize predictor distribution stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free")

#note that eicosenoic is only in Southern Italy, and linoleic is sharply
#different between Northern Italy and Sardinia

#Use those two predcitors only; should be able to get a perfect algorithm
olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()+ geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.535, xend = 0.065, yend = 10.535, color = "black", lty = 2)

#This leads to a decision tree approach
# If eicosenoic > 0.065 then Southrn Italy
#   else if lineolic > 10.535 then Sardinia
#   else Northern Italy
