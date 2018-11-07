###Case study: 2 or 7
#Going to simplify digit algorithm (784 predictors, 10 categories)
#to 2 predictors:
# % of dark pixels in upper left quadrant (x_1)
# % of dark pixels in lower right quadrant (x_2)
#2 categories
# 2 or 7
#
#Also subset of 1000 digits from overall bank of 60,000, pre-cut into training and test

data("mnist_27")

mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

#p(x1, x2) = Pr(Y=1 | X1=x_1, X2 = x_2) = g(^-1)(b0 + b1x_1 + b2x_2))
# where g(^-1) = exp(x)/(1+exp(x))

fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")

#If p > 0.5, 7 else 2
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference = mnist_27$test$y)

#plot true conditional probability
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p))+geom_raster()+
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

#calculate the boundary
# g(^-1)(b0 + b1x_1 + b2x_2) = 0.5
#  b0 + b1x_1 + b2x_2 = g(0.5) = 0
#  x_2 = -b0/b2 - b1x_1/b2  <-- this is linear; Regression can't be the actual relationship
                                #as the true conditional probabilty is a curve (above)

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

#See the mistakes by plotting data against the regression line
p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)
