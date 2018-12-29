
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

getModelInfo("Rborist")
modelLookup("Rborist")

#Q1 - minimum RSME on Rborist method changing minimum node size
library("Rborist")
set.seed(1)
tGrid <- data.frame(predFixed = 1, minNode = seq(25, 100, 25))
train_rborist <- train(y ~ .,
             method = "Rborist",
             data = dat,
             tuneGrid =  tGrid)

ggplot(train_rborist, highlight=TRUE)
train_rborist$bestTune

#Q2 scatter plot y-hat 
library(caret)
dat %>% 
  mutate(y_hat = predict(train_rborist)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
#  geom_smooth(aes(x, y_hat), col = 2)

#Q3 highest accuracy of classification tree against tissue_gene_expression
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
library(rpart)

set.seed(1991)
train_rpart <- train(x, y, method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, .01)))
#                     data = tissue_gene_expression)

ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune

#Q4 - based on best tuned ConfMatrix, what conclusions about placenta?
train_rpart$finalModel
confusionMatrix(train_rpart)

#Q5 - rerun rpart allowing to split on any nodes and check accuracy
set.seed(1991)
train_rpart_minsplit <- train(x, y, method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, .01)),
                     control = rpart.control(minsplit = 0))

confusionMatrix(train_rpart_minsplit)

#Q6 - plot tree from Q5, look at 1st gene split
plot(train_rpart_minsplit$finalModel, margin=0.1)
text(train_rpart_minsplit$finalModel, cex=.75)

#Q7 - random forest using "rf" method; mtry from 50-200 by 25s
# find mtry value that maximized accuracy
set.seed(1991)

train_rf <- train(x, y, method="rf",
                  tuneGrid = data.frame(mtry=seq(50,200,25)),
                  nodesize=1)

train_rf$bestTune
confusionMatrix(train_rf)
ggplot(train_rf, highlight = TRUE)

#Q8 - apply varImp to the model from Q7
imp <- varImp(train_rf)
imp

#Q9 - extract top 7 predictors from randomForest
#As an example, here is how with rpart
tree_terms <- as.character(unique(train_rpart_minsplit$finalModel$frame$var[!(train_rpart_minsplit$finalModel$frame$var == "<leaf>")]))
tree_terms

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)

