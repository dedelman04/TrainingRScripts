#Generative models - exercises
#data set of cerebellum and hippocampus, with 10 randomly selected predictors
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]


#Q1 - train the model with LDA and report accuracy
index_train <- createDataPartition(y, p=0.5, list = FALSE)
train_set <- data.frame(y = factor(y[index_train]),
                        x = x[index_train,])
test_set <- data.frame(y = factor(y[-index_train]),
                       x = x[-index_train,])

fit_lda <- train(y ~ ., method = "lda", data = train_set)

y_hat <- predict(fit_lda, test_set)
confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]

#Q2 determine which genes are driving the algorithm by plotting
#distribution means against each other

mean_cer <- fit_lda$finalModel$means[1, ]
mean_hip <- fit_lda$finalModel$means[2, ]
labs <- colnames(fit_lda$finalModel$means)

data.frame(mean_cer, mean_hip, factor(labs)) %>% 
  ggplot(aes(x=mean_cer, y=mean_hip))+geom_point()+
  geom_abline(slope = 1, intercept = 0)+geom_text(aes(label=labs))

##Official answer
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Q3/4 Repeat but with QDA
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

index_train <- createDataPartition(y, p=0.5, list = FALSE)
train_set <- data.frame(y = factor(y[index_train]),
                        x = x[index_train,])
test_set <- data.frame(y = factor(y[-index_train]),
                       x = x[-index_train,])

fit_qda <- train(y ~ ., method = "qda", data = train_set)

y_hat_qda <- predict(fit_qda, test_set)
confusionMatrix(data = y_hat_qda, reference = test_set$y)$overall["Accuracy"]

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Q5 Run LDA, but with "preProcess = 'scale'" to better show the
#difference between the predictor means
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

index_train <- createDataPartition(y, p=0.5, list = FALSE)
train_set <- data.frame(y = factor(y[index_train]),
                        x = x[index_train,])
test_set <- data.frame(y = factor(y[-index_train]),
                       x = x[-index_train,])

fit_lda <- train(y ~ ., method = "lda", data = train_set, preProcess="scale")
fit_lda_center <- 
  train(y ~ ., method = "lda", data = train_set, preProcess="center")
y_hat <- predict(fit_lda, test_set)

mean_cer <- fit_lda$finalModel$means[1, ]
mean_hip <- fit_lda$finalModel$means[2, ]
labs <- colnames(fit_lda$finalModel$means)

data.frame(mean_cer, mean_hip, factor(labs)) %>% 
  ggplot(aes(x=mean_cer, y=mean_hip))+geom_point()+
  geom_abline(slope = 1, intercept = 0)+geom_text(aes(label=labs))

t(fit_lda_center$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#side note - distributions of top two predictors
d <- apply(fit_lda_center$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

#Q6 LDA against all tissue types (e.g. more than 2 classes)
set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

#use x,y version of train, not formula
fit_lda_7 <- train(x, y, method="lda")

#display fitted model to the console
fit_lda_7