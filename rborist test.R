i <- 0
B <- 100

Rborist_func <- function(i) {

#  Rborist_results <- replicate(B, {
    if(i %% 2 == 0){set.seed(123)} 
    train_rf <- train(x[, col_index],
                    y,
                    method="Rborist",
                    nTree = 50,
                    trControl = control,
                    tuneGrid = grid,
                    nSamp = 5000)
  
    c(i, train_rf$bestTune$predFixed, train_rf$bestTune$minNode)
#  })
}

RF_runs <- sapply(seq(1:100), Rborist_func)

RF_runs <- RF_runs %>% t() %>% as.data.frame(col.names = c("seed", "predFixed", "minNode")) %>%
  mutate(rand = ifelse(seed %% 2 == 0, "Set", "Not Set"))

RF_runs %>% ggplot(aes(x=predFixed))+geom_histogram()+facet_wrap(minNode ~ rand)

res <- matrix(Rborist_results, 3, byrow=FALSE)