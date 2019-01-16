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

RF_runs <- sapply(seq(1:100), Rborist_func) %>% t() %>% colnames(c("seed", "predFixed", "minNode")) %>%
  mutate(rand = ifelse(seed %% 2 ==0, "Set", "Not Set"))


res <- matrix(Rborist_results, 3, byrow=FALSE)