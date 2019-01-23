##Dimension Reduction
#Q1 - plot tissue gene expression data "principle components" and look for clusters

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

x <- tissue_gene_expression$x %>% as.matrix()
d <- dist(x)


pca <- prcomp(x)
summary(pca)

data.frame(pca$x[,1:2], tissue=tissue_gene_expression$y) %>%
  ggplot(aes(x=PC1, y=PC2, color = tissue )) + geom_point()

#Q2 Plot mean of all predictors against PC1, find correlation
mean_all <- rowMeans(tissue_gene_expression$x)

data.frame(PC1 = pca$x[,1], MeanAllPred = mean_all, tissue=tissue_gene_expression$y) %>%
  ggplot(aes(y=PC1, x=MeanAllPred, color = tissue)) + geom_point()

cor(mean_all, pca$x[, 1])

#Q3 redo PCA with means removed
x2 <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))

pc <- prcomp(x2)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q4 Boxplots for first 10 PCs against tissue; 
#which 2 tissue have greatest median difference on 7th PC?

data.frame(PC = pc$x[, 7], tissue=tissue_gene_expression$y) %>%
  ggplot(aes(x=tissue, y=PC)) + geom_boxplot()

lapply(seq(1:10), function(i){df <- data.frame(PC = pc$x[, i], tissue=tissue_gene_expression$y) 
lab <- paste("PC", as.character(i))
ggplot(aes(x=tissue, y=PC), data = df) + ylab(lab) + geom_boxplot()
})

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#Q5 - how many PCs are needed to explain 50% of the variance?
summary(pc)$importance[3, ] %>% plot()

