#Clustering exercises
data("tissue_gene_expression")

#Q1 - remove row means and computer distance
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

#Q2 - Hierarchical plot, tissue type as labels
h <- hclust(d)
plot(h, cex=0.75)

#Q3 fit K-means cluster and compare to actuals
k <- kmeans(tissue_gene_expression$x, centers = 7)
groups <- k$cluster
split(names(groups), groups)

#Q4 heatmap
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]

heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), 
        scale = "row", ColSideColors = colors)
