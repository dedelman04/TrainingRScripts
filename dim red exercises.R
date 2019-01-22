##Dimension Reduction
#Q1 - plot tissue gene expression data "principle components" and look for clusters

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

data.frame(tissue_gene_expression$x[,1:2], tissue=tissue_gene_expression$y) %>%
  ggplot(aes(x=MAML1, y=LHPP, color = tissue )) + geom_point()