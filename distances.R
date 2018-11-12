#Distance

#random sample of 2's and 7's from mnist
set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind, ]
y <- mnist$train$labels[ind]

#First 3 labels
y[1:3]

#Predictors for first 3 labels
x_1 <- x[1, ]
x_2 <- x[2, ]
x_3 <- x[3, ]

#Find differences = sum of row deltas across all 784 dimensions
sqrt(sum((x_1-x_2)^2))  #7(1) v 7(2)
sqrt(sum((x_1-x_3)^2))  #7(1) v 2
sqrt(sum((x_2-x_3)^2))  #2 v 7(2)

#crossproduct of (Matrix A - Matrix B) = distance^2
sqrt(crossprod(x_1-x_2))
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_2-x_3))

#function dist(matrix) will create an object of type "dist" with all distances between all rows
d <- dist(x)
class(d)
#Many ML functions in R take type dist as input
#But also need to be able to coerce into matrix for index access
as.matrix(d)[1:3, 1:3]

image(as.matrix(d))

#sort by labels
image(as.matrix(d)[order(y), order(y)]) 
  #7v7 in upper right, 2v2 in lower left; 7s are "closer" to each other

#distance can also be done between pairs of predictors
#transpose the matrix then run dist
d <- dist(t(x))
dim(as.matrix(d))

#If you pick a single predictor, can see which pixels are "close" meaning they share/don't share ink
#Ex: 492d pixel

d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

#######Exercises
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d_gene <- dist(tissue_gene_expression$x)
image(as.matrix(d_gene))

cerebellum_1 <- tissue_gene_expression$x[1, ]
cerebellum_2 <- tissue_gene_expression$x[2, ]
colon_1 <- tissue_gene_expression$x[39, ]
colon_2 <- tissue_gene_expression$x[40, ]
endo_1 <- tissue_gene_expression$x[73, ]
endo_2 <- tissue_gene_expression$x[74, ]

sqrt(crossprod(cerebellum_1-cerebellum_2))
sqrt(crossprod(colon_1-colon_2))
sqrt(crossprod(endo_1-endo_2))

as.matrix(d_gene)[c(1,2,39,40,73,74), c(1,2,39,40,73,74)]


