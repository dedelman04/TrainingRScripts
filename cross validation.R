#Cross-validation
#Minimize mean squared error
#Really we are estimating the mean squared error using the observed mean square error
# MSE is "True" error, Observed MSE is "Apparent" error
#2 characteristics of Apparent Error
# 1 It is a Random variable
# 2 If we train an algorithm on the same data set we are use to compute the apparent error
#   we are overtraining --> underestimate the true error
#
# Cross validation prevents both problems
#
# True error can be thought of as the mean of the apparent errors of B trials of different,
#  random data sets
#

# Remember - ML problems start with a data set, that is used to build the algorithm
#  But it will be applied against an independent data set that we never get to see
# Splitting into training and test sets mimics having an unknown, independent data set
# Generally looking to use 10-20% of the data set as test
#
##### K-fold cross validation
# Collect a set of algorithm parametes lambda, estimate the MSE for each set of parameters
#   Choose lambda with the smallest MSE
#   Cross-validation will provide this esimate
#
# From a training set of data, choose M = N/K data points as a random sample --> validation set

####Exercises
#1 - write code to cross-validate logistic regression model on completely independent variables
# using caret::train

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method="glm")

fit$results

#2 fit a T-test and get a vector of p-values
library(devtools)
devtools::install_bioc("genefilter")


install.packages("BiocManager")
BiocManager::install("genefilter",version = "3.8")
library(genefilter)

source("https://bioconductor.org/biocLite.R")
  biocLite("genefilter")
  
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value

#3 find values of x_* that are statistically significantly associated with y (p-value <0.01)
ind <- tt[pvals < 0.01, ]

#4 re-train with new subset
x_subset_p <- x[ ,rownames(ind)]

fit_p <- train(x_subset_p, y, method="glm")

fit_p$results

#5 run cross-validation again, but with knn, 101 <= k <= 301 by 25s; plot accuracies
ks <- seq(101, 301, 25)

fit_k <- train(x_subset, y, method = "knn", tuneGrid=data.frame(k = ks))
ggplot(fit_k)

#7 train for tissue_gene_expression

library(dslabs)
data("tissue_gene_expression")

ks <- seq(1:101)
pred_matrix <- tissue_gene_expression$x
class_vec <- as.factor(tissue_gene_expression$y)

fit_gene <- train(pred_matrix, class_vec, method = "knn", tuneGrid=data.frame(k = ks))
ggplot(fit_gene)
