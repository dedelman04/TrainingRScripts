#often convenient to store predictors in a matrix and outcomes in a vector
#rather than all together in a data frame

mnist <- read_mnist()  #This function loads predictors as a matrix in train$images
class(mnist$train$images)

#Contains 60,000 images, so for our purposes, narrow down to first 1000 predictors and 1000 labels
x <- mnist$train$images[1:1000,]  #Matrix
y <- mnist$train$labels[1:1000]   #Vector

#Matrices are used because Linear Algebra is implemented in base R and other packages

#Going to use 5 challenges to demonstrate use of matrices
#1 Study distribution of total pixel darkness, and how it varies by digit
#2 Study variation of each pixel and remove predictors (pixels) that do not provide much
# predictive power because they are relatively constant
#3 Zero out low values that are likely smudges; pick a cutoff based on dist of all pixel values
#  to determine a likely cutoff point and define anything below that as unwritten space (set to 0)
#4 "Binarize" the data - for data below cutoff, assign 0 otherwise 1
#5 Scale each predictor in each entry to have same mean/sd

#tidyverse doesn't handle matrices well - going to generally use base R

# scalar - one value
# vector - one row or column
# matrix - one or more vectors combined together AS COLUMNS

x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)

#dimension is a two-value summary of number of rows and number of columns
dim(x)

#vectors can be thought of a Nx1 matrices, but vectors in R have no dimension
#can explicitly convert a vector into a matrix using as.matrix
dim(as.matrix(x_1))

#matrix function takes a vector and "wraps" it into a matrix
#matrix(vector, dimrows, dimcolumns)
#matrices are filled in column by column
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

#can transpose matrices and fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t

#function "t" will directly transpose a matrix
identical(t(mat), mat_t)

#if the length of the vector is less than rows x columns, R will recycle values 
#starting at the beginning
matrix(my_vector, 5, 5)

#Let's put the values for the 3rd entry of pixel intensities into a grid 28x28
grid <- matrix(x[3,], 28, 28)

#image function will visualize the grid
image(1:28, 1:28, grid)  #Note this is upside down (1 is left and bottom)
#flip by specifying order in which to take the values from grid
image(1:28, 1:28, grid[, 28:1])

#Challenge 1 - sum the values across each row and visualize how this sum varies by digit (label)
# Will use rowSums(matrix) function; also can use rowMeans(matrix) to get average of the row
sums <- rowSums(x)
avg <- rowMeans(x)

#Plot a boxplot to show how the intensity varies image to image
data.frame(y, avg) %>% ggplot(aes(x=as.factor(y), y=avg))+geom_boxplot()

#colSums, colMeans does the same across columns
#package matrixStats add rowSds and colSds
#function apply allows similar to sapply or map, but across a matrix
# apply(matrix, dimension, function)  <- dimension = 1, rows; dimension = 2, columns
# rowMeans(x) <==> apply(x, 1, mean)
avgs <- apply(x, 1, mean)
identical(avg, avgs)

####dedicated functions will perform faster than apply####

# Challenge 2 - study the variation in each pixel and determine which are not good predictors
#  Simplistically we will use SD across all entries; lower SD will be less useful
# remember, each column represent on specific pixel 
sds <- colSds(x)

#Distribution across sds
data.frame(sds) %>% ggplot(aes(sds))+geom_histogram()

#turn into a grid and plot
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#Let's keep only those predictors whose SD is above 60
new_x <- x[, colSds(x) > 60]
dim(new_x)

#Warning-if subsetting a single row or column from a matrix, it is no longer a matrix but a vector
#can use argument "drop" to prevent this
class(x[1,])
dim(x[1,])
class(x[, 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

# Challenge 3 - distribution of pixels
# Matrix can be turned into vector by as.vector()
qplot(as.vector(x), bins=30, color = I("black"))

#we will consider values below ~ 25 as smudges
new_x <- x
new_x[new_x < 25] <- 0

#can use index notation to replace all kinds of ranges
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12 ] <-0
mat

# Challenge 4 - Binarize the data
# Set all values in upper half (> 255/2) to 1, others to 0
####CAUTION - ALWAYS SET ZEROS FIRST####
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

#Can also convert to matrix and coerce
bin_X <- (x > 255/2)*1  #gives False * 1 = 0 or True * 1 = 1

#Compare binarized and non-binarized; note very little loss of information
image(1:28, 1:28, matrix(x[3,], 28, 28))
image(1:28, 1:28, matrix(bin_x[3,], 28, 28))

# Challenge 5 - standardize using vectorization
# matrix +/- vector does the operation row element by row element (vector is column)

x_std <- (x - rowMeans(x)) / rowSds(x)

#### For columns, have to transpose matrix first, then apply vector, then re-transpose
# t(t(x) - colMeans(x))
# can also use function sweep(matrix, dimension, vector to subtract)
# x_mean_0 <- sweep(x, 2, colMeans(x))
#
# subtract is default; use FUN argument to change
# x_std <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

#Matrix multiplication: %*%; crossprod(x) gives cross product of a matrix

#####Exercise
x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)

#For each digit in mnist, compute proportion of pixels in grey area, 50 < x < 205
x <- mnist$train$images
y <- mnist$train$labels

x_grey <- x
x_grey[x_grey <= 50 | x_grey >= 205] <- 0
x_grey[x_grey > 50 & x_grey < 205] <- 1

cnt <- rowMeans(x_grey)
data.frame(y, cnt) %>% ggplot(aes(x=as.factor(y), y=cnt)) + geom_boxplot()

mean(cnt)

