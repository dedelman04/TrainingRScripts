setwd("H:/DEdelman/")

library(maps)## load maps first to avoid map conflict with purrr
library(MASS)## load MASS and matrixStats first to avoid select and count conflict
library(matrixStats)
library(data.table)
library(tidyverse)
#library(htmlwidgets)
#library(RevoScaleR)
library(dplyr)
library(stringr)
library(lubridate)
#library(rgeos) # spatial package
#library(sp) # spatial package
#library(maptools) # spatial package
#library(ggmap)
library(ggplot2)
library(gridExtra) # for putting plots side by side
library(ggrepel) # avoid text overlap in plots
library(tidyr)
#library(seriation) # package for reordering a distance matrix
#library(gtools)
library(dslabs)
library(Lahman)
#library(googlesheets)
library(readr)
library(readxl)
library(HistData)
library(broom)
library(lpSolve)
library(rvest)
library(caret)
library(pdftools)
library(rpart)
library(randomforest)

plot_cond_prob <- function(p_hat=NULL){ 
  tmp <- mnist_27$true_p 
  if(!is.null(p_hat)){ tmp <- mutate(tmp, p=p_hat) } 
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) + 
    geom_raster(show.legend = FALSE) + 
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) + 
    stat_contour(breaks=c(0.5),color="black") }

plot_curve <- function(prob_df=NULL){
  tmp <- prob_df 
  if(!is.null(prob_df)){ tmp <- mutate(tmp, p=prob_df$p) } 
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) + 
    geom_raster(show.legend = FALSE) + 
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) + 
    stat_contour(breaks=c(0.5),color="black") }
