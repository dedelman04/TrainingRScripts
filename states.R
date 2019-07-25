library(tidyverse)
library(googlesheets)

gap <- gs_title("States")
gs_ws_ls(gap)

states <- gap %>% gs_read(ws="Sheet2")

states %>% group_by(Plate, Sighted, Type) %>% summarize(cnt = n()) %>%
  ggplot(aes(x=cnt)) + geom_histogram(binwidth = 1) + facet_wrap(Sighted ~ Type, ncol = 2)

states %>% group_by(Plate, Sighted, Type) %>% summarize(cnt = n()) %>%
  ggplot(aes(x=Sighted, y=cnt)) + geom_boxplot() + facet_wrap( ~ Type, ncol = 1)

states %>% group_by(Plate, Sighted, Type) %>% summarize(cnt = n()) %>%
  ggplot(aes(x=Type, y=cnt)) + geom_boxplot() + facet_wrap( ~ Sighted, ncol = 2)

