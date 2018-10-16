####
#gather() function
#first arg names column with that will have the column header (wide data) names as its value
#second arg names column that will contain cell values
#third arg column names that have the 1st arg values

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

new_tidy_data <- wide_data %>% gather(year, fertility, `1960`:`2015`)

#can also specify which columns *not* to gather
new_tidy_data2 <- wide_data %>% gather(year, fertility, -country)

#note that gather assumes column headers are always characters
# convert = TRUE converts to correct datatype
new_tidy_data <- wide_data %>% gather(year, fertility, -country, convert=TRUE)

###
#This is here if you use read.csv instead of read_csv
#new_tidy_data$year <- as.integer(sub("^X", "", x=new_tidy_data$year))
###

####
#spread() - inverse of gather
#first arg - specify column headers
#second arg - specify data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

####
#separate()
#used e.g. when variable type is encoded in column headers
#splits column (1st arg) on a delimiter (3rd arg) and puts into new columns (2nd arg)
# "_" is default delimiter
####

filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

dat <- raw_dat %>% gather(key, value, -country)
head(dat)

dat %>% separate(key, c("year", "variable_name"), "_")
#note that life and expectancy are also separated by _
#Need a third column and arg on how to treat missing values (fill argument)
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill="right")

#argument "extra" will tell separate to merge when multiple delimiters (don't need extra column)
dat %>% separate(key, c("year", "variable_name"), extra="merge")

#now spread the data
dat %>% separate(key, c("year", "variable_name"), extra="merge") %>%
  spread(variable_name, value)

#separate then unite then spread
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill="right") %>%
  unite(variable_name, first_variable_name:second_variable_name) %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

##########
#####Joining tables
##########

###join types
data(murders)
data(polls_us_election_2016)
tab <- left_join(murders, results_us_election_2016, by="state")
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)

####left_join
#All rows in tab1, NAs in missing tab2 data
left_join(tab1, tab2)
tab1 %>% left_join(tab2)  #Also can do pipe notation

####right_join
#All rows in tab2, NAs in missing tab1 data
tab1 %>% right_join(tab2)

####inner_join
#Only rows that exist in both tables
tab1 %>% inner_join(tab2)

####full_join
#All rows in all both tables with NA where missing
full_join(tab1, tab2)

####semi_join
#keeps parts of tab1 where there is info in tab2
##does not add columns from tab2##
semi_join(tab1, tab2)

####anti_join
#keeps parts of tab1 where there is *no* info in tab2
##does not add columns from tab2##
anti_join(tab1, tab2)

#########
####Binding
#########

####bind_cols
#Puts columns of each object into a single tibble
#does not match on any variable - order of original objects is maintained
bind_cols(a = 1:3, b=4:6)

#if dimension doesn't match, error thrown
bind_cols(a = 1:3, b=4:7)

#can also bind data frames together
tab1 <- tab[, 1:3] 
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

####bind_rows
#Same as bind_cols() but on rows
tab1 <- tab[1:2, ]
tab2 <- tab[3:4, ]
bind_rows(tab1, tab2)

#cbind(), rbind() do same function, but create matrix or data frame, not tibble

########
####Set Operators
########

####intersect
#shows elements on both object
#dplyr allows for use on tables
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

####union
#shows all elements with no duplicates
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

####setdiff
#shows elements in tab2 not in tab 1
#***NOTE*** this is not symmetric; setdiff(tab1, tab2) <> setdiff(tab2, tab1)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)
setdiff(tab2, tab1)

####setequal
#are two objects equivalent, regardless of order
setequal(1:5, 1:6)  #FALSE
setequal(1:5, 5:1)  #TRUE
#with dplyr, provides information on what is different between tab1 and tab2
setequal(tab1, tab2)

################
####Web scraping
################

###requires rvest library
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
##read in all html
h <- read_html(url)

##find all instances of node type table
##html_node() gets first instance only
tab <- h %>% html_nodes("table")
tab <- tab[[2]]

#convert to data frame
tab <- tab %>% html_table
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))


#FB Perspective 1st down rates
url2 <- "http://www.footballperspective.com/eli-manning-deshaun-watson-and-first-down-rates/"
h2 <- read_html(url2)

firstdown <- h2 %>% html_nodes("table")
firstdown <- firstdown[[1]] %>% html_table

