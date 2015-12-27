library(ggplot2)
library(dplyr)
library(RSQLite)

setwd("C:/Kaggle-collegescorecard/")
my_db <- src_sqlite("database.sqlite")
src_tbls(my_db)
tbl <- tbl(my_db, "Scorecard")

#building a smaller working set from the database for exploration

smaller <- sample(1:124699, 5000)
small_tbl <- tbl %>% filter(rowid %in% smaller)

#start data wrangling

#first step is to make the column names more legible using the data dictionary provided

data_dictionary <- read.csv("CollegeScorecardDataDictionary-09-12-2015.csv")

dim(data_dictionary)
#has more rows than there are variables in the scorecard table

#not sure what causes this, need to write a more robust method to rename
#let's look for matches in the current names cross referenced with the data dictionary
#replace those we have a match for

#in every row where a variable name occurs in the data_dictionary
#with a corresponding developer.friendly.name we can take that

current_colnames <- colnames(small_tbl)

for(i in 1:length(current_colnames)) {
  location <- which(data_dictionary$VARIABLE.NAME == current_colnames[i])
  if(length(location) == 0) {next}
  new_name <- as.character(data_dictionary[location, 4])
  print(i)
  if(new_name != "") {current_colnames[i] <- new_name}
}

head(current_colnames)

#insofar as I can tell, this usual assignment doesn't work
#perhaps has to do with the fact that it's a database
#however rename works
dimnames(small_tbl) <- current_colnames
dimnames(small_tbl)

updated_colnames <- current_colnames
current_colnames <- colnames(small_tbl)
new_name_list <- ""

new_test <- sapply(updated_colnames, sub, pattern = "-", replacement="to")
new_test <- as.character(new_test)

test_rename <- small_tbl

for(i in 1:length(current_colnames)) {
  if(i < length(current_colnames) && i %% 150 != 0) {
    new_name_list <- paste(new_name_list, new_test[i], "=", current_colnames[i], ", ")
  }
  else  {
    new_name_list <- paste(new_name_list, new_test[i], "=", current_colnames[i])
  }
  print(i)
  if(i %% 150 == 0) {
    Encoding(new_name_list) <- "UTF-8"
    eval(parse(text = 
                 paste("test_rename <- rename(small_tbl,", new_name_list, ") ")
    )) 
    new_name_list <- ""
  }
}


for(i in 1:length(current_colnames)) {
  print(i)
  eval(parse(text = 
               paste("test_rename <- rename(small_tbl,", updated_colnames[i], "=", current_colnames[i], ") ")
               )) 
}

eval(parse(text = 
             paste("test_rename <- rename(small_tbl,",
                   substr(new_name_list, 1, 43), ") ")
             ))

#([^,]+,){1,10}
#eval
#assign

#start paring down variables, too many to deal with for now

#some initial exploratory graphs

typeof(tbl)

test <- collect(select(small_tbl, state = STABBR))
ggplot(aes(x = STABBR), data = test) +
  geom_histogram()

small_tbl collect(small_tbl)

colnames(test) <- current_colnames

earnings <- collect(select(small_tbl, q50earnings = md_earn_wne_p10, 
                                      q10earnings = pct10_earn_wne_p10,
                                      q25earnings = pct25_earn_wne_p10,
                                      q75earnings = pct75_earn_wne_p10,
                                      q90earnings = pct90_earn_wne_p10))

year_2011 <- filter(tbl, Year == 2011)
year_2011 <- collect(year_2011)

year_2013 <- filter(tbl, Year == 2013)
year_2013 <- collect(year_2013)

earnings_2011 <- select(year_2011, q50earnings = md_earn_wne_p10, 
                                   q10earnings = pct10_earn_wne_p10,
                                   q25earnings = pct25_earn_wne_p10,
                                   q75earnings = pct75_earn_wne_p10,
                                   q90earnings = pct90_earn_wne_p10)

states_2011 <- select(year_2011, states = STABBR)

schools_2011 <- select(year_2011, school_name = INSTNM)

carnegie_class_2013 <- select(year_2013, carnegie_class = CCBASIC)
#filter out all special vocation schools
filter(carnegie_class_2013, !grepl("special", ignore.case = TRUE, carnegie_class))

cbind(schools_2011, states_2011)

schools_earnings_2011 <- cbind(schools_2011, earnings_2011)
schools_earnings_2011 <- schools_earnings_2011 %>% filter(!is.na(q50earnings), q50earnings != 0)
s_e_11 <- schools_earnings_2011
s_e_11 <- s_e_11 %>% filter(q10earnings != 0)
s_e_11 <- s_e_11 %>% arrange(desc(q50earnings))

ggplot(s_e_11[1:20,], aes(x = school_name, ymin = q10earnings, lower = q25earnings, middle = q50earnings, upper= q75earnings, ymax = q90earnings)) +
  geom_boxplot(stat="identity") +
  coord_flip()

#ymin=e10, lower=e25, middle=e50, upper=e75, ymax=e90
#Some things to explore:
#correlation between ACT/SAT test scores and earnings

