library(foreign)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)

mobility <- read.spss("Oct08 Mobility_cleaned.sav", to.data.frame = TRUE)
mobility <- tbl_df(mobility)
mobility_state <- select(mobility, sex, q9:q20, educ, age)

mobility_state <- filter(mobility_state, !is.na(q13))

mobility_state_college <- filter(mobility_state, educ %in% c("Some college, no 4-year degree (including associate degree)",
                                                             "College graduate (B.S., B.A., or other 4-year degree)",
                                                             "Post-graduate training or professional schooling after college (e.g., toward a Master's degree or Ph.D.; law or medical "),
                                 !is.na(q13))

ggplot(mobility_state, aes(x=q13, color=q13, fill=q13)) +
  geom_bar(stat="count") +
  scale_color_colorblind() +
  scale_fill_colorblind()

table(mobility_state$q13)
1067/(1067+440)

table(mobility_state_college$q13)
773/(773+237)

#so there's a large proportion of people who have lived in a different state at some point
#it would be nice to be more granular. who has stayed in the state where they went to university?

#q11a-k
ggplot(mobility_state, aes(x=q11d)) +
  geom_bar(stat="count")

table(mobility_state$q11d)
table(mobility_state_college$q11d)

ggplot(mobility_state_college, aes(x=q11d)) +
  geom_bar(stat="count")

#does college predict greater mobility
#go where the jobs are, but not necessarily, 
#too complex to really sort this out on this data set
