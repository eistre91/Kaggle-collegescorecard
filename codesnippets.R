slim_2011_programs <- select(slim_2011, X10_yrs_after_entry.median, contains("program_percentage"))
slim_programs <- select(slim_2011_programs, -X10_yrs_after_entry.median)

cor(slim_2011_programs, use = "complete.obs")[1,]
#a recognizable program contribution?
#how do I visualize this relationship?

#Make 2007 and 2009 training years, 2011 test set
slim_slice <- filter(slim, Year %in% c(2007, 2009, 2011))

for(id in 1:nrow(carnegie)) {
  slim_slice$carnegie_basic[slim_slice$name %in% carnegie$name[id]] <- carnegie$carnegie_basic[id]
}

#filter out all special vocation schools
filter(carnegie_class_2013, !grepl("special", ignore.case = TRUE, carnegie_class))

slim_slice_formodel <- select(slim_slice, -starts_with("pct"), -state_fips, -region_id, -locale, -admission_rate.by_ope_id, 
                              -sat_scores.25th_percentile.critical_reading, -sat_scores.75th_percentile.critical_reading, -sat_scores.25th_percentile.math, -sat_scores.75th_percentile.math, -sat_scores.25th_percentile.writing, -sat_scores.75th_percentile.writing, -sat_scores.average.by_ope_id, -act_scores.25th_percentile.cumulative, -act_scores.75th_percentile.cumulative, -act_scores.25th_percentile.english, -act_scores.75th_percentile.english, -act_scores.25th_percentile.math, -act_scores.75th_percentile.math, -act_scores.25th_percentile.writing, -act_scores.75th_percentile.writing, 
                              -contains("program"), 
                              -C150_L4_POOLED_SUPP, -rate_suppressed.four_year, -rate_suppressed.lt_four_year, -C200_4_POOLED_SUPP, 
                              -degree_urbanization, -degrees_awarded.highest, 
                              -loan_principal, -median_debt.completers.overall, -median_debt.noncompleters, -cumulative_debt.90th_percentile, -cumulative_debt.75th_percentile, -cumulative_debt.25th_percentile, -cumulative_debt.10th_percentile, -retention_rate.four_year.full_time, -retention_rate.lt_four_year.full_time, -retention_rate.four_year.part_time, -retention_rate.lt_four_year.part_time,
                              -attendance.academic_year, -enrollment.all)

slim_slice_formodel <- filter(slim_slice_formodel, X10_yrs_after_entry.median > 0)
slim_slice_formodel <- filter(slim_slice_formodel, operating != "Not currently certified as an operating institution")
slim_slice_formodel <- select(slim_slice_formodel, -operating)
slim_slice_formodel <- select(slim_slice_formodel, -city, -name)
slim_slice_formodel

for(i in 1:ncol(slim_slice_formodel)) { 
  if(typeof(slim_slice_formodel[[i]]) == "character") {
    slim_slice_formodel[[i]] <- as.factor(slim_slice_formodel[[i]])
  }
}

filter(slim_slice_formodel, !is.na(act_scores.midpoint.cumulative) | !is.na(sat_scores.average.overall))
#less than 1/3 of schools at this point in the data wrangling have reported (act or sat) scores
#should maybe try converting scores to a uniform value
by_sat <- select(slim_slice_formodel, -contains("act"), -sat_scores.average.overall)
by_act <- select(slim_slice_formodel, -contains("sat"), -act_scores.midpoint.cumulative)

#remove scores for now due to so many NAs
no_scores <- select(slim_slice_formodel, -contains("act"), -contains("sat"))

for_cor <- select(by_sat, -state, -ownership, -carnegie_basic, -degrees_awarded.predominant, -online_only) 
cor( for_cor , use = "complete.obs")

colnames(no_scores)[unlist(lapply(no_scores, function(x) any(is.na(x))))]

#lm with na.action = na.omit

no_scores_train <- filter(no_scores, Year == 2007 | Year == 2009)
no_scores_test <- filter(no_scores, Year == 2011)

no_scores_train <- select(no_scores_train, -Year)
no_scores_test <- select(no_scores_test, -Year)
model1 <- lm(X10_yrs_after_entry.median ~ ., data = no_scores_train, na.action = na.omit)

#the recession may throw a wrench into the analysis, but it any disparate effects should disappear over the large dataset

sat_earnings <- slim_2011 %>%
  select(Year, earnings = X10_yrs_after_entry.median, sat = sat_scores.average.overall) %>%
  filter(!is.na(sat), sat > 0)

ggplot(aes(x = state), data = slim_2011) +
  geom_boxplot(aes(lower = pct25_earn_wne_p10, 
                   y = X10_yrs_after_entry.median, 
                   upper = pct75_earn_wne_p10)) +
  scale_y_continuous(limits=c(0, 100000))

ggplot(aes(x = sat, y = earnings), data = sat_earnings) +
  geom_point() +
  scale_x_continuous(breaks=seq(710,1520,50))