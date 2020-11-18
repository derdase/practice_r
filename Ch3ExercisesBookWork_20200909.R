#########################################
############# CHAPTER 3 WORK ############
########### SEPTEMBER 9, 2020 ###########
#########################################

### In-Book Exercises:

library(readr)
library(dplyr)

## Exploring/Cleaning Data
beijing_pm_raw <- read_csv("data/Beijing_2017_HourlyPM25.csv",
                           skip = 3)

beijing_pm <- beijing_pm_raw %>%
              rename(sample_time = `Date (LST)`,
                     value = Value,
                     qc = `QC Name`) %>%
              select(sample_time, value, qc) %>%
              mutate(aqi = cut(value,
                               breaks = c(0, 50, 100, 150, 200, 300, 500, Inf),
                               labels = c("Good", "Moderate", "Unhealthy for some groups",
                                          "Unhealthy", "Very unhealthy", "Hazardous", "Beyond index")))

## Summarizing Numeric Vectors
beijing_pm$value # pulling out as vector
mean(x = beijing_pm$value) # inlcuding arguments to find mean of value

beijing_pm %>%
  select(value) # will select just the value column
  mean() # missing or NA due to value becoming one column dataframe

beijing_pm %>%
  pull(value) # will select the column value and send it back to vector class rather than having it as a one column dataframe
  mean() # able to calculate since value was sent back as a vector

# summarize() vs mutate() -->
# mutate() allows us to take columns and output the same number of rows; not compressed
# summarize() allows us to output a smaller dataframe in order to show summaries
  
beijing_pm %>%
  summarize(min_pm = min(value), # have missing values (-999; would want to change that)
            mean_pm = mean(value),
            max_pm = max(value)) %>%
  mutate(pm_range = max_pm - min_pm)
  
## Factor Vectors

  
  
