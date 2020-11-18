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

beijing_pm$aqi
levels(beijing_pm$aqi) # factors need levels --> will give levels of factor variable

beijing_pm %>%
  pull(aqi) %>%
  levels() # same thing as above, but being able to pipe through steps

beijing_pm %>%
  group_by(aqi) %>%
  count() # grouping by aqi and looking at count of those categories

beijing_pm %>%
  group_by(aqi) %>%
  summarize(mean_pm = mean(value),
            min_pm = min(value),
            max_pm = max(value),
            n = n())

# sometimes want to use character class that you would like to be a factor...
library(forcats)
beijing_pm %>%
  mutate(qc = as_factor(qc))

## Importing Data from R Packages
install.packages("faraway")
library(faraway)
data("worldcup")
head(worldcup)

data() # list of all packages able to be installed (can be lengthy)
data(package = "faraway") # list of datasets within library package
library() # list of packages you have installed and can use

## Dates in R
# often will need to mutate
install.packages("lubridate") # able to tell computer what order date elements want
library(lubridate)

beijing_pm %>%
  pull(sample_time) # to determine order of date
# this case has month-day-year-hour-minute

# Example
my_date <- ymd("2008-10-13")
class(my_date)

beijing_pm %>%
  mutate(sample_time = mdy_hm(sample_time))

beijing_pm %>%
  pull(sample_time) %>%
  class()

range(beijing_pm$sample_time_)
diff(range(beijing_pm$sample_time_)) # error

beijing_pm %>%
  pull(sample_time) %>%
  wday(label = TRUE) # prints out name of day

beijing_pm %>%
  mutate(sample_weekday = wday(sample_time, label = TRUE))

## Exercise

str(worldcup)
head(worldcup)
tail(worldcup)
colnames(worldcup)
summary(worldcup)

mean_world <- mean(worldcup$Saves)
mean_world

worldgoalies <- worldcup %>%
                  filter(Position=='Goalkeeper') %>%
                  summarize(mean_goal = mean(Saves))

worldcup %>%
  group_by(Position) %>%
  count()

worldcup %>%
  group_by(Position) %>%
  summarize(max_saves = max(Saves))

worldcup %>%
  filter(Position=='Forward') %>%
  group_by(Team) %>%
  summarize(n = n(),
            sum_shots = sum(Shots)) %>%
  mutate(mean_shots = sum_shots/n) %>%
  arrange(desc(sum_shots))

worldcup %>%
  filter(Position=='Defender') %>%
  group_by(Team) %>%
  summarize(max_shots = max(Tackles)) %>%
  arrange(desc(max_shots))

worldcup %>% 
  filter(Position == "Defender") %>% 
  top_n(n = 5, wt = Tackles)

# Exercise 3.8.3  
# Minimum time
arrange(worldcup, Time) %>% 
  select(Time) %>% 
  slice(1)

# Maximum time
arrange(worldcup, desc(Time)) %>% 
  select(Time) %>% 
  slice(1)

arrange(worldcup, Time) %>% 
  select(Time) %>% 
  slice(c(1, n()))

range(worldcup$Time)

worldcup %>%
  filter(Time == 570)

worldcup %>% 
  top_n(n = 1, wt = Time)

library(tibble)
worldcup %>%
  rownames_to_column(var = "Name") %>% 
  filter(Time == 570)

is_goalie <- worldcup$Position == "Goalkeeper"
sum(is_goalie)

worldcup %>% 
  filter(Position == "Goalkeeper") %>% 
  nrow()

brazil_players <- worldcup %>% 
  filter(Team == "Brazil" & Position != "Goalkeeper") 
head(brazil_players)

# Exercise 3.8.4
library(ggplot2)
head(worldcup)

# W/o piping
ggplot(data = worldcup) + geom_point(aes(x=Time, y=Passes), color="blue")

# W/ piping
worldcup %>% 
  ggplot() +
  geom_point(aes(x=Time, y=Passes))

ggplot(worldcup, mapping = aes(x=Time, y=Passes, color=Position)) +
  geom_point() +
  geom_rug()

worldcup1 <- worldcup %>%
                filter(Team %in% c("Spain", "Netherlands", "Germany", "Uruguay")) %>%
                ggplot() +
                geom_point(aes(x=Shots, y=Tackles, color=Position, shape = Team))

worldcup1

worldcup2 <- worldcup %>%
                mutate(top_4 = Team %in% c("Spain", "Netherlands", "Germany", "Uruguay")) %>%
                ggplot() +
                geom_point(aes(x=Time, y=Passes, color = top_4)) +
                geom_label()
worldcup2
                
data()

# Exercise 3.8.6

ggplot(worldcup, aes(x = Time)) + 
  geom_histogram(bins = 50, binwidth = 10, color = "blue", fill = "light blue")

ggplot()





