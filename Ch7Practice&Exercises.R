################################################################################
######################## Chapter 7 Practice/Exercises ##########################
############################## Exploring Data 2 ################################
################################################################################

### Videos

## Matrices

remotes::update_packages("rlang")

library(tidyverse)
library(faraway)
data("worldcup")

ex_matrix <- worldcup %>%
  select(Time:Passes) %>%
  as.matrix() # all columns need to be same type -> R has coerced everything into character type

ex_matrix[1:2, 3] # first 2 rows and 3rd column

ex_matrix[1:2, c(1,3)] # first and 3rd column; might have put it back into vector

worldcup %>%
  as.matrix() %>% # cannot use dplyr package on matrices
  select(Time:Passes) %>%
  slice(1:2) %>%
  select(c(1,3))

## Lists - allow object to stay as object

install.packages("listviewer")
library(listviewer)

a <- list(element_1 = sample(1:10, 5),
          element_2 = tibble(letters = letters[1:3],
                            numbers = 1:3))
a

a_list <- list(list("a","b"), list(1,2))
str(a_list)

jsonedit(a) # can use when you have loads of data where str couldn't explore

## Simple Stat Tests in R

normal_ex_vector <- rnorm(1000, mean = 200, sd = 50)

qplot(normal_ex_vector, geom = "histogram")

shapiro.test(normal_ex_vector) # p-value is showing that it has little evidence that
# distribution is different than normal

ex_sw_result <- shapiro.test(normal_ex_vector)
class(ex_sw_result) # htest specific to family of tests
is.list(ex_sw_result) # to see if list
str(ex_sw_result) # structure of data to look what is tucked into data

library(broom) # will search to see what object you have, but will allow to clean up
tidy(ex_sw_result) # tidy version of what's contained in that test

# can also pipe
normal_ex_vector %>%
  shapiro.test() %>%
  tidy()

## Simple Stat in Tidy Framework

normal_ex_df <- tibble(norm_vector = normal_ex_vector)

ggplot(normal_ex_df, aes(x = norm_vector)) +
  geom_histogram()

normal_ex_df %>%
  pull("norm_vector") %>% # able to pull column 
  shapiro.test() %>%
  tidy()

if(!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("microbiome")
library(microbiome)

data("atlas1006") # genomics class phyloseq

atlas1006 %>% 
  get_variable() %>%
  filter(time == 0) %>%
  ggplot(aes(x= diversity)) + geom_histogram()

## Regression Modeling
library(tidyverse)
library(broom)
library(ggfortify)

library(faraway)
data(worldcup)
worldcup <- worldcup %>%
  select(Time, Tackles, Position)

ggplot(worldcup, aes(x = Time, y = Tackles)) +
  geom_point()

tackle_model <- lm(Tackles ~ Time, data = worldcup)
# gives tackle explained by time from dataset worldcup
tackle_model
class(tackle_model)
is.list(tackle_model) # special type of list
str(tackle_model)

glance(tackle_model) # giving us stats model-fit wide; usually one row

tidy(tackle_model) # one row per variable plus one for intercept

augment(tackle_model) # pulls info from each observation; tacks it onto original dataset
# useful for plotting
augment(tackle_model) %>%
  ggplot(aes(x = Time)) +
  geom_point(aes(y = Tackles)) +
  geom_line(aes(y = .fitted), color = "red")

# diagnostic plots based on model fit to see if there are problems in validity of assumptions
tackle_model %>%
  autoplot(tackle_model)

position_model <- lm(Tackles ~ Position, data = worldcup)
position_model # gives diff levels representing positions

tidy(position_model)
# estimate of forward is the difference estimate of the position defender (intercept)

## Generalized Linear Models (glm)
# Diff than lm by fitting model differently

worldcup %>%
  select(Time, Shots, Tackles, Position)

tm_2 <- glm(Tackles ~ Time, data = worldcup)

tm_3 <- glm(Tackles ~ Time, data = worldcup,
    family = poisson(link = "log")) %>%
  tidy()

tm_3 %>%
  augment() %>%
  mutate(.fitted = exp(.fitted)) %>%
  ggplot(aes(x = Time, y = Tackles)) +
  geom_point +
  geom_line(aes(y = .fitted), color = "red", size = 1.2)

glm(Tackles ~ Time:Shots, data = worldcup,
    family = poisson(link = "log")) %>%
  tidy()

## Finding and Selecting R Packages - 10 Simple Rules

# 1. Consider your purpose.
# 2. Find and collect options.
# 3. Check how it's shared.
# 4. Explore availability and quality of help.
# 5. Quantify how established it is.
# 6. Seek evidence of peer acceptance and review.
# 7. Find out who developed it.
# 8. See how it's developed.
# 9. Put it to the test.
# 10. Develop your own package.

# can use diff distributions --> linear, logistic, poisson

## Exploring Data 2: Nesting and Mapping

library(tidyverse) # dplyr, broom, etc. to tidy up data
library(microbiome) # dataset
data("atlas1006")

atlas_sample_data <- atlas1006 %>%
  get_sample() # accessor function from microbiome package
# class is matrix - want to make into dataframe

atlas_sample_data[1:6, 1:6] # able to use brackets in matrices in order to pull out data
# this will pull out first 6 rows and columns

# Need to:

tidy_sample <- atlas_sample_data %>% 
  as.data.frame() %>% # change to data frame
  rownames_to_column(var = "species") %>% # move row names into column
  pivot_longer(-species, 
               names_to = "sample",
               values_to = "prevalence") # pivot longer so that column names in their own column values

## Intro to Nesting and Mapping

# using above example tidy_sample
library(purrr)
library(broom)

tidy_sample %>%
  group_by(species) %>%
  summarize(mean_prev = mean(prevalence)) %>%
  arrange(desc(mean_prev))

allistipes <- tidy_sample %>%
  filter(species == "Allistipes et rel.")

allistipes %>%
  pull(prevalence) %>%
  mean()

tidy_sample %>%
  group_by(species) %>%
  nest() # produces list column; tibble tucked into list showing info about prevelance about each sample
# nested dataframe

## More on Nesting and Mapping
# continue from last example
# want to do same "recipe" for more then one elements

nested_sample <- tidy_sample %>%
  group_by(species) %>%
  nest()

nested2 <- nested_sample %>%
  mutate(mean_prev = map(data,
                         ~ .x %>%
                           pull(prevalence) %>%
                           mean()))
# allows us to take a column and go across each element and take recipe to apply to each element 

nested2$mean_prev[1]
nested2$mean_prev[6] # class is list
nested2$mean_prev[[6]] # class is numeric (vector)
nested_sample$data[[1]] %>% pull(prevalence) %>% mean()

# unnesting
nested2 %>%
  unnest(cols = mean_prev)

# Shapiro Test

allistipes %>%
  pull(prevalence) %>%
  shapiro.test() %>%
  tidy()

nested2 <- nested_sample %>%
  mutate(shapiro = map(data,
                         ~ .x %>%
                           pull(prevalence) %>%
                           shapiro.test() %>%
                           tidy()))

nested2 %>%
  unnest(cols = shapiro) %>%
  ggplot(aes(x = log10(p.value))) +
  geom_histogram()

## Functions

number <- 6
number + 1

add_one <- function(number){
  new_number <- number + 1
  return(new_number) # tells which object you want returned; explicit
}

add_one(number = 1:3)

library(faraway)
library(tidyverse)

data(worldcup)
worldcup

df <- worldcup %>% filter(Team == "France")
lm(Tackles ~ Time + Position, data = worldcup)

fit_time_pos_mod <- function(df = worldcup){
  lm(Tackles ~ Time + Position, data = df)
}

library(broom)

worldcup %>%
  filter(Team %in% c("France", "Spain")) %>%
  fit_time_pos_mod() %>%
  tidy()

add_one <- function(number = 1){
  number + 1
}

add_one() # missing default value, but when add "=1" as default, will output 2

hello_world <- function(){
  print("Hello world!")
}

hello_world()

add_two_numbers <- function(first_number, second_number = 1){
  first_number_new <- first_number * 1
  first_number + second_number
}

add_two_numbers(first_number = 5)

# if/else functions

cat("Today's date is:")
cat(format(Sys.Date(), "%b. %d, %Y"))

tell_date <- function(){
  cat("Today's date is: ")
  cat(format(Sys.Date(), "%b. %d, %Y"))
  
  todays_wday <- lubridate::wday(Sys.time(),
                                 label=TRUE)
  if(todays_wday %in% c("Sat", "Sun")){
    cat("\nIt's the weekend!")
  }
}

tell_date()

tell_date2 <- function(){
  cat("Today's date is: ")
  cat(format(Sys.Date(), "%b. %d, %Y"))
  
  todays_wday <- lubridate::wday(Sys.time())
  if(todays_wday %in% c(1, 7)){
    cat("\nIt's the weekend!")
  } else if (todays_wday == c(6)){
    cat("It's almost the weekend!")
  } else {
    cat("It's ", 7 - todays_wday, "days until the weekend.")
  }
}

tell_date2()

# Using functions to map across vectors and lists

library(purrr)
library(tidyverse)
# include map, map_2, and pmap

a_list <- list(first_thing = 1:3,
               second_thing = c(32, 14, 5, 8))

range(1:3) # gives min and max
range(32, 14, 5, 8)

map(.x = a_list,
    .f = range)

a_df <- tibble(element = c("first", "first", "first",
                           "second", "second", "second"),
               measure = c(1,2,3,11,15,20))

nested_df <- a_df %>%
  group_by(element) %>%
  nest()

nested_df$data[[1]]

nested_df %>%
  mutate(num_range = map(data, .f = range)) %>%
  unnest(cols = num_range)

first_word <- c("open", "ride", "moot")
second_word <- c("source", "share", "point")

map2(.x = first_word,
     .y = second_word,
     .f = paste)

### Book Practice

library(readr)
library(dplyr)
clark_co_accidents <- read_csv("data/accident.csv") %>% 
  filter(STATE == 32 & COUNTY == 3)

clark_co_accidents %>% 
  count()

library(tidyr)
library(lubridate)
clark_co_accidents <- clark_co_accidents %>% 
  select(DAY, MONTH, YEAR) %>% 
  unite(date, DAY, MONTH, YEAR, sep = "-") %>% 
  mutate(date = dmy(date))

clark_co_accidents %>% 
  slice(1:5)

clark_co_accidents <- clark_co_accidents %>% 
  group_by(date) %>% 
  count() %>% 
  ungroup()
clark_co_accidents %>% 
  slice(1:3)

all_dates <- tibble(date = seq(ymd("2016-01-01"), 
                               ymd("2016-12-31"), by = 1))
all_dates %>% 
  slice(1:5)

clark_co_accidents <- clark_co_accidents %>% 
  right_join(all_dates, by = "date") %>% 
  # If `n` is missing, set to 0. Otherwise keep value.
  mutate(n = ifelse(is.na(n), 0, n))
clark_co_accidents %>% 
  slice(1:3)

clark_co_accidents <- clark_co_accidents %>% 
  mutate(weekday = wday(date, label = TRUE), 
         weekend = weekday %in% c("Fri", "Sat"))
clark_co_accidents %>% 
  slice(1:3)

clark_co_accidents <- clark_co_accidents %>% 
  mutate(any_crash = n > 0)
crash_prob <- clark_co_accidents %>% 
  group_by(weekend) %>% 
  summarize(n_days = n(),
            crash_days = sum(any_crash)) %>% 
  mutate(prob_crash_day = crash_days / n_days)

prop.test(x = crash_prob$crash_days, 
          n = crash_prob$n_days)

vegas_test <- prop.test(x = crash_prob$crash_days, 
                        n = crash_prob$n_days)
is.list(vegas_test)

# Exercise 7.8.2

library(babynames)
library(stringr)

top_names <- babynames %>%
  mutate(first_letter = str_sub(name, 1, 1))
  
top_names %>%
  select(name, first_letter) %>%
  slice(1:5)

top_names <- top_names %>% 
  group_by(first_letter) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))

library(ggplot2)

top_names %>% 
  mutate(first_letter = fct_reorder(first_letter, prop)) %>%
  ggplot(aes(x = first_letter)) + 
  geom_bar(aes(weight=prop)) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(x = "First Letter", y = "Percent of names that start with...")

class <- data.frame(name = c("Kailee", "Sarah", "Brooke",
                             "Shabana", "Becky", "Valerie",
                             "Tyler", "Sara", "Evan",
                             "Kellin"))

class <- class %>%
  mutate(first_letter = str_sub(name, 1, 1))

class <- class %>% 
  group_by(first_letter) %>%
  count() %>% 
  ungroup() %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))

class %>% 
  mutate(first_letter = fct_reorder(first_letter, prop)) %>%
  ggplot(aes(x = first_letter)) + 
  geom_bar(aes(weight=prop)) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(x = "First Letter", y = "Percent of names that start with...")

cs <- babynames %>%
  mutate(first_letter = str_detect(name, "^[CS]"))
  
# Exercise 7.4

install.packages("dlnm")
library(dlnm)
data(chicagoNMMAPS)

chic <- chicagoNMMAPS
head(chic)

mod_1 <- lm(dptp ~ temp, data = chic)
mod_1
summary(mod_1)

tidy(mod_1)
glance(mod_1)

augment(mod_1) %>%
ggplot(chic, aes(x = temp, y = dptp)) +
  geom_point()

augment(mod_1) %>%
  ggplot(aes(x = temp)) +
  geom_point(aes(y = dptp)) +
  geom_line(aes(y = .fitted), color = "red")

install.packages("ggfortify")
library(ggfortify)

autoplot(mod_1)
plot(mod_1)

mod_2 <- glm(dptp ~ temp, data = chic)
mod_2

tidy(mod_2)

mod_3 <- lm(pm10 ~ dow, data = chic)
mod_3

tidy(mod_3)

anova(mod_3)

library(lubridate)
summer <- chic %>%
  filter(month %in% c(6, 7, 8))

mod_4 <- glm(resp ~ temp, family = poisson(link = "log"), data = summer)
mod_4
tidy(mod_4)

# Exercise 7.8.6

install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
data("flights")

flights <- flights %>%
  select(dep_delay, carrier, hour, origin) %>%
  filter(origin == "LGA")

flights <- flights %>%
  filter(!is.na(dep_delay)) %>%
  mutate(late_dep = dep_delay >= 15)

# Proportion of 15 mins late?

flights %>% 
  pull("late_dep") %>%
  mean()

flights_late <- flights %>%
  group_by(hour) %>%
  summarize(prop_late = mean(late_dep))

# plot
library(ggplot2)
library(scales)
library(broom)

ggplot(flights_late, mapping = aes(x = hour, y = prop_late)) +
  geom_line() +
  scale_y_continuous(labels = percent)

tidy(glm(late_dep ~ hour, data = flights, family = binomial(link = "logit"))) %>%
  mutate(OR = exp(estimate))

nested_flights <- flights %>%
  group_by(carrier) %>%
  nest()

library(purrr)
prob_late <- nested_flights %>% 
  mutate(glm_result = purrr::map(data, ~ glm(late_dep ~ hour, 
                                             data = .x, family = binomial(link = "logit"))))

prob_late <- prob_late %>% 
  mutate(glm_tidy = purrr::map(glm_result, ~ tidy(.x)))
prob_late <- prob_late %>% 
  select(-data, -glm_result)
prob_late <- prob_late %>% 
  unnest(glm_tidy)
prob_late <- prob_late %>% 
  filter(term == "hour")
prob_late <- prob_late %>% 
  mutate(or = exp(estimate))
head(prob_late)
prob_late <- left_join(prob_late, airlines, by = "carrier")

library(forcats)
data(airlines)
prob_late %>% 
  ungroup() %>% 
  mutate(estimate = exp(estimate)) %>% 
  mutate(name = fct_reorder(name, estimate)) %>% 
  ggplot(aes(x = estimate, y = name)) + 
  geom_point() + 
  geom_vline(xintercept = 1, linetype = 3) + 
  labs(x = "Odds ratio for one-hour increase in scheduled deparatu]re time", y = "")

# Exercise 7.8.7

move_letter <- function(word){
  last_letter <- str_sub(word, 4, 4)
  first_part <- str_sub(word, 1, 3)
  new_word <- paste(last_letter, first_part, sep = "")
  return(new_word)
}

move_letter(word = c("cats", "dogs"))
tibble(oldword = c("cats", "dogs")) %>%
  mutate(new_word = move_letter(word = oldword))

word_list <- read_tsv("https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt", 
                      col_names = "word") %>%
  rename(word = a)

str_detect(new_word, word_list)



