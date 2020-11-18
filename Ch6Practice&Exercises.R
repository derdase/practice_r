################################################################################
######################## Chapter 6 Practice/Exercises ##########################
################################################################################

### Ch. 6 Practice

## Joining Datasets
x <- data.frame(course = c("x", "y", "z"),
                grade = c(90, 82, 78))
y <- data.frame(course = c("w", "x", "y"),
                day = c("Tues", "Mon / Fri", "Tue"))

library(tidyverse)
inner_join(x, y)
left_join(x, y)
right_join(x, y)
full_join(x, y)

class_grades <- tibble(course = c("x", "x", "y", "z"),
                       grade = c(92, 90, 82, 78),
                       student = c("a", "b", "a", "b"))

class_days <- tibble(class = c("w", "x", "x", "y"),
                     day = c("Tues", "Mon / Fri", "Mon / Fri", "Tues"),
                             student = c("a", "a", "b", "a"))
full_join(class_grades, class_days, 
          by = list(x = c("course","student"), 
                    y = c("class","student")))

## Pivoting Longer and Wider
library(tidyverse)
library(dplyr)
install.packages("tidyr")
library(tidyr)
library(ggplot2)
hogwarts_wide <- tibble(student = c("Harry", "Hermione"),
                        math = c(89, 100),
                        english = c(92, 99),
                        science = c(93, 98))
pivot_longer(data = hogwarts_wide,
             col = math:science,
             names_to = "subject",
             values_to = "grades") %>%
ggplot(aes(x = student, y = grades, color = subject)) + geom_point()

## Exercise 6.7.1
# No code. Discuss.

## Exercise 6.7.2

library(dplyr)
library(gridExtra)
library(ggthemes)

url1 <- paste0("https://raw.githubusercontent.com/geanders/RProgrammingForResearch/master/data/country_timeseries.csv")
country_ts <- read_csv(file = url1)
head(country_ts)

url2 <- paste0("https://raw.githubusercontent.com/geanders/RProgrammingForResearch/master/data/mexico_deaths.csv")
url3 <- paste0("https://raw.githubusercontent.com/geanders/RProgrammingForResearch/master/data/mexico_exposure.csv")

mex_deaths <- read_csv(url2)
head(mex_deaths)

mex_exp <- read_csv(url3)
head(mex_exp)

## Exercise 6.7.3

library(tidyverse)
library(lubridate)

country_ts <- country_ts %>%
  mutate(Date = mdy(Date))
head(country_ts)

country <- country_ts %>%
  pivot_longer(col = Cases_Guinea:Deaths_Mali,
              names_to = "country_stats",
              values_to = "country_values") %>%
  separate(col = country_stats,
          into = c("Type", "Country"),
          sep = "_") %>%
  pivot_wider(names_from = "Type",
              values_from = "country_values") %>%
  filter(!is.na(Cases) & !is.na(Deaths))

ggplot(country, aes(x = Date, y = Cases)) + 
  geom_line() + 
  facet_wrap(~ Country, ncol = 4) + 
  theme_classic()

ggplot(country, aes(x = Date, y = Deaths)) + 
  geom_line() + 
  facet_wrap(~ Country, ncol = 4) + 
  theme_classic()

ggplot(country, aes(x = Date, y = Cases)) + 
  geom_line() + 
  facet_wrap(~ Country, ncol = 4, scales = "free_y") + 
  theme_classic()

# Using Example Code:

ebola <- read_csv(url1) %>%
  pivot_longer(cols = c(-Date, -Day), names_to = "variable", values_to = "count") %>%
  mutate(Date = mdy(Date)) %>%
  separate(variable, c("type", "country"), sep = "_") %>%
  pivot_wider(names_from = type, values_from = count) %>%
  filter(!is.na(Cases) & !is.na(Deaths))

ggplot(ebola, aes(x = Date, y = Cases)) + 
  geom_line() + 
  facet_wrap(~ country, ncol = 4) + 
  theme_classic()

ggplot(ebola, aes(x = Date, y = Deaths)) + 
  geom_line() + 
  facet_wrap(~ country, ncol = 4) + 
  theme_classic()

library(forcats)
ggplot(ebola, aes(x = Date, y = Cases)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(country, Cases, .fun = max, .desc = TRUE),
             ncol = 4) + 
  theme_classic()

ggplot(ebola, aes(x = Date, y = Deaths)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(country, Deaths, .fun = max, .desc = TRUE),
             ncol = 4) + 
  theme_classic()

## Ch. 6 - October 5, 2020 - Practice

# Regular Expression

library(tidyverse)
install.packages("titanic")
library(titanic)
library(stringr)

data("titanic_train")
head(titanic_train)

titanic_train %>%
  separate(Name,
           into = c("last_name", "other_name"),
           sep = ",") %>%
  head()

ex <- titanic_train %>%
  pull(Name) # extract single column

str_extract(ex, pattern = ", M[a-z]+\\.")

str_detect(ex, pattern = ", M[a-z]+\\.") # can put into filter function (T/F)

titanic_train %>%
  filter(str_detect(Name, pattern = ", M[a-z]+\\.")) # detecting

titanic_train %>%
  filter(!str_detect(Name, pattern = ", M[a-z]+\\.")) # detecting all not

# Cont Regular Expressions (using pattern)

ex <- titanic_train %>%
  pull(Name)

ex <- ex[1:3]

str_extract(string = ex, pattern = "Mr") # CASE SENSITIVE

str_extract(string = ex, pattern = "Mr\\.") # period as metacharacter

str_extract(string = ex, pattern = "Mr[s]*\\.") # will bring out those with or
                                                  # without an "s"

str_extract(string = ex, pattern = "M[a-z]+\\.")

str_extract(string = ex, pattern = "M[rs]+\\.")

str_extract(string = ex, pattern = "M[^r]+\\.") # ^ negates whatever that is not

str_extract(string = ex, pattern = "[A-Z][a-z]+\\.") %>%
  unique()

# Cont Regular Expression (more on stringr package)

titanic_train %>% 
  filter(str_detect(Name, "Mrs\\.")) %>%
  head() # pulling out just Mrs.

titanic_train %>% 
  mutate(dr_title = str_extract(Name, "Dr\\.")) %>%
  filter(!is.na(dr_title)) %>%
  head() # pulls out all Dr. with mutate and filter

titanic_train %>% 
  mutate(dr_title = str_extract(Name, "[A-Z][a-z]+\\.")) %>%
  filter(!is.na(dr_title)) %>%
  head() # getting closer to providing full title

titanic_train %>% 
  mutate(title = str_extract(Name, ", [A-Z][a-z]+\\.")) %>%
  head()

titanic_train %>% 
  mutate(title = str_match(Name, ", ([A-Z][a-z]+)\\.")[ , 2]) %>%
  head() # just second column bc it outputs matrix
          # add () to only pull out what we want

titanic_train %>% 
  mutate(title = str_match(Name, ", ([A-Z][a-z]+)\\.")[ , 2]) %>%
  group_by(title) %>%
  count() %>%
  arrange(desc(n))

titanic_train %>%
  filter(str_detect(Name, "^A")) %>%
  head() # only where name starts with A

titanic_train %>%
  filter(str_detect(Name, "I{2,}")) %>%
  head() # only where there are 2 or more I's in a row

titanic_train %>%
  filter(str_detect(Name, "Anders[eo]n")) %>%
  head() # only names with Anderson or Andersen

titanic_train %>%
  filter(str_detect(Name, "^[AB]")) %>%
  head() # names with A or B

titanic_train %>%
  filter(str_detect(Name, "[Bb]$")) %>%
  head() # names that end with either B or b

# Selecting Columns Using Reg Exp

titanic_train %>%
  select(Pclass) %>%
  head()

titanic_train %>%
  select(starts_with("P")) %>%
  head() # selects all columns starting with P

titanic_train %>%
  select(ends_with("ss")) %>%
  head() # selects all columns ending with ss

titanic_train %>%
  select(contains("ss")) %>%
  head()

titanic_train %>%
  select_at(vars(contains("ss")), .funs = str_to_lower) %>%
  head()

titanic_train %>%
  select_if(is.numeric, .funs = str_to_lower) %>%
  head()

# Exercise 6.7.4

data("VADeaths")
head(VADeaths)

VADeaths2 <- VADeaths %>%
              as.data.frame() %>%
              rownames_to_column(var = "age") %>%
              pivot_longer(col = 2:5,
                           names_to = "Gender_Loc",
                           values_to = "Mort_Rate") %>%
              separate(col = Gender_Loc,
                  into = c("Location", "Gender"),
                  sep = " ")

VAplot <- ggplot(VADeaths2, aes(x = age, y = Mort_Rate, color = Gender)) +
          geom_point() +
          facet_wrap(~ Location) +
          theme_few() +
          labs(x = "Age Category", y = "Death Rate (per 1,000)")

# Exercise 6.7.5

install.packages("babynames")
library(babynames)
library(forcats)
data("babynames")
View(babynames)

my_name <- babynames %>%
  filter(name=="Sarah") %>%
  group_by(sex)
my_name

my_name_graph <- my_name %>%
  mutate(sex = fct_recode(sex, Male = "M", Female = "F")) %>%
  ggplot(aes(x = year, y = prop, color = sex)) + 
  geom_line() + 
  labs(x = "Year", y = "Proportion of babies\nof each sex named 'Brooke'", color = "")

top_names_year <- babynames %>% 
  filter(year == 1995) %>% 
  group_by(sex) %>% 
  arrange(desc(prop)) %>% 
  slice(1:5)

top_names_year












