### September 2, 2020

library(readr)
library(dplyr)
library(stringr)

# Daily Show Data
url1 <- paste0("https://raw.githubusercontent.com/fivethirtyeight/data/master/daily-show-guests/daily_show_guests.csv")

daily_show <- read_csv(file = url1)

## September 2, 2020 Exercises
url <- paste0("https://raw.githubusercontent.com/cmrivers/",
              "ebola/master/country_timeseries.csv")
ebola <- read_csv(file = url)
slice(.data = (select(.data = ebola, 1:3)), 1:3)

# Copy this code to an R script and add comments describing what each line is doing
# Install any packages that the code loads but that you don't have.
library(package = "haven") # allows us to call upon SAS files
library(package = "forcats") # tools for working with categorical variables
library(package = "stringr") # used to manipulate strings

icu <- read_sas(data_file = "data/icu.sas7bdat") # calling SAS ICU file

icu <- select(.data = icu, ID, AGE, GENDER) # selects columns

icu <- rename(.data = icu, 
              id = ID,
              age = AGE,
              gender = GENDER) # renaming columns in icu dataset to lowercase names

icu <- mutate(.data = icu,
              gender = factor(x = gender),
              gender = fct_recode(.f = gender,
                                  Male = "0",
                                  Female = "1"),
              id = str_c(id)) # joins multiple strings into single string
icu # prints out some of the data

ebola_liberia <- select(.data = ebola, Date, Cases_Liberia, Deaths_Liberia)
ebola_liberia
# could use head(), str(), etc.

ebola_liberia <- rename(.data = ebola_liberia,
                        date = Date,
                        cases = Cases_Liberia,
                        deaths = Deaths_Liberia)
head(x = ebola_liberia)

ebola_liberia <- mutate(.data = ebola_liberia,
                        ratio = deaths/cases)
head(x = ebola_liberia)




