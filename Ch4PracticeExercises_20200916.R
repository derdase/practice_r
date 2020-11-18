install.packages("dplyr")
library("dplyr")
library("ggplot2")
install.packages("ggthemes")
library("ggthemes")
library("gridExtra")
library("lubridate")

library("faraway")
data(worldcup)

install.packages("dlnm")
library("dlnm")
data(chicagoNMMAPS)

chic <- chicagoNMMAPS

chic_july <- chic %>%
              filter(year == 1995 & month == 7)

hottest_day <- chic_july %>%
  filter(temp == max(temp))

ggplot(data=chic_july, aes(x = date, y = death)) +
        geom_point(color="red") +
        geom_text(data = hottest_day, label = "Max",
                  size = 3, hjust = -0.2, vjust = 0)

hw <- tibble(data = c(ymd("1995-07-12"), ymd("1995-07-16")),
             death = c(425,425))

ggplot(chic_july, aes(x = date, y = death)) +
        geom_point(color="red") +
        geom_line(data=hw)

# Exercise 4.10.1

library(faraway)
data(worldcup)
head(worldcup, 2)

library(dplyr)
worldcup <- worldcup %>%
  tibble::rownames_to_column(var = "Player")
head(worldcup, 2)

# install.packages("ggplot2")
library(ggplot2)
# install.packages("ggthemes")
library(ggthemes)

# Exercise 4.10.2

ggplot(data=worldcup) + geom_point(aes(x = Time, y = Shots), size=1, alpha=0.5)

worldcup <- worldcup %>%
  mutate(top_4 = Team %in% c("Netherlands", "Uruguay", "Spain", "Germany"))
worldcup

ggplot(data=worldcup) + geom_point(aes(x = Time, y = Shots, color = top_4), size=1, alpha=0.5) + theme_classic() +
  labs(x = "Time played in World Cup (mins)", color = "Team's Final \n Ranking") + geom_vline(xintercept = 270, linetype= 2, color="grey") +
  geom_text(data = worldcup_player, x = worldcup_player$Time, y = worldcup_player$Shots, label= "Gyana, Gana", hjust = 1, vjust = 1)

head(worldcup)
worldcup_player <- worldcup %>%
      filter(Shots==max(Shots))




