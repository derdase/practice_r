### Video Problems

## Mapping w/ tidyverse

install.packages("tigris")
library(tigris)
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(viridis)

co_counties <- counties(state="CO", cb = TRUE, class = "sf")
class(co_counties)

co_counties %>%
  slice(1:3)

class(co_counties$geometry)

st_bbox(co_counties$geometry)

st_bbox(co_counties$geometry[1])

ggplot() +
  geom_sf(data = co_counties, aes(fill = ALAND)) +
  scale_fill_viridis_b(name = "Land area", label = comma) +
  ggtitle("Land areas of Colorado counties") +
  theme_dark()

larimer <- co_counties %>%
  filter(NAME == "Larimer")
larimer

ggplot() +
  geom_sf(data = co_counties, color = "lightgray") +
  geom_sf(data = larimer, fill = "darkcyan") +
  geom_sf_text(data = larimer, aes(label = NAME),
               color = "white") +
  theme_dark() + labs(x = "", y = "")

co_event_counts <- storm_final %>%
  filter(state == "Colorado") %>%
  group_by(fips) %>%
  count() %>%
  ungroup

co_county_events <- co_counties %>%
  mutate(fips = paste(STATEFP, COUNTYFP, sep = "")) %>%
  full_join(co_event_counts, by = "fips") %>%
  mutate(n = ifelse(!is.na(n), n, 0))

ggplot() + geom_sf(data = co_county_events, aes(fill = n)) +
  scale_fill_viridis_b(name = "Number of events\n(1960)")

co_event_counts <- storm_final %>%
  filter(state == "Colorado") %>%
  filter(event_type %in% c("Tornado", "Heavy Rain", "Hail")) %>%
  group_by(fips, event_type) %>%
  count() %>%
  ungroup()

co_county_events <- co_counties %>%
  mutate(fips = paste(STATEFP, COUNTYFP, sep = "")) %>%
  right_join(co_event_counts, by = "fips")

ggplot() +
  geom_sf(data = co_counties, color = "lightgray") +
  geom_sf(data = co_county_events, aes(fill = n)) +
  scale_fill_viridis_b(name = "Number of events\n(1960)") +
  theme(legend.position = "top") +
  facet_wrap(~ event_type, ncol = 3) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 

## More mapping w/ tidyverse

library(maps)

us_states <- map("state", plot=FALSE, fill = TRUE) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = us_states, color = "white",
          fill = "darkcyan", alpha = 0.5)
# can use xlim and ylim for zooming in on particular coordinates

getwd()
list.files("/Users/sarahderda/Desktop/ERHS 535 R Programming/practice_r/data/al092017_best_track/")
st_layers("/Users/sarahderda/Desktop/ERHS 535 R Programming/practice_r/data/al092017_best_track/")

harvey_track <- read_sf("/Users/sarahderda/Desktop/ERHS 535 R Programming/practice_r/data/al092017_best_track/",
                        layer = "al092017_lin")

head(harvey_track)

ggplot() +
  geom_sf(data = filter(us_states, ID %in% c("texas", "louisiana"))) +
  geom_sf(data = harvey_track, aes(color = STORMTYPE)) +
  xlim(c(-107, -89)) + ylim(c(25, 37))

harvey_windswath <- read_sf("/Users/sarahderda/Desktop/ERHS 535 R Programming/practice_r/data/al092017_best_track/",
                            layer = "al092017_windswath")

head(harvey_windswath)

ggplot() +
  geom_sf(data = filter(us_states, ID %in% c("texas", "louisiana"))) +
  geom_sf(data = harvey_windswath, aes(fill = factor(RADII)), alpha = 0.2) +
  xlim(c(-107, -89)) + ylim(c(25, 37)) +
  scale_fill_viridis_d(name = "Wind (kts)", option = "B", begin = 0.6, direction = -1)

# Exercise 8.5

library(readr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
data("state")

storm_1960 <- read_csv("/Users/sarahderda/Desktop/StormEvents_details-ftp_v1.csv")

storm_final <- storm_1960 %>%
  select(BEGIN_DATE_TIME, 
         END_DATE_TIME,
         EPISODE_ID:STATE_FIPS,
         EVENT_TYPE:CZ_NAME,
         SOURCE,
         BEGIN_LAT:END_LON) %>%
  mutate(BEGIN_DATE_TIME = dmy_hms(BEGIN_DATE_TIME),
         END_DATE_TIME = dmy_hms(END_DATE_TIME),
         STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME)) %>%
  filter(CZ_TYPE == "C") %>% 
  select(-CZ_TYPE) %>% 
  mutate(STATE_FIPS = str_pad(STATE_FIPS, 2, side = "left", pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, 3, side = "left", pad = "0")) %>% 
  unite(fips, STATE_FIPS, CZ_FIPS, sep = "") %>% 
  rename_all(funs(str_to_lower(.)))

us_state_info <- data_frame(state = state.name, 
                            area = state.area,
                            region = state.region)
state_storms <- storm_final %>% 
  group_by(state) %>% 
  count() %>% 
  ungroup() %>% 
  right_join(us_state_info, by = "state")

storm_plot <- ggplot(state_storms, aes(x = area, y = n)) + 
  geom_point(aes(color = region)) + 
  labs(x = "Land area (square miles)", 
       y = "# of storm events in 2017")
storm_plot

# Exercise 8.5.3

library(viridis)
library(maps)

storms_2017 <- read_csv("/Users/sarahderda/Desktop/StormEvents_details-ftp_v1.0_d2017_c20200616(1).csv")

storms_2017 <- storms_2017 %>% 
  select(BEGIN_DATE_TIME, END_DATE_TIME, 
         EPISODE_ID:STATE_FIPS, EVENT_TYPE:CZ_NAME, SOURCE,
         BEGIN_LAT:END_LON) %>% 
  mutate(BEGIN_DATE_TIME = dmy_hms(BEGIN_DATE_TIME),
         END_DATE_TIME = dmy_hms(END_DATE_TIME),
         STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME)) %>% 
  filter(CZ_TYPE == "C") %>% 
  select(-CZ_TYPE) %>% 
  mutate(STATE_FIPS = str_pad(STATE_FIPS, 2, side = "left", pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, 3, side = "left", pad = "0")) %>% 
  unite(fips, STATE_FIPS, CZ_FIPS, sep = "") %>% 
  rename_all(funs(str_to_lower(.)))

co_event_counts <- storms_2017 %>% 
  filter(state == "Colorado") %>% 
  group_by(fips) %>% 
  count() %>% 
  ungroup()

co_county_events <- co_counties %>% 
  mutate(fips = paste(STATEFP, COUNTYFP, sep = "")) %>% 
  full_join(co_event_counts, by = "fips") %>% 
  mutate(n = ifelse(!is.na(n), n, 0))

ggplot() + 
  geom_sf(data = co_county_events, aes(fill = n)) + 
  scale_fill_viridis(name = "Number of events\n(2017)")

co_event_counts <- storms_2017 %>% 
  filter(state == "Colorado") %>% 
  filter(event_type %in% c("Tornado", "Heavy Rain", "Hail")) %>% 
  group_by(fips, event_type) %>% 
  count() %>% 
  ungroup()

co_county_events <- co_counties %>% 
  mutate(fips = paste(STATEFP, COUNTYFP, sep = "")) %>% 
  right_join(co_event_counts, by = "fips")

ggplot() +
  geom_sf(data = co_counties, color = "lightgray") + 
  geom_sf(data = co_county_events, aes(fill = n)) + 
  scale_fill_viridis(name = "Number of events\n(2017)") + 
  theme(legend.position = "top") + 
  facet_wrap(~ event_type, ncol = 3) + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

