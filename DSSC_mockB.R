rm(list=ls())

ufo <- read.csv("https://www.louisaslett.com/Courses/DSSC/computer_exam/ufo.csv")
countries <- read.csv("https://www.louisaslett.com/Courses/DSSC/computer_exam/countries.csv")

str(ufo)
str(countries)

library(tidyverse)
library(ggplot2)
library(stringr)


## Q11.1

ggplot(ufo) +
  geom_bar(aes(y=ufo_shape))


## Q11.2

ufo2 <- ufo |>
  mutate(ufo_shape = ifelse(is.na(ufo_shape), "unknown", ufo_shape))

sum(is.na(ufo$ufo_shape))
sum(is.na(ufo2$ufo_shape))

ufo$ufo_shape[60]
ufo2$ufo_shape[60]

ufo2 <- ufo2 |>
  mutate(ufo_shape = str_to_title(ufo_shape))

ufo2$ufo_shape

str(ufo2$ufo_shape)

factor.levels <- ufo2 |>
  group_by(ufo_shape) |>
  summarise(count = n()) |>
  arrange(desc(count))

factor.levels

ufo2 <- ufo2 |>
  mutate(ufo_shape = factor(ufo_shape, levels = factor.levels$ufo_shape))

str(ufo2$ufo_shape)


ggplot(ufo2) +
  geom_bar(aes(y = ufo_shape)) +
  labs(x = "Number of observed UFOs", y = "UFO Shape")


## Q11.3

library(lubridate)

ufo3 <- ufo2 |>
  mutate(date_time = mdy_hm(date_time)) |>
  mutate(year = year(date_time),
         month = month(date_time, label = TRUE, abbr = FALSE),
         day = day(date_time),
         wday = wday(date_time, label = TRUE),
         hour = hour(date_time),
         min = minute(date_time))

str(ufo2$date_time)
str(ufo3$date_time)

#MONTH/DAY/YEAR HOUR/MINUTE

ggplot(ufo3, aes(x = year)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Year") +
  geom_vline(aes(xintercept = 1993, col = "red"), show.legend = FALSE)


## Q11.4

ufo4 <- ufo3 |>
  filter(latitude > 0) |>
  mutate(season = ifelse(month == "December"  |
                           month == "January"  |
                           month == "February", "Winter",
                         ifelse(month == "March"  |
                                  month == "April"  |
                                  month == "May", "Spring",
                                ifelse(month == "June"  |
                                         month == "July"  |
                                         month == "August",
                                       "Summer",
                                       "Autumn"))))


head(ufo4)

hour.counts <- ufo4 |>
  group_by(hour, season) |>
  summarise(count = n())

ggplot(hour.counts) +
  geom_line(aes(x = hour, y = count, fill = season, col = season)) +
  geom_point(aes(x = hour, y = count))


## Q11.5

countries.wider <- pivot_wider(countries, names_from=var, values_from=x)

short.countries.wider <- countries.wider |>
  select(alpha_3, name, region, sub_region)

ufo5 <- left_join(ufo4, short.countries.wider, by = c("country" = "alpha_3"))

table(ufo5$sub_region)

str(ufo5)

## Q11.6

library("tidyverse")
wrld <- map_data("world")
gb <- wrld |> filter(subregion == "Great Britain")

str(gb)

ufo.british <- ufo5 |>
  filter(country == "GBR" | country == "IRL")


library("leaflet")

leaflet() |>
  addTiles() |>
  addPolygons(lng = gb$long,
              lat = gb$lat,
              label = "Great Britain") |>
  addCircles(lng = as.numeric(ufo.british$longitude),
             lat = as.numeric(ufo.british$latitude),
             label = ufo.british$description,
             col = "red")
