#### DSSC Practical 10
#### Dates and Strings

### 5.14 New York City flights

library(tidyverse)
library(lubridate)

data("flights", package = "nycflights13")

?nycflights13::flights


## Exercise 5.94
# create two new variables sched_dep_hour and sched_dep_min
# which split sched_dep_time into two

# then create a new variable sched_dep
# which uses year, month, day, sched_dep_hour, sched_dep_min
# to create a date-time representation
# make sure it is set to America/New_York
# remove sched_dep_hour and sched_dep_min


flights <- flights |>
  mutate(sched_dep_hour = floor(sched_dep_time/100),
         sched_dep_min = sched_dep_time - 100*floor(sched_dep_time/100))

flights <- flights |>
  mutate(sched_dep = make_datetime(year, month, day, sched_dep_hour, sched_dep_min,
                                   tz = "America/New_York")) |>
  select(-sched_dep_hour, -sched_dep_min)


## Exercise 5.95
# summarise the average departure delay
# by day of the week for which the flight was scheduled

flights.delay <- flights |>
  mutate(sched_wday = wday(sched_dep, label = TRUE)) |>
  group_by(sched_wday) |>
  summarise(average_dep_delay = mean(dep_delay, na.rm = TRUE))

flights.delay.without.negative <- flights |>
  filter(dep_delay >= 0) |>
  mutate(sched_wday = wday(sched_dep, label = TRUE)) |>
  group_by(sched_wday) |>
  summarise(average_dep_delay = mean(dep_delay, na.rm = TRUE))


## Exercise 5.96
# write your own function to calculate a bootstrap
# estimate of the standard error in the mean for
# the departure delay on each day of the week
# calculate a simple normal confidence interval for the mean
# use 200 bootstrap simulations
# everything else should be done with tidyverse
# then by looking at the help file for
# geom_errorbarh
# create a plot showing these confidence intervals for each
# day of the week

bootstrap <- function(x) {

  B <- 200
  S <- mean
  n <- length(x)

  S.star <- rep(0, B)

  for (i in 1:B) {
    x.star <- sample(x, size = n, replace = TRUE)
    S.star[i] <- S(x.star)
  }

  estimate <- S(x)
  standard_error <- sd(S.star)

  return(standard_error)
}

mean_ci <- flights |>
  filter(dep_delay >= 0) |>
  mutate(day_of_week = wday(sched_dep, label = TRUE)) |>
  group_by(day_of_week) |>
  summarise(mean_delay = mean(dep_delay, na.rm = TRUE),
            bootstrap_se = bootstrap(dep_delay)) |>
  mutate(mean_ci_lower = mean_delay - 1.96*bootstrap_se,
         mean_ci_upper = mean_delay + 1.96*bootstrap_se)

mean_ci

ggplot(mean_ci, aes(x=mean_delay, y= day_of_week)) +
  geom_point() +
  geom_errorbarh(aes(xmin=mean_ci_lower, xmax=mean_ci_upper))



## Exercise 5.97
# the flights data frame contains a column
# called time_hour to enable joining with the weather table

data("weather", package = "nycflights13")
str(weather)
?nycflights13::weather

flights <- flights |>
  mutate(time_hour2 = floor_date(sched_dep, unit = "hour"))

head(select(flights, time_hour, time_hour2))
all(flights$time_hour == flights$time_hour2)



rm(list = ls())

### 5.15 Strings

library(tidyverse)

data("sentences", package = "stringr")

mean(str_length(sentences))

sentences[which.max(str_length(sentences))]
sentences[which.min(str_length(sentences))]

any(str_length(str_squish(sentences)) != str_length(sentences))

sentences[str_detect(sentences, "sheep")]

sentences[str_detect(sentences, "^.+[:upper:]")]

with_day <- str_detect(sentences, "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday")

day_sentence <- sentences[with_day]

str_replace_all(day_sentence, c("Monday"    = "Mon",
                                "Tuesday"   = "Tue",
                                "Wednesday" = "Wed",
                                "Thursday"  = "Thu",
                                "Friday"    = "Fri",
                                "Saturday"  = "Sat",
                                "Sunday"    = "Sun"))
