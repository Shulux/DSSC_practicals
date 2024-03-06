## SECTION_B

colony <- read.csv("https://www.louisaslett.com/Courses/DSSC/computer_exam/colony.csv")
stressor <- read.csv("https://www.louisaslett.com/Courses/DSSC/computer_exam/stressor.csv")

??colony

str(colony)
str(stressor)

# libraries

library("lubridate")
library("stringr")
library("tidyverse")
library("ggplot2")



### Q11.1

# firstly colony dataframe

colony2 <- colony |>
  mutate(months = factor(months, levels = c("January-March",
                                               "April-June",
                                               "July-September",
                                               "October-December"))) |>
  rename(quarter = months) |>
  filter(state != "United States")

n_distinct(colony2$state)
unique(colony2$state)
str(colony2)

nrow(colony2)

# now for stressor dataframe

stressor2 <- stressor |>
  mutate(months = factor(months, levels = c("January-March",
                                            "April-June",
                                            "July-September",
                                            "October-December"))) |>
  rename(quarter = months) |>
  filter(state != "United States")

n_distinct(stressor2$state)
unique(stressor2$state)
str(stressor2)

nrow(stressor2)

# checks
head(colony2, 3)
levels(colony2$quarter)

head(stressor2, 3)
levels(stressor2$quarter)



### Q11.2

ggplot(colony2) +
  geom_boxplot(aes(x = quarter, y = colony_reno))


ggplot(colony2) +
  geom_boxplot(aes(x = quarter, y = colony_reno)) +
  labs(x = "Quarter", y = "Number of bee colony renovations") +
  scale_y_log10()



### Q11.3

ggplot(stressor2, aes(x = stress_pct)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ stressor) +
  labs(x = "Percentage of colonies")



### Q11.4

stressor_wide <- pivot_wider(stressor2,
                            names_from = "stressor",
                            values_from = "stress_pct")

str(stressor_wide)
head(stressor_wide, 3)


colony_full <- left_join(colony2, stressor_wide, by = c("year", "quarter", "state"))

head(colony_full, 3)
str(colony_full)



### Q11.5

colony3 <- colony2 |>
  mutate(month_int = ifelse(quarter == "January-March", 1,
                              ifelse(quarter == "April-June", 4,
                                     ifelse(quarter == "July-September", 7, 10)))) |>
  mutate(date = date(make_datetime(year, month = month_int))) |>
  select(-month_int)


head(colony3, 3)
str(colony3)

california.connecticut.texas <- colony3 |>
  filter(state == "California" | state == "Connecticut" | state == "Texas")

ggplot(california.connecticut.texas, aes(x = date, y = colony_n, fill = state, col = state)) +
  geom_line() +
  labs(x = "Date", y = "Total number of colonies")



# there are gaps because in the dataset there are missing values (NA)
# for the date 2019-04-01 (quarter 2 of 2019)
# shown below

california.connecticut.texas |>
  select(colony_n, date) |>
  filter(date == make_datetime(year = 2019,
                               month = 4))



### Q11.6

colony4 <- colony3

ggplot(colony4) +
  geom_bar()
