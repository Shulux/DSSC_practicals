##### DSSC Practical 6: Data Wrangling

#install.packages("tidyverse")

library("tidyverse")

pres.res <- data.frame(
  Candidate = c("Clinton", "Trump", "Other"),
  California = c("8753788/14181595", "4483810/14181595", "943997/14181595"),
  Arkansas = c("380494/1130676", "684872/1130676", "65310/1130676")
)

# candidate first column
# proportion of the vote for each candidate
# as a fraction votes/total_votes

pres.res


## Exercise 5.53
# is the above data tidy? why?

# each column is a variable
# each row is an observation
# could be put as percentages
# repeated values
# could use a total votes seperate from vote count

## something like
# candidate, state, vote count, total votes
# clinton, california, 8753788, 14181595
# clinton, arkansas, 380494, 1130676
# ....
# ....
# ....
# ....


## Exercise 5.54
# manipulate pres.res dataset to create a new one
# containing 6 rows, 3 columns, candidate, state, proportion
# still containing textual proportion

pres.res2 <- pivot_longer(pres.res,
                          cols = c("California", "Arkansas"),
                          names_to = "State",
                          values_to = "Proportion")

pres.res2


## Exercise 5.55
# create a dataset pres.res3 in which you split proportion
# into two new variables Votes and Total

pres.res3 <- separate(pres.res2,
                       sep = "/",
                       col = "Proportion",
                       into = c("Votes", "Total"))
pres.res3

str(pres.res3)

pres.res4 <- mutate(pres.res3,
                    Votes = as.numeric(Votes),
                    Total = as.numeric(Total))

pres.res4

str(pres.res4)


## Exercise 5.56
# create a new data set pres.res5 which contains
# the overall % of the vote for each candidate
# for this 2-state result, sort the final output
# from highest vote share to lowest.

pres.res5 <- pres.res4 |>
        group_by(Candidate) |>
        summarise(Percentage = signif(100*sum(Votes)/sum(Total), 3)) |>
        arrange(desc(Percentage))


#### using all of the above in a pipeline

pres.res6 <- pres.res |>
  pivot_longer(cols = c("California", "Arkansas"),
               names_to = "State",
               values_to = "Proportion") |>

  separate(sep = "/",
           col = "Proportion",
           into = c("Votes", "Total")) |>

  mutate(Votes = as.numeric(Votes),
         Total = as.numeric(Total)) |>

  group_by(Candidate) |>

  summarise(Percentage = signif(100*sum(Votes)/sum(Total), 3)) |>

  arrange(desc(Percentage))



## Exercise 5.7
# install nycflights13 package
# load dataset flights

#install.packages("nycflights13")

library(nycflights13)
data("flights", package = "nycflights13")
?nycflights13::flights

dim(flights)
names(flights)
str(flights)
summary(flights)

sum(is.na(flights))


## Exercise 5.58
# confirm that no flight departures in the data
# from any year except 2013
# find out how many flights took off from NYC airport
# on your birthday that year
# create a new dataframe with the count of how many flights
# took off on every day of the year
# it should have 365 rows and 4 columns
# (year, month, day, count)
# sort from largest to smallest count

flights |>
  filter(year != 2013) |>
  nrow()

# 0, so no other flight departure years

flights |>
  filter(year == 2013, month == 10, day == 17) |>
  nrow()

# 995, so 995 flights took off on my birthday in 2013

flights.count <- flights |>
  group_by(year, month, day) |>
  summarise(count = n()) |>
  arrange(desc(count))

flights.count


## Exercise 5.59
# find all flights that arrived
# at least 2 hours late
# what is the mean departure delay,
# the standard deviation of the departure delay,
# number of flights for each carrier?
# does this imply a carrier you would prefer
# to avoid a late departure?
# look up the name of the carrier using
# airlines dataset


flights.late <- flights |>
  filter(arr_delay >= 120)

flights.late

flights.stats <- flights |>
  group_by(carrier) |>
  summarise(mean_delay = mean(dep_delay, na.rm = TRUE),
            sd_delay = sd(dep_delay, na.rm = TRUE),
            num_flights = n())

best.carrier.code <- flights.stats[flights.stats$mean_delay == min(flights.stats$mean_delay), ]$carrier

data("airlines", package = "nycflights13")

str(airlines)

name <- airlines |>
  filter(carrier == best.carrier.code) |>
  pull(name)

name


## Exercise 5.60
# plot a histogram and boxplot
# of departure delay
# repeat excluding flights departing over 2 hours late
# so that you can see more detail
# do you see anything unexpected?

hist(flights$dep_delay)
boxplot(flights$dep_delay, horizontal = TRUE)

flights.without_2 <- flights |>
  filter(dep_delay <= 120)

hist(flights.without_2$dep_delay)
boxplot(flights.without_2$dep_delay, horizontal = TRUE)

# negative departure delays


## Exercise 5.61
# recompute the average, sd, and count
# after changing the early departure delay to be 0 instead of negative

flights.pos <- flights |>
  mutate(dep_delay = ifelse(dep_delay <= 0, 0, dep_delay))

flights.stats2 <- flights.pos |>
  group_by(carrier) |>
  summarise(mean_delay = mean(dep_delay, na.rm = TRUE),
            sd_delay = sd(dep_delay, na.rm = TRUE),
            num_flights = n())

flights.stats2

best.carrier.code2 <- flights.stats2[flights.stats2$mean_delay == min(flights.stats2$mean_delay), ]$carrier

data("airlines", package = "nycflights13")

str(airlines)

name2 <- airlines |>
  filter(carrier == best.carrier.code2) |>
  pull(name)

name2

hist(flights.pos$dep_delay )
boxplot(flights.pos$dep_delay, horizontal = TRUE)

flights.pos.without_2 <- flights.pos |>
  filter(dep_delay <= 120)

hist(flights.pos.without_2$dep_delay)
boxplot(flights.pos.without_2$dep_delay, horizontal = TRUE)


# we dont change our preferred airline, it is still US Airlines Inc.


## Exercise 5.62
# the times given in _time are clock times
# which are easy to interpret
# but we cant calculate easily with them
# create a new function clock_to_minutes
# which takes a clock time eg 1259
# and converts it to the number of minutes
# past midnight, eg f(1259) -> 779

clock_to_minutes <- function(clock_time) {
  return(floor(clock_time/100)*60 + clock_time - 100*floor(clock_time/100))
}

clock_to_minutes(1259)


## Exercise 5.63
# use the function to modify the dep_time and arr_time
# columns to record minutes since midnight instead of clock time

flights.modified <- flights |>
  mutate(dep_time = clock_to_minutes(dep_time),
         arr_time = clock_to_minutes(arr_time))

head(flights.modified)


## Exercise 5.64
# NA values, represent cancelled flights
# how many flights were cancelled in total?
# create a table showing the total number of cancellations
# by the hour of scheduled departure,
# sorted in descending order by the count
# repeat the analysis, calculating the proportion
# of flights cancelled in each hour of the day
# would you choose to fly before or after lunch?

flights |>
  filter(is.na(dep_time)) |>
  nrow()

cancellation.table <- flights |>
  filter(is.na(dep_time)) |>
  mutate(sched_dep_time = floor(sched_dep_time/100)) |>
  group_by(sched_dep_time) |>
  summarise(count_cancellation = n()) |>
  arrange(desc(count_cancellation))

cancellation.table


cancellation.table.proportion <- flights |>
  mutate(sched_dep_time = floor(sched_dep_time/100)) |>
  group_by(sched_dep_time) |>
  summarise(total = n(), cancelled = sum(is.na(dep_time))) |>
  mutate(proportion = cancelled/total) |>
  arrange(desc(proportion))

cancellation.table.proportion

# we can see the top 10 cancelled times are
# 1am, 7pm, 8pm. 9pm, 4pm, 10pm, 6pm, 3pm, 7pm, 2pm
# which are all after lunch
# so we should aim to book before lunch
