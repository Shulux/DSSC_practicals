#### DSSC Practical 7: Advanced Graphics
### 5.8 Joining data frames

library("tidyverse")
data("band_members", package = "dplyr")
data("band_instruments", package = "dplyr")
data("band_instruments2", package = "dplyr")

dim(band_members)
dim(band_instruments)
dim(band_instruments2)

names(band_members)
names(band_instruments)
names(band_instruments2)

#i)
right_join(band_members, band_instruments)


#ii)
left_join(band_members,
          band_instruments2 |>
            select(name = artist, plays))

#iii)
inner_join(band_members, band_instruments)


## Exercise 5.66
# left_join and read explanation of 'by' argument
# join band_members and band_instruments2
# without renaming the artist column

left_join(band_members, band_instruments2, by = c("name" = "artist"))


### 5.9 Car efficiency


library("ggplot2")
data("mpg", package = "ggplot2")
?ggplot2::mpg

dim(mpg)
names(mpg)
str(mpg)
summary(mpg)

## Exercise 5.67
# produce a scatterplot of the fuel efficiency
# on a highway against the engine size
#
# i) colour the points by the type of car
#
# ii) remove the colouring, but split the plot
#     into facets where you have a graphic per
#     car type
#
# iii) add to this collection of plots the fuel
#      efficiency for city driving, coloured in red
#      update the y-axis to be called "fuel efficiency"
#      to reflect that it is not only the hwy variable plotted now

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(colour = class)) +
  xlab("Engine size (litres)") + ylab("Fuel efficiency (miles per gallon)")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlab("Engine size (litres)") + ylab("Fuel efficiency (miles per gallon)") +
  facet_wrap(~ class)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlab("Engine size (litres)") + ylab("Fuel efficiency (miles per gallon)") +
  facet_wrap(~ class) +
  geom_point(aes(x = displ, y = cyl), col = "red")


## Exercise 5.68
# write the code to produce the plot (on the website)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(col = drv)) +
  geom_smooth(aes(col = drv)) +
  geom_smooth(col = "black")

## Exercise 5.69
# produce a bar plot of the count
# of the number of each type of vehicle
# in the data using the geom_bar()
# with aesthetic x set to the vehicle type
# make a second version where you add
# an aesthetic fill which is set to the variable drv
# what is this showing?

ggplot(mpg, aes(x = class)) +
  geom_bar()

ggplot(mpg, aes(x = class)) +
  geom_bar(aes(fill = drv))


### 5.10 New York City flights

data("flights", package = "nycflights13")
?nycflights13::flights


## Exercise 5.70
# by looking at the help file or otherwise
# find the names of all the data frames in the nycflights13
# package and load them

data("airlines", package = "nycflights13")
data("airports", package = "nycflights13")
data("planes", package = "nycflights13")
data("weather", package = "nycflights13")
data("flights", package = "nycflights13")


## Exercise 5.71
# without running it what does the following code do?

routes <- flights |>   # assign 'routes' to the output of the pipeline
  group_by(dest) |>    # group by destination (airport code)
  summarise(n = n())   # replace all columns by the count of each destination

# so it counts how many flights are going to that airport

airports2 <- left_join(airports, routes, by = c("faa" = "dest"))

head(airports2)



#install.packages("maps")
library(maps)

# Get the latitude and longitude coordinate for mainland America:
usa <- map_data("usa")

# Plot these using geom_polygon
ggplot() +
  geom_polygon(aes(x = long, y = lat), data = usa, fill = "darkgreen")


## Exercise 5.72
# what are the maximum and minimum longitude in the usa dataset
# defining the most westerly and easterly longitudes of mainland america?

max_min <- c(max(usa$long), min(usa$long))

airports2 <- airports2 |>
  filter(lon >= max_min[2] & lon <= max_min[1])

## Exercise 5.73
# add the airports2 dataset to the ggplot
# then plot the lat/long location
# of all mainland US airports
# by adding a geom_point()
# change the size to be half the default

# Get the latitude and longitude coordinate for mainland America:
usa <- map_data("usa")

# Plot these using geom_polygon
ggplot(airports2) +
  geom_polygon(aes(x = long, y = lat), data = usa, fill = "darkgreen") +
  geom_point(aes(x = lon, y = lat), size = 0.5)


## Exercise 5.74
# update the geom_point layer by removing the fixed size
# and adding a size aesthetic linked to the variable n

ggplot(airports2) +
  geom_polygon(aes(x = long, y = lat), data = usa, fill = "darkgreen") +
  geom_point(aes(x = lon, y = lat, size = n))
