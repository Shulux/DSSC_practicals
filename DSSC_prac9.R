library(ukpolice)
library(ggplot2)
library(tidyverse)

??ukpolice::ukc_available



## Exercise 5.84
# what is the id for durham constabulary

ukc_available()

forces <- ukc_forces()

durhamid <- forces |>
  filter(name == "Durham Constabulary") |>
  select(id)


## Exercise 5.85
# what are the policing neighborhoods in durham?
# save the lat and long of the boundary of durham city
# make sure the data is numeric

durham.nbhoods <- ukc_neighbourhoods(durhamid)


durham.nbhoods |> filter(grepl("Durham City", name))
## 2 codes with Durham City, assuming the first one

durham.city.id <- (durham.nbhoods |>
                     filter(grepl("Durham City", name)) |>
                     select(id)
                   )[1, ]

durham.city.boundary <- ukc_neighbourhood_boundary(durhamid, durham.city.id)

durham.city.boundary <- durham.city.boundary |>
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

str(durham.city.boundary)


## Exercise 5.86
# install leaflet package

#install.packages("leaflet")

library("leaflet")

leaflet() |>
  addTiles() |>
  addPolygons(lng = durham.city.boundary$longitude,
              lat = durham.city.boundary$latitude,
              label = "Durham City")


## Exercise 5.87
# using ukc_crime_poly
# download the most recent month of crime data for durham city
# policing neighbourhood in Durham Constabulary
# do you get an error?
# can you fix it?

durham.city.boundary2 <- durham.city.boundary |>
  mutate(lat = latitude,
         lng = longitude,
         .keep = "none")

durham.city.boundary2 <- durham.city.boundary2[round(seq(1, nrow(durham.city.boundary2), length.out = 100)), ]

crime.data <- ukc_crime_poly(durham.city.boundary2)


## Exercise 5.88
# query the API multiple times to gather data for the most recent
# available 6 months into one data frame

crimes <- rbind(ukc_crime_poly(durham.city.boundary2, "2023-09"),
                ukc_crime_poly(durham.city.boundary2, "2023-08"),
                ukc_crime_poly(durham.city.boundary2, "2023-07"),
                ukc_crime_poly(durham.city.boundary2, "2023-06"),
                ukc_crime_poly(durham.city.boundary2, "2023-05"),
                ukc_crime_poly(durham.city.boundary2, "2023-04"))
dim(crimes)


## Exercise 5.89
# plot a bar chart
# showing the number of each category of crime committed
# extend the bar chart by breaking down each bar
# into the outcome status counts and tidy up the axes and legend labels

ggplot(crimes) +
  geom_bar(aes(y = category, fill = outcome_status_category)) +
  labs(y = "Crime", fill = "Outcome Status")


## Exercise 5.90
# use addCircles() from leaflet package
# add locations of the crimes in red on top of the map
# label of each point should be the category of crime

leaflet() |>
  addTiles() |>
  addPolygons(lng = durham.city.boundary$longitude,
              lat = durham.city.boundary$latitude,
              label = "Durham City") |>
  addCircles(lng = as.numeric(crimes$longitude),
             lat = as.numeric(crimes$latitude),
             label = crimes$category,
             col = "red")


### 5.13 Getting Interactive ...

## Exercise 5.91
# save the nghborhoods into a vector
# overwtie the names of each element
# with the names

nbd2 <- durham.nbhoods$id
names(nbd2) <- durham.nbhoods$name
