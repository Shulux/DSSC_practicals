###### DSSC Practical 4
#### Base R data exploration
#### Diamonds data


## Exercise 5.31

#install.packages("ggplot2")

library("ggplot2")
data(diamonds, package = "ggplot2")
?diamonds

dim(diamonds)
names(diamonds)
str(diamonds)
summary(diamonds)


## Exercise 5.32
# how many observations and variables?

dim(diamonds)

## Exercise 5.33
# how many factor type variables are there?

str(diamonds) # 3

## Exercise 5.34
# print first few observations

head(diamonds)

## Exercise 5.35
# summary statistics

summary(diamonds)

## Exercise 5.36
# find the total value of all ideal cut diamonds
# with colour code 'D' and with depth percentage of 60 or less
# is the total value greater than $200,000?

total.value <- sum(diamonds[diamonds$cut == "Ideal" & diamonds$color == "D" & diamonds$depth <= 60, "price"])
total.value

total.value > 200000
total.value <= 200000


## Exercise 5.37
# create a new variable called ppc in the diamonds dataframe
# containing price per carat of each diamond
# compute the median price per carat

diamonds["ppc"] <- diamonds$price / diamonds$carat

names(diamonds)
str(diamonds)
summary(diamonds)

median(diamonds$ppc)


## Exercise 5.38
# plot a scatterplot of carat on the x-axis and ppc on the y-axis

plot(diamonds$carat, diamonds$ppc)


## Exercise 5.39
# add a horizontal line for the mean ppc
# add vertical lines for carat = 1, 2, 3, 4, 5
# what range of carats seem to attract the highest ppc?

abline(h = mean(diamonds$ppc), col = "red")
abline(v = c(1, 2, 3, 4, 5), col = "blue")

# the 1:2 carat diamonds seem to attract the highest ppc


## Exercise 5.40
# create two new data frames
# contain only the diamonds carat between 1 and 2
# first has ppc exceeding 10,000
# second has ppc less than or equal to 10,000
# provide counts for clarity of each dataframe

diamonds.lowppc <- diamonds[(1 <= diamonds$carat) & (diamonds$carat <= 2) & (diamonds$ppc <= 10000), ]
diamonds.highppc <- diamonds[(1 <= diamonds$carat) & (diamonds$carat <= 2) & (diamonds$ppc > 10000), ]

str(diamonds.lowppc)
str(diamonds.highppc)

table(diamonds.lowppc$clarity)
table(diamonds.highppc$clarity)


## Exercise 5.41
# what is the price, number of carats,
# cut and clarity of the most expensive diamond in the data?

expensive <- diamonds[diamonds$price == max(diamonds$price), ]
expensive[, c("price", "carat", "cut", "clarity")]

## Exercise 5.42
# are there any diamonds which have more carats, superior cut
# and superior clarity than the most expensive diamond?
# what is the biggest saving you could make if you just wanted
# to improve on these characteristics?

diamonds.better <- diamonds[diamonds$carat > expensive$carat &
                              diamonds$cut > expensive$cut &
                              diamonds$clarity > expensive$clarity
                              , ]

diamonds.better

cheapest.better <- diamonds.better[diamonds.better$price == min(diamonds.better$price), ]
cheapest.better

saving <- abs(cheapest.better$price - expensive$price)
saving
