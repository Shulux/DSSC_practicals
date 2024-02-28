##### DSSC Practical 5: Data Visualisation (base)
### Movies data

# installed.packages("ggplot2movies")

data(movies, package = "ggplot2movies")
?ggplot2movies::movies


dim(movies)
names(movies)
str(movies)
summary(movies)

sum(is.na(movies))

## Exercise 5.45
# largest budget in the data set

max(movies$budge, na.rm = TRUE)

## Exercise 5.46

movies.withbudget <- movies[!is.na(movies$budget), ]

dim(movies.withbudget) # only 5215 now
str(movies.withbudget)
sum(is.na(movies.withbudget)) # no missing now

highest.budget <- movies.withbudget[movies.withbudget$budget == max(movies.withbudget$budget), "title"]
highest.budget

rm(movies.withbudget)
rm(highest.budget)


## Exercise 5.47/5.48
# create a histogram of the length of all films
# interpret the plot
# then box plot

hist(movies$length)
boxplot(movies$length, horizontal = TRUE)

movies[movies$length > 1000, "title"]


## Exercise 5.49
# repeat the histogram,
# using only films less than or equal to 180 minutes
# manually choose breakpoints for bins of length 1 minute

h <- hist(movies[movies$length <= 180, ]$length, breaks = seq(0, 180, 1))

str(h)

## Exercise 5.50
# investigate what bin lengths the patterns disappear

for (m in c(1:6, 9, 10)) {
  hist(movies[movies$length <= 180, ]$length, breaks = seq(0, 180, m))
}

h$breaks[order(h$counts, decreasing = TRUE)[1:3]]

## Exercise 5.51
# scatterplot of movie length against year of release
# restrict the y axis to 0-500 range
# add a lowess smoother and straight line

plot(movies$year, movies$length, ylim = c(0, 500))
lines(lowess(movies$year, movies$length), col = "red")
abline(lm(movies$length ~ movies$year), col = "green")


## Exercise 5.52
# i) write a function that takes a vector of data
#    and calculate the bootstrap confidence interval
#    of the median of that data
#
# ii) write a for loop which creates a new data frame
#     one row at a time for each decade in the data
#     it should have 3 variables: the decade, the lower,
#     and the upper bootstrap confidence interval for the
#     median.
#
# iii) draw a line plot showing the lower and upper 99%
#      confidence intervals for the median movie length for
#      each decade plot(..., type = "l", ...)

confint.bootstrap <- function(x, sig.level = 0.05, B = 1000, S = median) {
  S.star <- rep(0, B)
  for (b in 1:B) {
     x.star <- sample(x, replace = TRUE)
     S.star[b] <- S(x.star)
  }

  estimate <- S(x)

  conf.int <- estimate + c(-1, 1) * qnorm(p = sig.level, lower.tail = FALSE) * sd(S.star)

  return(conf.int)
}

### solution for (i)
bootstrap <- function(x, ci = 0.99, B = 10000) {
  S.star <- rep(0, B)
  for (i in 1:B) {
    x.star <- sample(x, replace = TRUE)
    S.star[i] <- median(x.star)
  }

  lower <- sort(S.star)[round((1 - ci)/2*B)]
  upper <- sort(S.star)[round((ci + (1 - ci)/2)*B)]

  return(c(lower, upper))
}

## testing my solution
test.vector <- runif(100, 0, 100)
median(test.vector)
confint.bootstrap(test.vector, 0.01, 10000, median)
bootstrap(test.vector)


## ii)
str(movies)

med.CI <- NULL

for (decade in seq(1890, 2000, 10)) {
  movies.decade.lengths <- movies[movies$year >= decade & movies$year < (decade + 10), "length"]
  decade.ci <- bootstrap(movies.decade.lengths)
  med.CI <- rbind(med.CI,
                  data.frame(decade = decade,
                             lower = decade.ci[1],
                             upper = decade.ci[2]))
}

med.CI


## iii)

plot(med.CI$decade + 5, med.CI$lower, col = "red", type = 'l')
lines(med.CI$decade + 5, med.CI$upper, col = "blue", type = 'l')


