######## Practical 3 DSSC

### Wine Quality
wq.red <- read.csv("C:/Users/nicsa/Documents/Durham University/MATHS/Year 2/MATH2687 Data Science and Statistical Computing DSSC/Practicals/DSSC_practicals/data/winequality-red.csv", sep = ";")
wq.white <- read.csv("C:/Users/nicsa/Documents/Durham University/MATHS/Year 2/MATH2687 Data Science and Statistical Computing DSSC/Practicals/DSSC_practicals/data/winequality-white.csv", sep = ";")
View(wq.red)
View(wq.white)

X <- data.frame(sugars = c(1.9, 2.6, 2.3, 1.9),
                pH = c(3.51, 3.20, 3.26, 3.16),
                alcohol = c(9.4, 9.8, 9.8, 9.8))

dim(X)
colMeans(X)
summary(X)

str(wq.red)
str(wq.white)

names(wq.red)
names(wq.white)


## Exercise 5.23

plot(wq.red$sulphates, wq.red$quality)


## Exercise 5.24

red_median <- median(wq.red$sulphates)
low.s <- wq.red[which(wq.red$sulphates <= red_median),]
high.s <- wq.red[which(wq.red$sulphates > red_median),]


## Exercise 5.25
# plot the empirical cdf of the low sulphate wine qualities

ecdf.low <- ecdf(low.s$quality)
plot(ecdf.low)


## Exercise 5.26
# add to the plot the high sulphate ecdf

ecdf.high <- ecdf(high.s$quality)
lines(ecdf.high, col = "red")


## Exercise 5.27
# ecdf(x) evaluates ^F(x). estimate P(Q <= 5) P(Q >= 7)

str(ecdf.low)
str(ecdf.high)

ecdf.low(5)
1 - ecdf.high(6)


## Exercise 5.28
# compute the confidence interval for the mean wine quality
# of the top 50% of sulphate red and
# bottom 50% of sulphate red wines
t.test(high.s$quality)
t.test(low.s$quality)


## Exercise 5.29
# we can use bootstrap to estimate the confidence interval
# for the standard deviation in wine quality
# even though we dont know the sampling distribution

n <- 1000
m <- 1000

sd.low <- rep(0, n)
sd.high <-  rep(0, n)

for (i in 1:n) {
  x.star.low <-  sample(low.s$quality, m, replace = TRUE)
  x.star.high <- sample(high.s$quality, m, replace = TRUE)
  sd.low[i] <- sd(x.star.low)
  sd.high[i] <- sd(x.star.high)
}

sd.low.estimator <- sd(low.s$quality)
sd.high.estimator <- sd(high.s$quality)

empirical_sd.low <- sum(sd.low)/n
empirical_sd.high <- sum(sd.high)/n

empirical_se.low <- sd(sd.low)
empirical_se.high <- sd(sd.high)

c(sd.low.estimator, sd.high.estimator)
c(empirical_sd.low, empirical_sd.high)
c(empirical_se.low, empirical_se.high)



### Categorical Variables
## factor variables with possible levels

wq.red$colour <- factor(rep("red", nrow(wq.red)))
wq.white$colour <- factor(rep("white", nrow(wq.white)))

## Exercise 5.30
# rbind, combine the data frames into a single one called wq

wq <- rbind(wq.red, wq.white)

summary(wq)
dim(wq)
names(wq)
str(wq)
