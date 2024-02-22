2+3       # add
sqrt(2)   # square root
2*3       # multiply
2^3       # exponent
2/3       # divide
log10(2)  # log base 10
pi        # pi = 3.141593
log(2)    # log base e (natural log, ln)
sin(2)    # sin (in radians)
exp(1)    # e^1 exponential e^x
exp(2)    # e^2 exponential e^x
5%%2      # 5 mod 2 (remainder division)


## Exercise 5.1
# confirm e^pi > pi^e

exp(pi) > pi^exp(1)

x <- sqrt(2)
x


## Exercise 5.2
# compute log base 10 of pi 

log10(pi) # log10(x)
log(pi, 10) # log(x, base = exp(1))

log10(pi) == log(pi,10)


## Exercise 5.3
# 221 students, use qbirthday and pbirthday, assume 30 days in all months,
# calculate the prob that 12 of us are born on the same day of the month

pbirthday(221, classes=30, coincident = 12)


## Exercise 5.4
# simulate 10 random numbers normally distributed with mean 0 and variance 2

rnorm(10, mean=0, sd=sqrt(2))


## Exercise 5.5
# let X ~ N(mu = 2, sd^2 = 1)
# compute P(X>2.13), find x st P(X<=x)=0.123

mean <- 2
sd_squared <-  1

pnorm(2.13, mean=mean, sd=sqrt(sd_squared), lower.tail=FALSE)

qnorm(0.123, mean=mean, sd=sqrt(sd_squared), lower.tail=TRUE)


###
# Define our sample size
# By doing it here we can change and rerun easily to experiment
n <- 10

# Generate some data
x <- rnorm(n, mean = 3.14, sd = 1)

# Compute test statistic
test.stat <- (mean(x) - 3.14)/(1/sqrt(n))

# Find the p-value of this test statistic
# Can you see why this is the p-value?
p.val <- pnorm(-abs(test.stat))*2

print(p.val)
###

## Exercise 5.6
# type 1 error probability

# Sample size
n <- 10

type.I <- rep(0, 10000)
for(i in 1:10000) {
  # Generate some data
  x <- rnorm(n, mean = 3.14, sd = 1)
  
  # Compute test statistic
  test.stat <- (mean(x) - 3.14)/(1/sqrt(n))
  
  # Find the p-value of this test statistic
  p.val <- pnorm(-abs(test.stat))*2
  
  # Was this a type I error?
  type.I[i] <- (p.val<0.05)
}
mean(type.I)


## Exercise 5.7
# adapt the code so that you aren't simulating x from the null
# continue to test mean=3.14
# calculate type II error

# Sample size
n <- 10

type.II <- rep(0, 10000)
for(i in 1:10000) {
  # Generate some data
  x <- rnorm(n, mean = 4, sd = 1)
  
  # Compute test statistic
  test.stat <- (mean(x) - 3.14)/(1/sqrt(n))
  
  # Find the p-value of this test statistic
  p.val <- pnorm(-abs(test.stat))*2
  
  # Was this a type II error?
  type.II[i] <- (p.val>0.05)
}
mean(type.II)




