## Exercise 5.8, 5.9, 5.10
# factorials

factorial <- factorial

factorial_new <- function(n) {
  if (n == 0){
    return(1)
  } else {
    x <- 1
    for (k in 1:n){
      x <- x*k 
    }
    return(x)
  }
}

factorial_new(0)
factorial_new(1)
factorial_new(2)
factorial_new(3)
factorial_new(4)
factorial_new(100)

factorial(100)
prod(1:100)


factorial_new2 <- function(n) {
  if (n == 0){
    return(1)
  } else { 
    return(n*factorial_new2(n-1))
  }
}


factorial_new3 <- function(n) {
  if (n==0){return(1)} else{return(prod(1:n))}
}


## Exercise 5.11, 5.12
n <- 100
exact <- rep(0,n)
stirling <- rep(0,n)

for (k in 1:n) {
  exact[k] <- factorial(k)
  stirling[k] <- (k/exp(1))^k * sqrt(2*pi*k)
}

error <- abs(stirling-exact)/exact


plot(1:n, error, xlab='k')
plot(1:n, log10(error), xlab='k')


### Monte Carlo Hypothesis Testing

table(sample(1:6, 60, replace = TRUE))


## Exercise 5.13
# biased dice

table(sample(1:6, prob=c(rep(0.15,5),0.25), 60, replace=TRUE))

# mean, median are test statistics here

## Exercise 5.14
# monte carlo hyp test

x <- table(sample(1:6, 60, replace = TRUE, prob = c(0.15, 0.15, 0.15, 0.15, 0.15, 0.25)))

obs.test.stat <- sum((x-10)^2)

test.stat <- rep(0, 10000)
for(i in 1:10000) {
  sim <- table(sample(1:6, 60, replace = TRUE))
  
  test.stat[i] <- sum((sim-10)^2)
}
mean(test.stat > obs.test.stat)


## Exercise 5.15
# calculate the exact p-value for one-sided test for N=107 vs N<107,
# given 324 is the real value
# alpha = 0.05

p.value <- prod((61:57)/(107:103))
p.value

alpha <- 0.05

p.value <= alpha # FALSE


## Exercise 5.16

n <- 5
x <- c(61, 19, 56, 24, 16)
N <- 107

obs.test.stat <- max(x)

m <- 100000

for (i in 1:m) {
  star <- sample(1:N, n, replace=FALSE)
  test.stat[i] <- max(star)
}

empirical.p.value <- sum(test.stat<=obs.test.stat)/m
empirical.p.value


## Exercise 5.17

empirical.p.values <- rep(0, 1000)
for (k in 1:1000) {
  n <- 5
  x <- c(61, 19, 56, 24, 16)
  N <- 107
  
  obs.test.stat <- max(x)
  
  m <- 100000
  
  for (i in 1:m) {
    star <- sample(1:N, n, replace=FALSE)
    test.stat[i] <- max(star)
  }
  
  empirical.p.value <- sum(test.stat<=obs.test.stat)/m
  empirical.p.values[k] <- empirical.p.value
}

mean_p <- mean(empirical.p.values)


library("MASS")
truehist(empirical.p.values, xlim=c(0.03, 0.08))
abline(v=0.05,col='red',lwd=4)
abline(v=mean_p,col="purple",lwd=4)


## SOLUTION

# Tank data and null
x <- c(61, 19, 56, 24, 16)
N0 <- 107

# Wrap the important code from earlier in an outer for loop and
# fill up the pvals vector
pvals <- rep(0, 1000)
for(i in 1:1000) {
  # Simulate M sets of data of size 5, assuming the null is true
  M <- 1000
  t <- rep(0, M)
  for(j in 1:M) {
    # Sampling is from the set of values {1,...,N0} without replacement
    z <- sample(1:N0, 5)
    t[j] <- max(z)
  }
  # Calculate empirical p-value
  pvals[i] <- sum(t <= max(x)) / M
}

library("MASS")
truehist(pvals, xlim = c(0.03, 0.08))
abline(v = 0.05, col = "red", lwd = 4)
abline(v = 0.05596113, col = "purple", lwd = 4)



## Exercise 5.18

p_wrong_1000 <- 100*sum(pvals<=alpha)/1000
p_wrong_100000 <- 100*sum(empirical.p.values<=alpha)/100000
p_wrong_1000
p_wrong_100000
