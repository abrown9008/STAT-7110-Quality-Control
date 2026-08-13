## Introduction to Nonparametric Statistics ##

library(tidyverse)

## Let's run a simulation to estimate power for a one-population
## t-test under various conditions ##

## Using the football example, suppose 
## H0: mu <= 163
## H1: mu > 163 ##

## We can calculate this power directly, but let's simulate it instead ##

## Suppose the true mean is 170, and the standard deviation is 10 ##

## Let's first set a seed for reproducability ##

set.seed(12345)

## Now, let's save a vector of results ##

b <- 10000

results <- vector('double',b)

## Now, let's run the simulation ##

for(i in 1:b){
  
  ## Generate a sample of size 5 from a normal distribution with mean 170 and sd 10 ##
  
  x <- rnorm(5,mean=170,sd=10)
  
  ## Run a one-sample t-test ##
  
  t <- t.test(x,mu=163,alternative='greater')
  
  ## Save the p-value ##
  
  results[i] <- t$p.value
  
}

## Now, let's calculate the power ##

length(which(results < 0.05))/b

## So our result tells us that given this mean and alternative,
## and assuming that the true mean is 170, we would reject the null
## about 37% of the time (not too good!) ##

## Let's try a larger sample size ##

results <- vector('double',b)

for(i in 1:b){
  
  ## Generate a sample of size 50 from a normal distribution with mean 170 and sd 10 ##
  
  x <- rnorm(50,mean=170,sd=10)
  
  ## Run a one-sample t-test ##
  
  t <- t.test(x,mu=163,alternative='greater')
  
  ## Save the p-value ##
  
  results[i] <- t$p.value
  
}

## Now, let's calculate the power ##

length(which(results < 0.05))/b

## So our result tells us that given this mean and alternative,
## and assuming that the true mean is 170, we would reject the null
## about 99% of the time (much better!) ##

## Now what if our data did not come from a normal distribution 
## but instead came from the exponential distribution? ##

## Example of Exponential ##

## PDF of exponential is:
## f(x) = lambda * exp(-lambda*x) for x >= 0 ##
## where E[X] = 1/lambda and Var[X] = 1/lambda^2 ##

## So if the true mean is 170, this means lambda = 1/170 ##

hist(rexp(1000,rate=1/170))

## Decidedly non-normal! ##

## Let's run the simulation again ##

results <- vector('double',b)

for(i in 1:b){
  
  ## Generate a sample of size 5 from an exponential distribution with rate 1/170 ##
  
  x <- rexp(5,rate=1/170)
  
  ## Run a one-sample t-test ##
  
  t <- t.test(x,mu=163,alternative='greater')
  
  ## Save the p-value ##
  
  results[i] <- t$p.value
  
}

## Now, let's calculate the power ##

length(which(results < 0.05))/b

## So we went from 36% to 1% power! ##
## This is why it is important to check assumptions! ##

## Now, let's try a larger sample size ##

results <- vector('double',b)

for(i in 1:b){
  
  ## Generate a sample of size 50 from an exponential distribution with rate 1/170 ##
  
  x <- rexp(1000,rate=1/170)
  
  ## Run a one-sample t-test ##
  
  t <- t.test(x,mu=163,alternative='greater')
  
  ## Save the p-value ##
  
  results[i] <- t$p.value
  
}

## Now, let's calculate the power ##

length(which(results < 0.05))/b

## Also a severe decrease in power, even for a large sample size!! ##

## Sign Test Example ##

## Using the penguins data, let's take a random sample of 
## size 7. ##

install.packages('palmerpenguins')

data('penguins',package='palmerpenguins')

sample_penguins <- penguins |>
  na.omit() |>
  sample_n(7)

## Let's say I want to test:
## H_0: median = 3500 ##
## H_1: median != 3500 ##

## The test statistic is given by the number of observations
## greater than 3500 ##

T_Stat <- length(which(sample_penguins$body_mass_g > 3500))

## If we are using alpha = 0.05 and since this is a two-tailed 
## test, the p-value is given by ##

if(T_Stat > 7*0.5){
  
  p_value <- 2 * pbinom(T_Stat,size=7,prob=0.5,lower.tail=F)

  } else {
  
    p_value <- 2 * pbinom(T_Stat,size=7,prob=0.5,lower.tail=T)

}

print(p_value)

## Since 0.0156 < alpha, the data more substantially supports
## the alternative hypothesis ##

## Now let's run a simulation to estimate the power of the Sign Test 
## using the same conditions as the t-test before: ##

## Note:
## H0: median = 163 ##
## H1: median > 163 ##

## First, to make things easier, let's write a sign test 
## function ##

sign_test <- function(x,C){
  
  T_Stat <- length(which(x > C))
  
  if(T_Stat > length(x)*0.5){
  
    p_value <- 2 * pbinom(T_Stat,size=length(x),prob=0.5,lower.tail=F)
  
    } else {
  
    p_value <- 2 * pbinom(T_Stat,size=length(x),prob=0.5,lower.tail=T)
  
  }
  
  return(p_value)
  
}

np_results <- vector('double',b)

for(i in 1:b){
  
  ## Generate a sample of size 50 from an exponential distribution with rate 1/170 ##
  
  x <- rexp(50,rate=1/170)
  
  ## Run the Sign Test ##
  
  np_results[i] <- sign_test(x,log(2)/163)
  
}

## Now, let's calculate the power ##

length(which(np_results < 0.05))/b
  
## We went from 5% to 100%! Much better! ##

## Wilcoxon Signed Rank Test ##

## Let's run the WSRT on the penguin data like
## we did before with the sign test! ##

wsrt <- wilcox.test(sample_penguins$body_mass_g,mu=3500,
                    alternative='two.sided')

print(wsrt$p.value)
