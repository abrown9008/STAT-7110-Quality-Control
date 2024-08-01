## STAT 7110 - Power & Average Run Length Simulation ##

library(tidyverse)

## POWER ##

## Remember, statistical power is the probability of rejecting the null
## hypothesis given a specific value of the alternative hypothesis. For most
## of our common statistical tests, there is an exact formula for such 
## calculations. But what if there wasn't? How could we use software 
## to estimate power of a test in a given situation? ##

## SIMULATION ##

## Let's start with an independent means t-test ##

## Specify Group 1 & Group 2 "true" means ##

mu1 <- 15
mu2 <- 20

## Specify standard deviations (assume equal) ##

sd1 <- 4
sd2 <- 4

## Sample Size specification ##

n1 <- 25
n2 <- 25

## Alpha Level ##

alpha <- 0.05 

## Generate Two Independent Random Samples from Normal Distributions ##

Sample1 <- rnorm(n1,mu1,sd1)
Sample2 <- rnorm(n2,mu2,sd2)

## Perform t-test ##

t.test(Sample1,Sample2)

## Since p < 0.05, the test correctly rejected H0 since clearly
## mu1 =/= mu2 ##

## But power is a probability, no a "Yes/No" answer like we've gotten
## here. To turn it into a probability, we do the exact same set of 
## procedures we just did, except we do it a big number of times,
## say 100K. Then power is the proportion of those 100K iterations
## in which we rejected H0 ##

## Specify Number of Iterations ##

b <- 100000

## Save Logical Vector ##

power <- vector('logical',length=b)

## Set up loop ##

for(i in 1:b){
  
  ## Generate Random Samples ##
  
  Sample1 <- rnorm(n1,mu1,sd1)
  Sample2 <- rnorm(n2,mu2,sd2)
  
  ## Save P Value ##
  
  p_value <- t.test(Sample1,Sample2)$p.value
  
  ## Compare to Alpha & Save in power vector ##
  
  power[i] <- isTRUE(p_value < alpha)
  
}

## Power, then is the number of TRUE divided by b ##

sum(power)/b

## So basically, we have a lot of statistical power if 
## the values of mu & sigma are really the true values for
## either population ##

## Let's go through another example where the means are a little closer together ##

mu1 <- 15
mu2 <- 16

## Save new power vector ##

power2 <- vector('logical',length=b)

## Set up loop ##

for(i in 1:b){
  
  ## Generate Random Samples ##
  
  Sample1 <- rnorm(n1,mu1,sd1)
  Sample2 <- rnorm(n2,mu2,sd2)
  
  ## Save P Value ##
  
  p_value <- t.test(Sample1,Sample2)$p.value
  
  ## Compare to Alpha & Save in power vector ##
  
  power2[i] <- isTRUE(p_value < alpha)
  
}

## Calculate new power estimate ##

sum(power2)/b

## Much less likely to detect this smaller effect ##

## Now, let's see what happens when our data are non-normal ##

## Uniform Distribution, Laplace, & Exponential ##

## Visualize the Difference Between them ##

#install.packages('jmuOutlier')

x <- seq(-2,2,by=0.001)

u <- dunif(x,min=-2,max=2)
n <- dnorm(x)
l <- jmuOutlier::dlaplace(x)
e <- dexp(x)

## Stack on Top of Each other ##

dists <- tibble(Density = c(u,n,l,e),
                Distributions = c(rep("Uniform",length(x)),
                                  rep("Normal",length(x)),
                                  rep("Laplace",length(x)),
                                  rep("Exponential",length(x))),
                Domain = rep(x,4))

## Plot ##

dists |>
  ggplot(aes(x=Domain,y=Density,color=Distributions)) + geom_line() +
  theme_classic()

## Clearly, Uniform, Laplace, & Exponential have very different
## distributional shapes than Normal...
## Let's see how that affects statistical power ##

## Specify Parameters ##

mu1 <- 10
mu2 <- 14

sd1 <- 4
sd2 <- 4

n1 <- 30
n2 <- 30

alpha <- 0.05 

## Shrink iterations so this doesn't take forever to run ##

b <- 10000

power_norm <- vector('logical',length=b)
power_unif <- vector('logical',length=b)
power_lapl <- vector('logical',length=b)
power_expo <- vector('logical',length=b)

for(i in 1:b){
  
  ## Normal ##
  
  norm1 <- rnorm(n1,mu1,sd1)
  norm2 <- rnorm(n2,mu2,sd2)
  
  pv_norm <- t.test(norm1,norm2)$p.value
  
  ## Uniform ##
  
  ## Note, mean of unif = (max - min)/2 ##
  
  unif1 <- runif(n1,min=0,max=20)
  unif2 <- runif(n2,min=0,max=28)
  
  pv_unif <- t.test(unif1,unif2)$p.value
  
  ## Laplace ##
  
  lapl1 <- jmuOutlier::rlaplace(n1,mean=mu1,sd=sd1)
  lapl2 <- jmuOutlier::rlaplace(n2,mean=mu2,sd=sd2)
  
  pv_lapl <- t.test(lapl1,lapl2)$p.value
  
  ## Exponential ##
  
  ## Note, mean of exp = 1/rate ##
  
  expo1 <- rexp(n1,rate=1/mu1)
  expo2 <- rexp(n2,rate=1/mu2)
  
  pv_expo <- t.test(expo1,expo2)$p.value
  
  ## Compare to Alpha ##
  
  power_norm[i] <- isTRUE(pv_norm < alpha)
  power_unif[i] <- isTRUE(pv_unif < alpha)
  power_lapl[i] <- isTRUE(pv_lapl < alpha)
  power_expo[i] <- isTRUE(pv_expo < alpha)
  
}

## Compare Power ##

pwr_df <- tibble(Distribution = c("Normal","Uniform","Laplace","Exponential"),
                 Power = c(sum(power_norm)/b,sum(power_unif)/b,
                           sum(power_lapl)/b,sum(power_expo)/b))

pwr_df

## Pretty stark difference...notice CLT doesn't help uniform or exponential!!! ##

## Now, let's move into SPC. How can we simulate average run length?? 
## It's conceptually similar to what we just did, but with one key difference --
## The charting procedure stops once an OOC point is observed. Thus, we have 
## to incorporate a while loop ##

## A for loop will perform all iterations in the sequence (e.g., 1:b)
## A while loop will perform iterations until a condition is met ##

## Simple Example ## 

j <- 1

while(j <= 5){
  
  print(j)
  
  j <- j+1
  
}

## For us in SPC, we want to keep taking new samples and plotting our 
## charting statistic until the statistic exceeds a control limit ##

## Suppose this is Phase II analysis & mu0/sd0 are known ##

mu0 <- 1
sd0 <- 1
n0 <- 5
nsigmas <- 3

UCL <- mu0 + nsigmas*sd0/sqrt(n0)
LCL <- mu0 - nsigmas*sd0 /sqrt(n0)

## Suppose the mean has shifted from its in-control value of 1
## to an out-of-control value of 1.75 ##

ooc_mu <- 1.75

## Take random samples from OOC distribution until a point plots 
## OOC ##

xbar <- mean(rnorm(n0,mean=ooc_mu,sd=sd0))

j <- 1

while(xbar < UCL & xbar > LCL){
  
  xbar <- mean(rnorm(n0,mean=ooc_mu,sd=sd0))
  
  j <- j+1
  
}

## The run length is j...the number of times the while loop ran ##

j 

## A really long time! Now, how do we get average run length? 
## We have to perform the prior steps a big number of times! ##

b <- 10000

## Save run length vector ##

run_length <- vector('integer',length=b)

for(i in 1:b){
  
  ## Take random samples from OOC distribution until a point plots 
  ## OOC ##
  
  xbar <- mean(rnorm(n0,mean=ooc_mu,sd=sd0))
  
  j <- 1
  
  while(xbar < UCL & xbar > LCL){
    
    xbar <- mean(rnorm(n0,mean=ooc_mu,sd=sd0))
    
    j <- j+1
    
  }
  
  ## Save j in run_length ##
  
  run_length[i] <- j
  
}

## Run length distribution ##

hist(run_length)

## Notice the positive skewness!! ##

## Average Run Length ##

mean(run_length)

## Median Run Length may be more appropriate ##

median(run_length)

## Let's see how run length changes under different distributional assumptions ##

b <- 10000

mu0 <- 1
sd0 <- 1
n0 <- 5
nsigmas <- 3

UCL <- mu0 + nsigmas*sd0/sqrt(n0)
LCL <- mu0 - nsigmas*sd0 /sqrt(n0)

## Let's check IC-ARL ##

ooc_mu <- 1

run_length_norm <- vector('integer',length=b)
run_length_unif <- vector('integer',length=b)
run_length_lapl <- vector('integer',length=b)
run_length_expo <- vector('integer',length=b)

for(i in 1:b){
  
  ## Normal ##
  
  norm_xbar <- mean(rnorm(n0,mean=ooc_mu,sd=sd0))
  
  j_norm <- 1
  
  while(norm_xbar < UCL & norm_xbar > LCL){
    
    norm_xbar <- mean(rnorm(n0,mean=ooc_mu,sd=sd0))
    
    j_norm <- j_norm + 1
    
  }
  
  run_length_norm[i] <- j_norm
  
}
  
for(i in 1:b){
  
  ## Uniform ##
  
  unif_xbar <- mean(runif(n0,min=1,max=3))
  
  j_unif <- 1
  
  while(unif_xbar < UCL & unif_xbar > LCL){
    
    unif_xbar <- mean(runif(n0,min=1,max=3))
    
    j_unif <- j_unif + 1
    
  }
  
  run_length_unif[i] <- j_unif
  
}

for(i in 1:b){
  
  ## Laplace ##
  
  lapl_xbar <- mean(jmuOutlier::rlaplace(n0,mean=ooc_mu,sd=sd0))
  
  j_lapl <- 1
  
  while(lapl_xbar < UCL & lapl_xbar > LCL){
    
    lapl_xbar <- mean(jmuOutlier::rlaplace(n0,mean=ooc_mu,sd=sd0))
    
    j_lapl <- j_lapl + 1
    
  }
  
  run_length_lapl[i] <- j_lapl
  
}

for(i in 1:b){
  
  ## Exponential ##
  
  expo_xbar <- mean(rexp(n0,rate=1/ooc_mu))
  
  j_expo <- 1
  
  while(expo_xbar < UCL & expo_xbar > LCL){
    
    expo_xbar <- mean(rexp(n0,rate=1/ooc_mu))
    
    j_expo <- j_expo + 1
    
  }
  
  run_length_expo[i] <- j_expo
  
}

## Average Length Comparison ##

arlz <- tibble(Distribution = c("Normal","Uniform","Laplace","Exponential"),
               ARL = c(mean(run_length_norm),mean(run_length_unif),
                       mean(run_length_lapl),mean(run_length_expo)))

arlz
