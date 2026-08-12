## STAT 7110 CUSUM & EWMA Code ##

library(tidyverse)
library(readxl)
library(qcc)

## Read in Soda Bottles Data ##

bottles <- read_xlsx("Soda Bottles.xlsx")

bottles |>
  glimpse()

## Calculate Sample Means & Overall SD ##

bottles_means <- rowMeans(bottles)
sigma_hat <- sd.xbar(bottles,std.dev="UWAVE-SD")

mu0 <- 16

## Now, let's standardize the row means: ##

z_means <- (bottles_means - mu0)/(sigma_hat/sqrt(5))

## Let's calculate the CUSUM values manually before using the 
## qcc function: ##

k <- 0.5
h <- 5

CUSUM_df <- tibble(Means = z_means)

## Calculate C+ and C- values prior to taking maximums ##

CUSUM_df <- CUSUM_df |>
  mutate(C_plus = Means-k,
         C_minus = -k-Means)

## Create a loop ##

c_plus <- vector('double',length=nrow(CUSUM_df))
c_minus <- vector('double',length=nrow(CUSUM_df))

for(i in 1:length(c_plus)){
  
  if(i==1){
    
    c_plus[i] <- max(0,CUSUM_df$C_plus[i])
    c_minus[i] <- max(0,CUSUM_df$C_minus[i])
    
  } else {
    
    c_plus[i] <- max(0,CUSUM_df$C_plus[i] + c_plus[i-1])
    c_minus[i] <- max(0,CUSUM_df$C_minus[i] + c_minus[i-1])
    
  }
  
}

## Plot ##

CUSUM_df <- CUSUM_df |>
  mutate(C_plus1 = c_plus,
         C_minus1 = c_minus)

CUSUM_df |>
  ggplot(aes(x=1:nrow(CUSUM_df))) +
  geom_line(aes(y=C_plus1),color="blue") +
  geom_point(aes(y=C_plus1),color='blue') +
  geom_line(aes(y=C_minus1),color="red") +
  geom_hline(yintercept=h,linetype="dashed") +
  labs(title="CUSUM Chart for Soda Bottles",
       x="Sample Number",
       y="CUSUM Value") +
  theme_minimal()

## Now how do we do this using the qcc package? ##

## Note, qcc wants to standardize the data, so we need to
## be mindful of this. ##

cusum(bottles,sizes=5,center=mu0,se.shift=k*2,
      decision.interval = h,plot=T)

## This let's us know that the process is not in control
## for mu0 = 16 as the upper CUSUM values exceed the decision interval. ##

## Let's go through the same process for EWMA ##

## Calculate the EWMA values manually before using the qcc function: ##

lambda <- 0.20
L <- 2.962

EWMA_df <- tibble(Means = rowMeans(bottles))

## Calculate the EWMA values ##

ewma_values <- vector('double',length=nrow(EWMA_df))

for(i in 1:length(ewma_values)){
  
  if(i==1){
    
    ewma_values[i] <- lambda*EWMA_df$Means[i] + (1-lambda)*mu0
    
  } else {
    
    ewma_values[i] <- lambda*EWMA_df$Means[i] + (1-lambda)*ewma_values[i-1]
    
  }
  
}

## Calculate SD for EWMA ##

sd_ewma <- vector('double',length=nrow(EWMA_df))

for(i in 1:length(sd_ewma)){
  
  sd_ewma[i] <- sigma_hat^2/5*(lambda/(2-lambda))*(1-(1-lambda)^(2*i))
  
}

## Calculate the UCL and LCL ##

EWMA_df <- EWMA_df |>
  mutate(`EWMA Values` = ewma_values,
         Variance = sd_ewma,
         UCL = mu0 + L*sqrt(Variance),
         LCL = mu0 - L*sqrt(Variance)
  )

## Plot ##

EWMA_df |>
  ggplot(aes(x=1:nrow(EWMA_df))) +
  geom_line(aes(y=`EWMA Values`),color="blue") +
  geom_point(aes(y=`EWMA Values`),color="blue",size=1.5) +
  geom_line(aes(y=UCL),color="red") +
  geom_line(aes(y=LCL),color="red") +
  geom_hline(yintercept=mu0,linetype="dashed") +
  labs(title="EWMA Chart for Soda Bottles",
       x="Sample Number",
       y="EWMA Value") +
  theme_minimal()

ewma(bottles,center=16,lambda=0.2,nsigmas=2.962)

## The main thing to point out here with both the CUSUM and EWMA charts is
## that qcc assumes you're working with the sample mean. So, you need to
## realize that Var[X-bar] = Var[X]/n. ##

## Let's go through another example using the customer hold data (Sheet 1 of EWMA Datasets) ##
## Here, n = 1, so we don't need to worry about the sample mean ##
## mu0 = 10 minutes and sigma = 1 minute ##

customer_hold <- read_xlsx("EWMA Datasets.xlsx",sheet=1)

customer_hold |>
  glimpse()

## QCC ##

ewma(customer_hold$Time,center=10,sigma = 1,lambda=0.2,nsigmas=2.962)

## Let's try this with CUSUM ##

cusum(customer_hold$Time,sizes=1,center=10,sigma=1,
      se.shift=k*2,decision.interval = 5,plot=T)

## Let's try this with the second dataset (Sheet 2 of EWMA Datasets) ##
## Suppose we're the Scranton branch of Dunder-Mifflin. 
## We want to monitor the amount of fuel our warehouse delivery trucks use. 
## In the second sheet of this week's class data, we have the past 24 months fuel costs. 
## Let's estimate the mean and standard deviation and see if the process is in control
## using both CUSUM & EWMA ##

fuel_costs <- read_xlsx("EWMA Datasets.xlsx",sheet=2)

fuel_costs |>
  glimpse()

## Estimate process mean ##

mu0 <- mean(fuel_costs$Cost)

## Estimate process standard deviation ##

sigma_hat <- sd(fuel_costs$Cost)

## CUSUM ##

cusum(fuel_costs$Cost,sizes=1,center=mu0,sigma=sigma_hat,
      se.shift=k*2,decision.interval = 5,plot=T)

## It appears that the process is in control using the estimated
## mean and sd ##

## EWMA ##

ewma(fuel_costs$Cost,center=mu0,sigma=sigma_hat,lambda=0.2,nsigmas=2.962)

## Same for EWMA!! ##

