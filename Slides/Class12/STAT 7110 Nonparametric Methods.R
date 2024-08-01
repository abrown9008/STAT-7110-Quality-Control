## STAT 7110 Nonparametric Methods R Code ## 

library(tidyverse)

## Read in Data ##

dentist <- readxl::read_xlsx("dentist.xlsx")

## Specify Median ##

mu0 <- 2.89

## Calculate Wilcoxon Signed Rank Test Statistic for Each Sample ##

## Compute Rowwise Ranks ##

ranks <- t(apply(dentist[,-7],1,rank))

## Determine Signs ##

signs <- t(apply(dentist[,-7],1,FUN=function(x){
  
  k <- ifelse(x > mu0,1,-1)
  
  return(sign(k))
  
}))

## Put them together to obtain WSRT! ##

sr <- rowSums(signs*ranks)

## Calculate Tt's ##

k <- 15
h <- 6

subz <- sr - k

Tts <- cumsum(subz)

## Determine Pts ##

min_tts <- vector("double",length=52)

for(i in 1:52){
  
  min_tts[i] <- min(0,Tts[1:i])
  
}

Pts <- Tts - min_tts

## Plot Chart ##

ggplot() + geom_line(aes(x=seq(52),y=h),color="black") +
  geom_point(aes(x=seq(52),y=Pts),color="red") +
  geom_line(aes(x=seq(52),y=Pts),color="red") + 
  labs(x = "Week",
       y = "CUSUM Value") +
  theme_classic() +
  ggtitle("Bakir & Reynolds' NP CUSUM for Time-until-Repeat Visit",
          subtitle = "Jeremy Jamm, DDS") +
  theme(plot.title=element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50)) + 
  scale_x_continuous(breaks = seq(52))

## Okay, so let's determine which points are >= h and determine their medians ##

which(Pts >= h)

apply(dentist[which(Pts >= h),-7],1,median)

mu0

## Let's compare to a Shewhart X-bar & R Chart ##

qcc::qcc(dentist[,-7],type="xbar",plot=T,center=4.17)

## B&R detected a shift at point 3 whereas the x-bar chart took much longer ##

## Now let's look at an example using ordinal, Likert-scale items ##

## Read in Data ##

dunkin <- readxl::read_xlsx("dunkin.xlsx")

## Specify Median ##

mu0 <- 3

## Calculate Wilcoxon Signed Rank Test Statistic for Each Sample ##

## Compute Rowwise Ranks ##

ranks <- t(apply(dunkin[,-7],1,rank))

## Notice, with integers, the average rank is assigned ##

## Determine Signs ##

## Notice here, we're going to add a >= since we are working
## with an ordinal (read, discrete) variable ##

signs <- t(apply(dunkin[,-7],1,FUN=function(x){
  
  k <- ifelse(x >= mu0,1,-1)
  
  return(sign(k))
  
}))

## Put them together to obtain WSRT! ##

sr <- rowSums(signs*ranks)

## Calculate Tt's ##

k <- 3
h <- 18

subz <- sr - k

Tts <- cumsum(subz)

## Determine Pts ##

min_tts <- vector("double",length=30)

for(i in 1:30){
  
  min_tts[i] <- min(0,Tts[1:i])
  
}

Pts <- Tts - min_tts

## Plot Chart ##

ggplot() + geom_line(aes(x=seq(30),y=h),color="black") +
  geom_point(aes(x=seq(30),y=Pts),color="red") +
  geom_line(aes(x=seq(30),y=Pts),color="red") + 
  labs(x = "Day",
       y = "CUSUM Value") +
  theme_classic() +
  ggtitle("Bakir & Reynolds' NP CUSUM for Dunkin Satisfaction",
          subtitle = "Casey Affleck, Managing Partner") +
  theme(plot.title=element_text(hjust=0.50),
        plot.subtitle = element_text(hjust=0.50)) + 
  scale_x_continuous(breaks = seq(30))

## An OOC point at t = 5? ## 

apply(dunkin,1,median)

mu0

## Let's compare to Shewhart ##

qcc::qcc(dunkin[,-7],type="xbar",plot=T,center=3)

## Shewhart is leading us to conclude that the process is IC when
## B&R is leading us to conclude that the process is OOC...which is right? ##

## When a nonparametric test is significant but the parametric test is not,
## or vice versa, it is best to rely on the NP test results ##