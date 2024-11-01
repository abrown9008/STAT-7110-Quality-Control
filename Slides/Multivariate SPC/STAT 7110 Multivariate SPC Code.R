## STAT 7110 Multivariate SPC R Code ##

library(tidyverse)
library(readxl)
library(qcc)

## Monitoring 3 Soda Can Filling Lines ##

## Read in Phase I Canning Data as List ##

## Get Sheet Names ##

sn <- excel_sheets("Can Volume Data.xlsx")

cans <- lapply(sn,FUN=function(x){
  
  d <- read_xlsx("Can Volume Data.xlsx",sheet=x) %>%
    dplyr::select(-`Sample Number`,-`Line Number`)
  
  return(d)
  
})

names(cans) <- paste("Line",1:3,sep="")

## Use mqcc to Plot the Phase 1 Chart! ##

t2 <- mqcc(cans,type='T2')

## Note default alpha = 1 - (1 - 0.0027)^p and in this case p = 3 ##

1 - (0.9973)^3

## Notice, t2 contains the estimated mean vector and v-cov matrix ##

t2$center

t2$cov

## Read in Phase II Can Volume Data ##

sn2 <- excel_sheets("Can Volume Phase II Data.xlsx")

cans2 <- lapply(sn2,FUN=function(x){
  
  d <- read_xlsx("Can Volume Phase II Data.xlsx",sheet=x) %>%
    dplyr::select(-`Sample`,-`Line Number`)
  
  return(d)
  
})

names(cans2) <- paste("Line",1:3,sep="")

## Use mqcc to Plot the Phase II Chart! ##

p2 <- mqcc(cans2,type="T2",
           center=t2$center,
           cov=t2$cov,
           limits=F,pred.limits=T,
           plot=T)

## We can see the process is way out of control immediately!
## What is the new mean vector? ##

lapply(cans2,FUN=function(x){
  
  return(mean(rowMeans(x)))
  
})

## So Line 1 & Line 2 seem fine. Line 3 is the problem. ##
## Also notice, despite the fact that line 3 experienced
## a decrease, our plotting statistic is above the upper
## control limit. This is because T2 is strictly positive
## regardless of the direction of the shift, much like 
## the F-statistic in ANOVA. ##

## Just like ANOVA, when we have a significant omnibus F-test,
## we generally perform a post-hoc analysis. Here, our post-hoc 
## analysis involves the plotting of individual x-bar & s charts ##

## Let's extract the phase I mean and sd ##

## c4 for n = 10 is 0.9727 ##

c4 <- 0.9727

line1_xbar <- t2$center[1]
line2_xbar <- t2$center[2]
line3_xbar <- t2$center[3]

line1_sd <- sqrt(t2$cov[1,1])/c4
line2_sd <- sqrt(t2$cov[2,2])/c4
line3_sd <- sqrt(t2$cov[3,3])/c4

## Calculate UCL & LCL ##

line1_ucl <- line1_xbar + 3*line1_sd
line1_lcl <- line1_xbar - 3*line1_sd

line2_ucl <- line2_xbar + 3*line2_sd
line2_lcl <- line2_xbar - 3*line2_sd

line3_ucl <- line3_xbar + 3*line3_sd
line3_lcl <- line3_xbar - 3*line3_sd

## Now, let's plot the x-bar charts ##

## Line 1 ##

xbar1 <- qcc(rowMeans(cans2$Line1),
             type="xbar",
             center=line1_xbar,
             sigma=line1_sd,
             sizes=10,
             limits=c(line1_lcl,line1_ucl))

## Line 2 ##

xbar2 <- qcc(rowMeans(cans2$Line2),
             type="xbar",
             center=line2_xbar,
             sigma=line2_sd,
             sizes=10,
             limits=c(line2_lcl,line2_ucl))

## Line 3 ##

xbar3 <- qcc(rowMeans(cans2$Line3),
             type="xbar",
             center=line3_xbar,
             sigma=line3_sd,
             sizes=10,
             limits=c(line3_lcl,line3_ucl))

## If it wasn't obvious from the T2 chart, we can very clearly
## see here that Line 3 is out of control. ##

## Let's try the same Phase I & Phase II example with the MEWMA Chart ##

## Use Phase I estimates of mean and v-c matrix
## from Hotelling's T2 ##

mean_vec <- t2$center
cov_mat <- t2$cov

## Must arrange data in the right format and center ##

p1_means <- as.matrix(
  
  sapply(cans,FUN=function(x){
    
    return(rowMeans(x))
  
  })

)

p1_means_centered <- t(
  
  apply(p1_means,1,FUN=function(x){
  
  return(x - mean_vec)
  
  })
  
)

p2_means <- as.matrix(
  
  sapply(cans2,FUN=function(x){
  
      return(rowMeans(x))
  
  })

)

p2_means_centered <- t(
  
  apply(p2_means,1,FUN=function(x){
  
    return(x - mean_vec)
  
  })
  
)

## Let's set lambda to 0.1 ##

lambda <- 0.10

## Write Function to Calculate Sigma_Z ##

sigma_z <- function(lambda,vc_matrix,t){
  
  c <- (lambda/(2-lambda))*(1-(1-lambda)^(2*t))
  
  return(c*vc_matrix)
  
}

## Write Function to Calculate Z Statistic ##

Z1 <- matrix(0,nrow=nrow(p1_means),ncol=3)

for(i in 1:nrow(p1_means)){
  
  if(i == 1){
    
    Z1[i,] <- lambda*t(p1_means_centered[i,])
    
  } else {
    
    Z1[i,] <- lambda*t(p1_means_centered[i,]) + (1-lambda)*Z1[i-1]
    
  }
  
}

Z2 <- matrix(0,nrow=nrow(p2_means),ncol=3)

for(i in 1:nrow(p2_means)){
  
  if(i == 1){
    
    Z2[i,] <- lambda*t(p2_means_centered[i,])
    
  } else {
    
    Z2[i,] <- lambda*t(p2_means_centered[i,]) + (1-lambda)*Z2[i-1]
    
  }
  
}

## Write Function to Calculate MEWMA Statistic ##

mewma1 <- vector('double',length=nrow(p1_means))
mewma2 <- vector('double',length=nrow(p2_means))

for(i in 1:nrow(p1_means)){
  
  S1 <- sigma_z(lambda,cov_mat,i)
  
  mewma1[i] <- t(Z1[i,])%*%solve(S1)%*%Z1[i,]
  
}

for(i in 1:nrow(p2_means)){
  
  S2 <- sigma_z(lambda,cov_mat,i)
  
  mewma2[i] <- t(Z2[i,])%*%solve(S2)%*%Z2[i,]
  
}

## Choose H = 15.26 as control limit and plot Phase I ##

ggplot() +
  geom_hline(yintercept=15.26,linetype="dashed",color='red') +
  geom_point(aes(x=1:nrow(p1_means),y=mewma1),color='steelblue') +
  geom_line(aes(x=1:nrow(p1_means),y=mewma1),color='steelblue') +
  labs(title="Phase I MEWMA Chart for Soda Can Filling Lines",
       x="Sample Number",
       y="MEWMA Statistic") +
  theme_classic()

ggplot() +
  geom_hline(yintercept=15.26,linetype="dashed",color='red') +
  geom_point(aes(x=1:nrow(p1_means),y=mewma2),color='steelblue') +
  geom_line(aes(x=1:nrow(p1_means),y=mewma2),color='steelblue') +
  labs(title="Phase II MEWMA Chart for Soda Can Filling Lines",
       x="Sample Number",
       y="MEWMA Statistic") +
  theme_classic()

## Again, we can see the process immediately goes out of control. ##

## Let's use individual EWMA control charts to determine which one
## went OOC ##

## If we're using lambda = 0.1, let's use L = 2.814 ##

L <- 2.814

## Line 1 ##

ewma(cans2$Line1,
     center=t2$center[1],
     lambda=0.1,sigma=sqrt(t2$cov[1,1]/c4),
     nsigmas=L)

## Line 2 ##

ewma(cans2$Line2,center=t2$center[2],lambda=0.1,sigma=sqrt(t2$cov[2,2]/c4),
     nsigmas=L)

## Line 3 ##

ewma(cans2$Line3,center=t2$center[3],lambda=0.1,sigma=sqrt(t2$cov[3,3]/c4),
     nsigmas=L)

## So again, very clearly, line 3 is the culprit ##