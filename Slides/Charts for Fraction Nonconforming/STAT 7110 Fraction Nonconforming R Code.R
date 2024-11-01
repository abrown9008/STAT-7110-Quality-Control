## STAT 7110 Fraction Nonconforming Code ##

library(tidyverse)
library(readxl)
library(qcc)

## P-Chart ##

enrollment <- read_xlsx("Enrollment.xlsx")

enrollment |>
  glimpse()

## Phase I Control Chart ##

enroll_chart <- qcc(enrollment$`Number Not Re-Enrolling`,
                    type="p",
                    sizes=enrollment$`Sample Size`,
                    plot=TRUE)

## Phase I Control Chart - Orange Juice Cans ##

ojc <- read_xlsx("Orange Juice Cans.xlsx")

ojc_chart <- qcc(ojc$`Number of Nonconforming Cans`,
                 type="p",
                 sizes=ojc$`Sample Size`,
                 plot=TRUE)

## We have two OOC points -- Let's remove them and recalculate the limits ##

ooc_points <- ojc_chart$violations$beyond.limits

ojc1 <- ojc[-ooc_points,]

ojc_chart1 <- qcc(ojc1$`Number of Nonconforming Cans`,
                  type="p",sizes=ojc1$`Sample Size`,
                  plot=TRUE)

## Now we have another OOC point -- Let's remove it and recalculate ##

ooc_points1 <- ojc_chart1$violations$beyond.limits

ojc2 <- ojc1[-ooc_points1,]

ojc_chart2 <- qcc(ojc2$`Number of Nonconforming Cans`,
                  type="p",sizes=ojc2$`Sample Size`,
                  plot=TRUE)

## Okay great! The process now plots in control so we can move on to Phase II ##

ojcp2 <- read_xlsx("Orange Juice Cans.xlsx",sheet=2)

## Take the Phase I Limits ##

ojc_chart2$limits

uclp <- ojc_chart2$limits[1,2]
lclp <- ojc_chart2$limits[1,1]
pbar <- ojc_chart2$center

ojc_chartp2 <- qcc(ojcp2$`Number of Nonconforming Cans`,
                   type="p",sizes=ojcp2$`Sample Size`,
                   limits = c(lclp,uclp),
                   center = pbar,
                   plot=TRUE)

## Notice here, while none of the points plot OOC, we definitely have evidence ##
## That the proportion non-conforming may have shifted downward ##

## Let's test it using a two-sample Z test! ##

x1 <- sum(ojc2$`Number of Nonconforming Cans`)
x2 <- sum(ojcp2$`Number of Nonconforming Cans`)
n1 <- sum(ojc2$`Sample Size`)
n2 <- sum(ojcp2$`Sample Size`)

prop.test(c(x1,x2),c(n1,n2),alternative="two.sided",
          correct=FALSE)

## Since our p-value is quite small, we have significant evidence to suggest ##
## that the proportion in Phase II may be significantly different than our phase I ##
## Proportion ##

## Building a Function to Calculate Sample Size for P-Chart ##

pchart_ss <- function(prob,p){
  
  n <- ceiling(log(1-prob)/log(1-p))
  
  return(n)
  
}

pchart_ss(0.95,0.01)

## Function for Sample Size Required to Have a Positive Control Limit ##

nonzero_lcl_ss <- function(p,L){
  
  n <- ceiling(L^2*(1-p)/p)
  
  return(n)
  
}

nonzero_lcl_ss(0.01,3)

## Variable Sample Size P-Charts ##

## Read in PO Data ##

pos <- read_xlsx("POs.xlsx")

po_chart <- qcc(pos$`Incorrect POs`,type="p",
                sizes=pos$`Sample Size`,
                plot=T)

## Determining Out of Control Points ##

ooc_points2 <- po_chart$violations$beyond.limits

## Remove the Points and Re-plot Chart ##

pos1 <- pos[-ooc_points2,]

po_chart1 <- qcc(pos1$`Incorrect POs`,type="p",
                 sizes=pos1$`Sample Size`,
                 plot=T)

## Calculate ARL0 for the Orange Juice Can Example ##

uclp <- ojc_chart2$limits[1,2]
lclp <- ojc_chart2$limits[1,1]
pbar <- ojc_chart2$center
n <- ojc_chart2$sizes[1]

## First we need to calculate alpha ##

## alpha = P[p > UCL or p < LCL | p = pbar] ##

## alpha = P[p > UCL | p = pbar] + P[p < LCL | p = pbar] ##

## alpha = 1 - P[p < UCL | p = pbar] + P[p < LCL | p = pbar] ##

## So now, since we know phat ~ N(mu = pbar,sigma = sqrt(pbar(1-pbar)/n)), we can use the normal distribution
## to find alpha ##

alpha <- 1 - pnorm(uclp,mean=pbar,sd=sqrt(pbar*(1-pbar)/n)) + pnorm(lclp,pbar,sqrt(pbar*(1-pbar)/n))

## Now we can calculate ARL0 ##

arl0 <- 1/alpha
arl0

## Estimate beta for a shift from 0.2081 to p1 = 0.1108 ##

## beta = P[LCL < phat < UCL | p = p1] ##
## beta = P[phat < UCL | p = p1] - P[phat < LCL | p = p1] ##
## This implies that the mean is now p1 and the standard deviation is sqrt(p1(1-p1)/n) ##
## So we can again use pnorm to help us calculate beta ##

p1 <- 0.1108

beta <- pnorm(uclp,p1,sqrt(p1*(1-p1)/n)) - pnorm(lclp,p1,sqrt(p1*(1-p1)/n))
beta

## Plotting OC Curve ##

oc.curves.p(ojc_chart2)

## ARL1 ##

arl1 <- 1/(1-beta)

arl1

## Approximately 22 Samples ##
