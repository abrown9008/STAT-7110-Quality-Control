## STAT 7110 Class 2 Code ##

library(tidyverse)

## Phase I Monitoring of Hard Break Process ##

library(readxl)

flow <- read_xlsx("Wafer Data.xlsx")

flow |>
  glimpse()

## These data are organized in "wide format" 
## where each row represents a point in time 
## rather than a unique observation.
## For the qcc package, wide data is typically
## what we need. ##

## Setting Up X-bar Chart ##

library(qcc)

qcc(flow[,-1],type="xbar",plot=TRUE)

## R Chart ##

qcc(flow[,-1],type="R",plot=TRUE)

## Phase II Control Charts ##

## X-bar ##

## Saving Phase I Information ##

p1 <- qcc(flow[,-1],type="xbar",plot=FALSE)

cl <- p1$center

UCLx <- p1$limits[2]

LCLx <- p1$limits[1]

## Supposing flow contained new data, we can plot our 
## phase II data by: ##

qcc(flow[,-1],type="xbar",center=cl,limits=c(LCLx,UCLx),plot=TRUE)

## R Chart ##

p2 <- qcc(flow[,-1],type="R",plot=FALSE)

cl_r <- p2$center

UCLr <- p2$limits[2]

LCLr <- p2$limits[1]

qcc(flow[,-1],type="R",center=cl_r,limits=c(LCLr,UCLr),plot=TRUE)

## Estimating Process Capability ##

process.capability(p1,spec.limits=c(1,2),target=1.50,std.dev=p1$std.dev,
                   nsigmas=3)

## Calculating Fraction Non-Conforming ##

pnorm(1,mean=p1$center,sd=p1$std.dev) + pnorm(2,mean=p1$center,sd=p1$std.dev,
                                              lower.tail=FALSE)

## Calculating OC-Curve for X-bar Chart ##

qcc::oc.curves.xbar(p1,n=c(5,10,15,20))

## We can see here that as the sample size gets larger
## we have an increased probability of detecting a shift
## more quickly. ##