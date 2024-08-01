## STAT 7110 Class 6 Code ##

library(tidyverse)

## Read in Circuit Board Phase I Data ##

circuits <- readxl::read_xlsx("Class5.xlsx")

## Plot Phase I C-Chart ##

circuit_chart <- qcc::qcc(circuits$`Number of Nonconformities`,
                          type="c")

## Two OOC Points were Detected ##

ooc_points <- circuit_chart$violations$beyond.limits

circuits1 <- circuits[-ooc_points,]

circuit_chart1 <- qcc::qcc(circuits1$`Number of Nonconformities`,
                           type="c")

## Plot OC Curve ##

oc_c <- qcc::oc.curves.c(circuit_chart1)

## Determine which points are closest to 0.20 ##

pointz <- abs(0.20 - oc_c)

pointz <- pointz[order(pointz)][1:2]

names(pointz)

## Magnitude of Shifts ##

38/circuit_chart1$center
4/circuit_chart1$center

## ARL1 ##

1/(1-0.20)

## Find OC Curve for U Chart using Supply Chain Data ##

## Read In Supply Chain Data ##

sc <- readxl::read_xlsx("Class5.xlsx",
                        sheet=5)

## Plot U Chart ##

sc_chart <- qcc::qcc(sc$`Total Number of Errors`,
                     type="u",sizes=sc$`Sample Size`)

## Okay great! The problem here is that there isn't a built-in function ##
## in the qcc package to plot the OC Curve, so we have to do it manually ##

cu <- sc_chart$center
nlcl <- ceiling(50*sc_chart$limits[1])
nucl <- floor(50*sc_chart$limits[2])

betas <- vector("double",length=51)

for(i in 1:length(betas)){
  
  betas[i] <- ppois(nucl,lambda=i-1) - ppois(nlcl,lambda=i-1)
  
}

ggplot() + geom_line(aes(x=seq(0,50),y=betas)) + 
  geom_vline(xintercept = 50*cu,linetype = "dashed") +
  geom_segment(aes(x=0,y=0.2,xend=length(betas)-1,yend=0.20),color='red') +
  labs(x = "nU",
       y = quote(beta),
       title = "OC Curve for U Chart") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50))
