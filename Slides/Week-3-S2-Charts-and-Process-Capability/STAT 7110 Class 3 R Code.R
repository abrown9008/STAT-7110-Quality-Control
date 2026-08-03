## STAT 7110 Class 3 Code ##

library(tidyverse)

## Phase I X-bar and s Control Charts ##

library(readxl)

rings <- read_xlsx("Piston Rings.xlsx")

rings |>
  glimpse()

## The data are in the wide format we worked with last
## week ##

## X-bar ##

library(qcc)

sig_hat <- sd.xbar(rings[,-1]) # This give us s-bar/c4 

qcc(rings[,-1],type="xbar",std.dev=sig_hat,plot=TRUE)

## S ##

qcc(rings[,-1],type="S",plot=TRUE)

## Let's use the Phase I limits and center values in 
## Phase II using the Phase II rings data: ##

## Read in Phase II data ##

rings2 <- read_xlsx("Phase II rings data.xlsx")

## Save the Phase I limits and center values ##

## Limits ##

xbar_lims <- qcc(rings[,-1],type="xbar",std.dev=sig_hat,plot=FALSE)$limits

s_lims <- qcc(rings[,-1],type="S",plot=FALSE)$limits

## Center Values ##

xbar_center <- qcc(rings[,-1],type="xbar",std.dev=sig_hat,plot=FALSE)$center

s_center <- qcc(rings[,-1],type="S",plot=FALSE)$center

## Phase II X-bar ##

qcc(rings2[,-1],type="xbar",center=xbar_center,limits=xbar_lims,plot=TRUE)

## Phase II S ##

qcc(rings2[,-1],type="S",center=s_center,limits=s_lims,plot=TRUE)

## So we have evidence that the mean has shifted, but not the variance ##

## Phase I X-bar and s Control Charts w/Variable Sample Size ##

rings1 <- read_xlsx("Piston Rings - Variable Sample Size.xlsx")

rings1 |>
  glimpse()

## X-bar ##

numerator <- sum(
  
  apply(rings1[,-1],1,FUN=function(x){
  
  sum(!is.na(x))*mean(x,na.rm=T)
  
  })
  
)

denominator <- sum(
  
  apply(rings1[,-1],1,FUN=function(x){
    
    sum(!is.na(x))
    
  })
  
)

xdbar <- numerator/denominator

## Specify Sample Sizes ##

n <- apply(rings1[,-1],1,FUN=function(x){
  
  sum(!is.na(x))
  
})

## Estimate SD ##

## Note, RMSDF will give us the sbar from the notes ##

sbar <- sd.xbar(rings1[,-1],sizes=n,std.dev="RMSDF")

qcc(rings1[,-1],type="xbar",sizes = n,center=xdbar,plot=TRUE)

## S ##

qcc(rings1[,-1],type="S",center=sbar,plot=TRUE)

## Code for Generating Phase I s^2 Chart (Fixed Sample Size) ##

ssquared.p1 <- function(dat,alpha){
  
  sbar2 <- mean(apply(dat,1,var))
  UCL <- sbar2*qchisq(alpha/2,df=ncol(dat)-1,lower.tail=F)/(ncol(dat)-1)
  LCL <- sbar2*qchisq(alpha/2,df=ncol(dat)-1,lower.tail=T)/(ncol(dat)-1)
  s2 <- apply(dat,1,var)
  
  p <- ggplot() + geom_line(aes(x=seq(1,nrow(dat),by=1),y=UCL),color="red") +
                  geom_line(aes(x=seq(1,nrow(dat),by=1),y=LCL),color="red") +
                  geom_line(aes(x=seq(1,nrow(dat),by=1),y=s2),color="black") +
                  geom_point(aes(x=seq(1,nrow(dat),by=1),y=s2),color="black") +
                  labs(x="Time Points",y="Value of Measurement") +
                  theme_classic() +
                  ggtitle("S-Squared Control Chart") +
    theme(plot.title=element_text(hjust=0.5))
  
  print(p)
  
}

ssquared.p1(rings[,-1],alpha=0.0027)

## Control Charts for Individuals ##

mortgages <- read_xlsx("Mortgage Loan Costs.xlsx")

qcc(mortgages$Cost,type="xbar.one",plot=TRUE)

## Control Chart for Moving Range ##

library(ggQC)

mortgages |>
  ggplot(aes(x=Week, y = Cost)) +
  stat_mR() + ylab("Moving Range")

## Read in Battery Voltage Phase I Data ##

battery <- read_xlsx("Battery Voltage Phase I Data.xlsx")

battery |>
  glimpse()

## Generate Phase I X-Bar & S Charts ##

sig_hat <- sd.xbar(battery[,-1])

p1x <- qcc(battery[,-1],type="xbar",std.dev=sig_hat,plot=TRUE)

qcc(battery[,-1],type="S",plot=TRUE)

## Okay good! It looks like the process is in-control. Now, 
## let's calculate Cp ##

## Cp ##

process.capability(p1x,spec.limits=c(1.4,1.6),target=1.5,nsigmas=3)

## Cp = 1.61, which means that the difference between USL & LSL is 1.61 times
## the difference between UCL & LCL. This is good, but as we can see from the
## histogram, misleading. ##

## FNC ##
## From the chart, Exp<LSL + Exp>USL = 0.04% = 0.0004. This is good. ##

## Cpk ##

## From the chart, we can see that Cpk is 1.12, which is potentially okay,
## but certainly not as positive of a picture as Cp was giving to us ##

