## STAT 7110 Class 3 Code ##

library(tidyverse)

## Phase I X-bar and s Control Charts ##

rings <- readxl::read_xlsx("Piston Rings.xlsx")

## X-bar ##

sig_hat <- qcc::sd.xbar(rings[,-1])

qcc::qcc(rings[,-1],type="xbar",std.dev=sig_hat,plot=TRUE)

## S ##

qcc::qcc(rings[,-1],type="S",plot=TRUE)

## Phase I X-bar and s Control Charts w/Variable Sample Size ##

rings1 <- readxl::read_xlsx("Piston Rings - Variable Sample Size.xlsx")

## X-bar ##

numerator <- apply(rings1[,-1],1,FUN=function(x){
  
  (5-sum(is.na(x))-1)*var(x,na.rm=T)
  
})

## Check that it worked ##

numerator[1]

4*var(as.vector(rings1[1,-1]))

## Calculate the Denominator ##

denom <- apply(rings1[,-1],1,FUN=function(x){
  
  5-sum(is.na(x)) - 1
  
})

## Check that it worked ##
## Sample 2 has 3 obs ##

denom[2] # 3 - 1 = 2 #

sig_hat1 <- (sum(numerator)/sum(denom))^0.50

qcc::qcc(rings1[,-1],type="xbar",std.dev=sig_hat1,plot=TRUE)

## S ##

qcc::qcc(rings1[,-1],type="S",center=sig_hat1,std.dev=sig_hat1,plot=TRUE)

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

mortgages <- readxl::read_xlsx("Mortgage Loan Costs.xlsx")

qcc::qcc(mortgages$Cost,type="xbar.one",plot=TRUE)

## Control Chart for Moving Range ##

library(ggQC)

ggplot(mortgages, aes(x=Week, y = Cost)) +
  stat_mR() + ylab("Moving Range")
