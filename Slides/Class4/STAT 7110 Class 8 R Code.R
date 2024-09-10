## STAT 7110 Class 8 R Code ##

library(tidyverse)

## Read in Example 1 Data ##

ex1 <- readxl::read_xlsx("Class8.xlsx",sheet=1)

## Plot CUSUM Using QCC ##

cusum_plot <- qcc::cusum(ex1[,2],sizes=1,center=10,std.dev=1,decision.interval = 5,
                         head.start = 0.50,plot=T)

## Determine Which Points Went OOC ##

cusum_plot$violations$lower
cusum_plot$violations$upper

## Estimate New Mean ##

mu0 <- cusum_plot$center

ci_plus <- cusum_plot$pos[min(cusum_plot$violations$upper)]

k <- cusum_plot$head.start

first_ooc <- min(cusum_plot$violations$upper)

i <- first_ooc

ci_plus1 <- ci_plus

while(ci_plus1 > 0){
  
  ci_plus1 <- cusum_plot$pos[i - 1]
  
  i <- i - 1
  
}

i

N_plus <- first_ooc - i + 1

new_mean <- mu0 + k + ci_plus/N_plus
new_mean

## Piston Rings Example ##

library(qcc)
data("pistonrings")

p1 <- pistonrings %>%
  dplyr::filter(trial == "TRUE")

## Estimate Process Mean & SD ##

xbar <- mean(p1$diameter)

sig_hat <- sd(p1$diameter)

plotting_xbar <- p1 %>%
  dplyr::group_by(sample) %>%
  dplyr::summarize(Xbarz = mean(diameter))

p1_cusum <- qcc::cusum(plotting_xbar$Xbarz,sizes=5,
                       std.dev=sig_hat,center=xbar,
                       head.start=0.50,decision.interval = 5,
                       plot=T)

## Okay great! Move on to Phase II ##

plotting_xbarp2 <- pistonrings %>%
  dplyr::filter(trial == "FALSE") %>%
  dplyr::group_by(sample) %>%
  dplyr::summarize(Xbarz = mean(diameter))

p2_cusum <- qcc::cusum(plotting_xbarp2$Xbarz,sizes=5,
                       std.dev=sig_hat,center=xbar,
                       head.start=0.50,decision.interval=5,
                       plot=T)

## Which one went out of control first? ##

p2_cusum$violations$upper[1]

## New Mean ##

xbar + 0.5 + p2_cusum$pos[12]/7

## How does this compare to X-bar & R Chart? ##

Ranges <- p1 %>%
  dplyr::group_by(sample) %>%
  dplyr::summarize(Range = max(diameter) - min(diameter))

R_est <- mean(Ranges$Range)/2.326

sxbar <- qcc::qcc(plotting_xbar$Xbarz,sizes=5,type="xbar",std.dev=R_est,plot=T)

## Moving on to Phase II ##

sxbarp2 <- qcc::qcc(plotting_xbarp2$Xbarz,sizes=5,
                    type = "xbar",
                    center=sxbar$center,
                    limits = c(sxbar$limits[1],sxbar$limits[2]),
                    plot=T)

sxbarp2$violations$beyond.limits[1] + 25

## Estimating ARL Using a Function ##

cusum_arl <- function(side="UPPER",k,h,mu0,mu1,sigma0){
  
  if(side == "UPPER"){
    
    d_ast <- (mu1-mu0)/sigma0
    
    b = h  + 1.166
    
    big_delta <- d_ast - k
    
    ARL <- (exp(-2*big_delta*b) + 2*big_delta*b - 1)/(2*big_delta^2)
    
    return(ARL)
    
  } else{ 
    
    if(side == "LOWER"){
      
      d_ast <- (mu1-mu0)/sigma0
      
      b = h  + 1.166
      
      big_delta <- -d_ast - k
      
      ARL <- (exp(-2*big_delta*b) + 2*big_delta*b - 1)/(2*big_delta^2)
      
      return(ARL)
      
    } else{
        
      
      return('Error: Either specify side="UPPER" or side="LOWER"')
      
      }
    
    }
  
}

## Using Pistons Data ##

## Let's say we want to estimate the ARL for a +/- 1sd shift ##

upper_arl <- cusum_arl(side="UPPER",k=0.5,h=5,mu0=p1_cusum$center,sigma0=p1_cusum$std.dev,
                       mu1=p1_cusum$center + p1_cusum$std.dev)

lower_arl <- cusum_arl(side="LOWER",k=0.5,h=5,mu0=p1_cusum$center,sigma0=p1_cusum$std.dev,
                       mu1=p1_cusum$center - p1_cusum$std.dev)

## Overall ARL ##

(1/upper_arl + 1/lower_arl)^(-1)

## Which is: ##

lower_arl/2
