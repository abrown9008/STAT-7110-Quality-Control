## STAT 7110 - Class 7 R Code ##

library(tidyverse)

## Read in Bottles Data ##

bottles <- readxl::read_xlsx("Class7.xlsx",sheet = 1)

## Plot Histogram ##

bottles %>%
  ggplot(aes(PSI)) + geom_histogram(bins=nclass.Sturges(bottles$PSI),
                                    color='black',
                                    fill='white') +
  labs(title="Histogram of PSI for Beer Bottles") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.50))

## Calculate Upper and Lower Natural Tolerance Limits ##

xbar <- mean(bottles$PSI)

stdev <- sd(bottles$PSI)

UNTL <- round(xbar + 3*stdev,2)

LNTL <- round(xbar - 3*stdev,2)

## Add Tolerance Limits to Histogram ##

bottles %>%
  ggplot(aes(PSI)) + geom_histogram(bins=nclass.Sturges(bottles$PSI),
                                    color='black',
                                    fill='white') +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = LNTL,color='red') +
  geom_vline(xintercept = UNTL,color='red') +
  labs(title="Histogram of PSI for Beer Bottles") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.50)) +
  scale_x_continuous(limits = c(38,125),
                     breaks=c(LNTL,xbar,UNTL))

## Suppose our Spec limits are 60 and 120...Let's add those to the hist ##

LSL <- 60
USL <- 120

bottles %>%
  ggplot(aes(PSI)) + geom_histogram(bins=nclass.Sturges(bottles$PSI),
                                    color='black',
                                    fill='white') +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = LNTL,color='red') +
  geom_vline(xintercept = UNTL,color='red') +
  geom_vline(xintercept = LSL,color='blue') +
  geom_vline(xintercept = USL,color='blue') +
  labs(title="Histogram of PSI for Beer Bottles") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.50)) +
  scale_x_continuous(limits = c(38,125),
                     breaks=c(LNTL,LSL,xbar,USL,UNTL))

## Calculate Cp ##

cp <- (USL - LSL)/(6*stdev)

cp

## Calculate p-hat ##

1/cp*100

## One Sided Spec Limit ##

cpl <- (xbar - 60)/(3*stdev)

cpl

p <- 1/cpl*100

p

FNC <- pnorm(60,mean=xbar,sd=stdev)

FNC

cpu <- (120 - xbar)/(3*stdev)

cpk <- min(cpl,cpu)

cpk

## New Glass Container PSI ##

jars <- readxl::read_xlsx("Class7.xlsx",sheet=2)

## Calculate Cp assuming LSL = 200 & USL = 350 ##

hist(jars$PSI)

xbar2 <- mean(jars$PSI)

stdev2 <- sd(jars$PSI)

Cp <- (350-200)/(6*stdev2)

Cp

## Calculate Cpk ##

Cpu <- (350-xbar2)/(3*stdev2)

Cpl <- (xbar2-200)/(3*stdev2)

Cpk <- min(Cpu,Cpl)

## Plot Histogram w/Spec Limits ##

USL2 <- 350
LSL2 <- 200

jars %>%
  ggplot(aes(PSI)) + geom_histogram(bins=nclass.Sturges(jars$PSI),
                                    color='black',
                                    fill='white') +
  geom_hline(yintercept=0) +
  geom_vline(xintercept = LSL2,color='blue') +
  geom_vline(xintercept = USL2,color='blue') +
  labs(title="Histogram of PSI for Glass Jars") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.50)) +
  scale_x_continuous(limits = c(170,360),
                     breaks=c(LSL2,xbar2,USL2))

## Estimate Fraction Nonconforming ##

fnc <- pnorm(200,mean=xbar2,sd=stdev2) + 
  pnorm(350,mean=xbar2,sd=stdev2,lower.tail=F)

fnc

## Calculate CI for Jars Example ##

cp_jars <- (350-200)/(6*stdev2)

chi_up <- qchisq(0.025,nrow(jars)-1,lower.tail = F)
chi_low <- qchisq(0.025,nrow(jars)-1) 

ci_low <- cp_jars*sqrt(chi_low/(nrow(jars)-1))
ci_up <- cp_jars*sqrt(chi_up/(nrow(jars)-1))

c(ci_low,ci_up)

## Calculate CI for Cp ##

USL <- 62
LSL <- 38
mu <- (USL + LSL)/2
s <- 1.75

Cp <- (USL-LSL)/(6*s)

chi_up <- qchisq(0.05/2,df=20-1,lower.tail=F)
chi_down <- qchisq(0.05/2,df=20-1,lower.tail=T)

Up <- Cp*sqrt(chi_up/(20-1))
Down <- Cp*sqrt(chi_down/(20-1))

c(Down,Up)

## Cp Ratio for Piston Rings ##

pistons <- readxl::read_xlsx("Piston Rings.xlsx")

xbarz <- apply(pistons[,-1],1,mean)
s2 <- apply(pistons[,-1],1,sd)

sig_hat <- mean(s2)/0.94

xbar_chart <- qcc::qcc(pistons[,-1],type="xbar",sizes=5,
                       std.dev=sig_hat,plot=T)

s_chart <- qcc::qcc(pistons[,-1],type="S",sizes=5,plot=T)

## Both charts are in control so let's move to Cp! ##

## Process Capability ##

## What are the spec limits? 74.0 +/- 0.05 ##

qcc::process.capability(xbar_chart,spec.limits=c(74-0.05,74+0.05),
                        target=xbar_chart$center)
