## STAT 7110 Class 4 Code ##

## P-Chart ##

enrollment <- readxl::read_xlsx("Enrollment.xlsx")

## Phase I Control Chart ##

enroll_chart <- qcc::qcc(enrollment$`Number Not Re-Enrolling`,
                         type="p",sizes=enrollment$`Sample Size`,
                         plot=TRUE)

## Phase I Control Chart - Orange Juice Cans ##

ojc <- readxl::read_xlsx("Orange Juice Cans.xlsx")

ojc_chart <- qcc::qcc(ojc$`Number of Nonconforming Cans`,
                      type="p",sizes=ojc$`Sample Size`,
                      plot=TRUE)

## We have two OOC points -- Let's remove them and recalculate the limits ##

ooc_points <- ojc_chart$violations$beyond.limits

ojc1 <- ojc[-ooc_points,]

ojc_chart1 <- qcc::qcc(ojc1$`Number of Nonconforming Cans`,
                       type="p",sizes=ojc1$`Sample Size`,
                       plot=TRUE)

## Now we have another OOC point -- Let's remove it and recalculate ##

ooc_points1 <- ojc_chart1$violations$beyond.limits

ojc2 <- ojc1[-ooc_points1,]

ojc_chart2 <- qcc::qcc(ojc2$`Number of Nonconforming Cans`,
                       type="p",sizes=ojc2$`Sample Size`,
                       plot=TRUE)

## Okay great! The process now plots in control so we can move on to Phase II ##

ojcp2 <- readxl::read_xlsx("Orange Juice Cans.xlsx",
                           sheet=2)

## Take the Phase I Limits ##

ojc_chart2$limits

uclp <- ojc_chart2$limits[1,2]
lclp <- ojc_chart2$limits[1,1]
pbar <- ojc_chart2$center

ojc_chartp2 <- qcc::qcc(ojcp2$`Number of Nonconforming Cans`,
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

prop.test(c(x1,x2),c(n1,n2),alternative=c("two.sided"),
          correct=FALSE)

## Since our p-value is quite small, we have significant evidence to suggest ##
## that the proportion in Phase II is significantly different than our phase I ##
## Proportion ##

## We need to recalculate the limits for the Phase II Samples ##

p2_chart <- qcc::qcc(ojcp2$`Number of Nonconforming Cans`,
                     type="p",
                     sizes=ojcp2$`Sample Size`,
                     plot=T)

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

pos <- readxl::read_xlsx("POs.xlsx")

po_chart <- qcc::qcc(pos$`Incorrect POs`,type="p",
                     sizes=pos$`Sample Size`,
                     plot=T)

## Determining Out of Control Points ##

ooc_points2 <- po_chart$violations$beyond.limits

## Remove the Points and Re-plot Chart ##

pos1 <- pos[-ooc_points2,]

po_chart1 <- qcc::qcc(pos1$`Incorrect POs`,type="p",
                      sizes=pos1$`Sample Size`,
                      plot=T)

## Estimate Beta from Second Orange Juice Can Example ##

UCL <- p2_chart$limits[1,2]
LCL <- p2_chart$limits[1,1]

n <- ojcp2$`Sample Size`[1]

UCL1 <- round(n*UCL,0)
LCL1 <- round(n*LCL,0)

p <- 0.05

beta <- pbinom(UCL1,n,p) - pbinom(LCL1,n,p)

## Plotting OC Curve ##

qcc::oc.curves.p(p2_chart)

## ARL0 ##

alpha <- 1 - (pbinom(UCL1,n,p2_chart$center) - pbinom(LCL1,n,p2_chart$center))

arl0 <- 1/alpha

## Notice the arl0 < 370 which is what we'd expect for 3 sigma limits ##

p <- 0.27

beta <- pbinom(UCL1,n,p) - pbinom(LCL1,n,p)

arl1 <- 1/(1-beta)

arl1

## Approximately 2 Samples ##
