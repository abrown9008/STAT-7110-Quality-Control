## STAT 7110 Class 5 R Code ##

library(tidyverse)

## Phase I np Control Chart - Enrollment Data ##

enrollment <- readxl::read_xlsx("Enrollment.xlsx")

## Phase I Control Chart ##

enroll_chart <- qcc::qcc(enrollment$`Number Not Re-Enrolling`,
                         type="np",sizes=50,
                         plot=TRUE)

## Phase I Control Chart - Orange Juice Cans ##

ojc <- readxl::read_xlsx("Orange Juice Cans.xlsx")

ojc_chart <- qcc::qcc(ojc$`Number of Nonconforming Cans`,
                      type="np",sizes=ojc$`Sample Size`,
                      plot=TRUE)

## Whoops! Two OOC Points ##

ooc_points <- ojc_chart$violations$beyond.limits

ojc1 <- ojc[-ooc_points,]

ojc1_chart <- qcc::qcc(ojc1$`Number of Nonconforming Cans`,
                       type="np",sizes=ojc1$`Sample Size`,
                       plot=TRUE)

## Now we just have one ##

ooc_points1 <- ojc1_chart$violations$beyond.limits

ojc2 <- ojc1[-ooc_points1,]

ojc2_chart <- qcc::qcc(ojc2$`Number of Nonconforming Cans`,
                       type="np",sizes=ojc2$`Sample Size`,
                       plot=TRUE)

## Okay great! The process now plots in control so we can move on to Phase II ##

ojcp2 <- readxl::read_xlsx("Orange Juice Cans.xlsx",
                           sheet=2)

## Take the Phase I Limits ##

ojc2_chart$limits

uclnp <- ojc2_chart$limits[2]
lclnp <- ojc2_chart$limits[1]
npbar <- ojc2_chart$center

ojc_npchartp2 <- qcc::qcc(ojcp2$`Number of Nonconforming Cans`,
                        type="np",sizes=50,
                        limits = c(lclnp,uclnp),
                        center = npbar,
                        plot=TRUE)

## Again, notice the same phenomenon that we observed last week ##

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

## Okay, now all of our points plot in between the control limits ##

## Let's read in Phase II data ##

p2_circuits <- readxl::read_xlsx("Class5.xlsx",
                                 sheet=2)

## Save the Phase I Control Limits ##

uclc <- circuit_chart1$limits[2]
lclc <- circuit_chart1$limits[1]
cl <- circuit_chart1$center

p2_circuit_chart <- qcc::qcc(p2_circuits$`Number of Nonconformities`,
                             type="c",
                             limits = c(lclc,uclc),
                             center = cl,
                             plot=TRUE)

## Read in Midterm Data ##

midterm <- readxl::read_xlsx("Class5.xlsx",
                             sheet=3)

## Plot C Chart for Phase 1 ##

midterm_chart <- qcc::qcc(midterm$`Number of Incorrect Responses`,
                          type="c")

## Let's read in Phase II Data to Monitor Next 15 semesters worth of data ##

midterm2 <- readxl::read_xlsx("Class5.xlsx",
                              sheet=4)

## Save Control Limits and CL from Phase I ##

clc <- midterm_chart$center
uclc <- midterm_chart$limits[2]
lclc <- midterm_chart$limits[1]

p2_chart <- qcc::qcc(midterm2$`Number of Incorrect Responses`,
                     type="c",
                     limits=c(lclc,uclc),
                     center=clc,
                     plot=TRUE)

## We definitely have one OOC point, but it's somewhat close to the UCL ##
## We also can see that the number of incorrect responses appears to have ##
## Increases to more around 50 (75%) than around 40 (80%) ##

## How can we know for sure? Let's test Phase I vs Phase II Data! ##

t.test(midterm$`Number of Incorrect Responses`,
       midterm2$`Number of Incorrect Responses`,
       alternative = "two.sided")

## Mann-Whitney U Test/Wilcoxon Rank Sum ##

wilcox.test(midterm$`Number of Incorrect Responses`,
            midterm2$`Number of Incorrect Responses`,
            alternative = "two.sided")

## Calculating Control Limits for Midterm Data Using Alpha Method ##

## Assume that C-bar is true mean ##

alpha <- 0.0027

UCL <- qpois(alpha/2,lambda=midterm_chart$center,lower.tail=FALSE)
LCL <- qpois(alpha/2,lambda=midterm_chart$center,lower.tail=TRUE)

## Compare to 3 Sigma Limits ##

c(UCL,midterm_chart$limits[2])

c(LCL,midterm_chart$limits[1])

## Read In Supply Chain Data ##

sc <- readxl::read_xlsx("Class5.xlsx",
                             sheet=5)

## Plot U Chart ##

sc_chart <- qcc::qcc(sc$`Total Number of Errors`,
                     type="u",sizes=sc$`Sample Size`)

## Variable Sample Size U Chart ##

## Read in Textile Data ##

textile <- readxl::read_xlsx("Class5.xlsx",
                        sheet=6)

textile$`Number of Inspection Units Per Roll` <- textile$`Number of Square Meters`/50

## Plot U Chart ##

vu_chart <- qcc::qcc(textile$`Total Number of Nonconformities`,
                     type="u",
                     sizes=textile$`Number of Inspection Units Per Roll`,
                     plot=T)
