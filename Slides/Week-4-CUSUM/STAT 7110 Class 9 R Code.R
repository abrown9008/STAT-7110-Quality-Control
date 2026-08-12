## STAT 7110 Class 9 R Code ##

library(tidyverse)

## Customer Service Hold Times ##

## Read in Data ##

hold <- readxl::read_xlsx("Class9.xlsx",sheet=1)

## Create EWMA Control Chart ##

ewma_chart <- qcc::ewma(hold$Time,
                        sizes=1,
                        center=10,
                        std.dev=1,
                        lambda=0.10,
                        nsigmas=2.7,
                        plot=T)

## Determining which points are OOC ##

ewma_chart$violations

## Phase I Dunder-Mifflin Fuel Costs ##

fuel <- readxl::read_xlsx("Class9.xlsx",sheet=2)

xbar <- mean(fuel$Cost)
sigma_hat <- sd(fuel$Cost)

## Plot Chart ##

fuel_chart <- qcc::ewma(fuel$Cost,
                        sizes=1,
                        center=xbar,
                        std.dev=sigma_hat,
                        lambda=0.20,
                        nsigmas=2.962,
                        plot=T)


## Phase II Data ##

fuel2 <- readxl::read_xlsx("Class9.xlsx",sheet=3)

fuel2_chart <- qcc::ewma(fuel2$Cost,
                         sizes=1,
                         lambda=0.20,
                         nsigmas=2.962,
                         center=xbar,
                         std.dev=sigma_hat,
                         plot=T)

fuel2_chart$violations

## ACF ##

acf(fuel$Cost,type="correlation")
