## STAT 7110 Multiple Stream Process Chart Code ##

library(tidyverse)
library(readxl)

## Car Wash Example ##

carwash <- read_xlsx("CarWash.xlsx")

View(carwash)

## Boyd's Group Control Chart ##

## Obtain Sample Means & Ranges ##

## Dataframe for Sample Means ##

wax_means <- tibble(Means = rowMeans(carwash[,-6]), # removing CarWashID from the calculation
                    CarWashID = carwash$CarWashID, # adding CarWashID back in
                    Week = rep(1:25,5)) # adding a week column

## Dataframe for Sample Ranges ##

wax_ranges <- tibble(Ranges = 
                       apply(carwash[,-6],1,FUN=function(x){
                         
                         max(x) - min(x)
                         
                         }), # calculating the range for each row
                     CarWashID = carwash$CarWashID, # adding CarWashID back in
                     Week = rep(1:25,5) # adding a week column
              )

## Calculate the Grand Mean ##

xdbar <- mean(wax_means$Means)

rbar <- mean(wax_ranges$Ranges)

## Control Limits ##

A2 <- 0.577

UCL <- xdbar + A2*rbar

LCL <- xdbar - A2*rbar

## Obtain Minimum and Maximum Mean Values for Each Car Wash ##

max_means <- wax_means |>
  group_by(Week) |>
  summarize(Max = max(Means),
            Min = min(Means))

## Plot Phase I Chart ##

max_means |>
  ggplot(aes(x=Week)) + 
  geom_line(aes(y=UCL),color="black") +
  geom_line(aes(y=LCL),color="black") +
  geom_line(aes(y=Max),color="red") +
  geom_line(aes(y=Min),color="red") +
  geom_point(aes(y=Max),color="red") +
  geom_point(aes(y=Min),color="red") +
  geom_line(aes(y=xdbar),color="black",linetype="dashed") +
  labs(x = "Week", 
       y = "Gallons of Liquid Wax",
       title = "Boyd's GCC Phase I Control Chart") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.50)) +
  scale_x_continuous(breaks = seq(1,25))

## Okay, great! It looks like our Phase I analysis shows evidence of 
## statistical control, so let's move on to Phase II analysis ##

carwash2 <- read_xlsx("CarWash.xlsx",sheet=2)

## Obtain Sample Means & Ranges ##

wax_means2 <- tibble(Means = rowMeans(carwash2[,-6]),
                     CarWashID = carwash2$CarWashID,
                     Week = rep(1:25,5))

wax_ranges2 <- tibble(Ranges = 
                        apply(carwash2[,-6],1,FUN=function(x){
                          
                          max(x) - min(x)
                          
                        }),
                      CarWashID = carwash2$CarWashID,
                      Week = rep(1:25,5)
)

## Obtain Minimum and Maximum Mean Values ##

max_means2 <- wax_means2 |>
  group_by(Week) |>
  summarize(Max = max(Means),
            Min = min(Means))

## Plot Phase II Chart ##

max_means2 |>
  ggplot(aes(x=Week)) + 
  geom_line(aes(y=UCL),color="black") +
  geom_line(aes(y=LCL),color="black") +
  geom_line(aes(y=Max),color="red") +
  geom_line(aes(y=Min),color="red") +
  geom_point(aes(y=Max),color="red") +
  geom_point(aes(y=Min),color="red") +
  geom_line(aes(y=xdbar),color="black",linetype="dashed") +
  labs(x = "Week", 
       y = "Gallons of Liquid Wax",
       title="Boyd's GCC Phase II Control Chart") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.50)) +
  scale_x_continuous(breaks = seq(1,25))

## Okay, it looks like we have some points plotting above the UCL...
## but which stream is causing it? ##

## Violations ##

violations <- wax_means2 |>
  filter(Means > UCL | Means < LCL)

print(violations)

## Looks like CarWash 1 is our culprit ##

## A Control Chart for Each Stream Approach ##

## Split up Phase I Data by CarWashID ##

p1 <- split(carwash,carwash$CarWashID)

p1

## Now, p1 is a list where each element of the list is a dataframe -- 
## one for each carwash ##

## Calculate Means & Ranges ##

samp_list <- lapply(p1,FUN=function(x){
  
  df <- tibble(Means = rowMeans(x[,-6]),
               Ranges = 
                 apply(x[,-6],1,FUN=function(x){
                   
                   max(x) - min(x)
                   
                 }),  
                CarWashID = x$CarWashID,
                Week = seq(1,25))
  
  return(df)
  
})

## The above code calculates the sample means and ranges for each car wash
## and stores them in their own dataframes with those dataframes being stored
## as elements in the list samp_list ##

## Calculate Control Limits & Plot Charts ##

library(IQCC)

d2 <- d2(5)

L <- 3.46

## Notice in the article for the Control Chart for Each Stream Technique,
## in Table 3, if we assume that the correlation between each stream is 0,
## which is what we're doing in this example, then the L value we use for
## the control limits when the number of monitored streams is 5 is 3.46 ##

cfes <- lapply(samp_list,FUN=function(x){
  
  xdbar <- mean(x$Means)
  
  rbar <- mean(x$Ranges)
  
  UCL <- xdbar + L*rbar/(d2*sqrt(5))
  
  LCL <- xdbar - L*rbar/(d2*sqrt(5))
  
  p1_chart <- x |>
    ggplot(aes(x=Week,y=Means)) +
    geom_point(color='red') + geom_line(color='red') +
    geom_line(aes(y=UCL),color='black') +
    geom_line(aes(y=LCL),color='black') +
    geom_line(aes(y=xdbar),color='black',linetype='dashed') +
    labs(x = "Week", y = "Gallons of Liquid Wax",
         title = paste("Chart for Car Wash ",x$CarWashID,sep="")) +
    theme_classic()  +
    theme(plot.title=element_text(hjust=0.50)) +
    scale_x_continuous(breaks = seq(1,25))
  
  info <- list(Center = xdbar,
               Limits = c(LCL,UCL),
               Violations = x |>
                 filter(Means > UCL | Means < LCL) |>
                 select(Week))
  
  print(p1_chart)
  
  return(info)
  
})

View(info)

## Phase II Analysis ##

## Split up Phase II Data by CarWashID ##

p2 <- split(carwash2,carwash2$CarWashID)

## Calculate Means ##

samp_list2 <- lapply(p2,FUN=function(x){
  
  df <- tibble(Means = rowMeans(x[,-6]),
               Ranges = 
                 apply(x[,-6],1,FUN=function(x){
                   
                   max(x) - min(x)
                   
                 }),  
               CarWashID = x$CarWashID,
               Week = seq(1,25))
  
  return(df)
  
})

## Plot Phase II Charts ##

cfes2 <- vector('list',length=5)

for(i in 1:5){
  
  limits <- cfes[[i]]$Limits
  
  center_line <- cfes[[i]]$Center
  
  df <- samp_list2[[i]]
  
  p2_chart <- df |>
    ggplot(aes(x=Week,y=Means)) +
    geom_point(color='red') + geom_line(color='red') +
    geom_line(aes(y=limits[2]),color='black') +
    geom_line(aes(y=limits[1]),color='black') +
    geom_line(aes(y=center_line),color='black',linetype='dashed') +
    labs(x = "Week", y = "Gallons of Liquid Wax",
         title = paste("Chart for Car Wash ",unique(df$CarWashID),sep="")) +
    theme_classic()  +
    theme(plot.title=element_text(hjust=0.50)) +
    scale_x_continuous(breaks = seq(1,25))
  
  info <- list(Violations = df |>
                 filter(Means > limits[2] | Means < limits[1]) |>
                 select(Week))
  
  cfes2[[i]] <- info
  
  print(p2_chart)
  
}

## So how can we know if any of our points are OOC? ##

cfes2[[1]]
cfes2[[2]]
cfes2[[3]]
cfes2[[4]]
cfes2[[5]]

## So just like with Boyd's GCC, only CarWash 1 is out of control ##
