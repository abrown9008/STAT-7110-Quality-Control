## STAT 7110 Multiple Stream Process Chart Code ##

library(tidyverse)

## Car Wash Example

carwash <- readxl::read_xlsx("CarWash.xlsx")

## Obtain Sample Means & Ranges ##

wax_means <- tibble(Means = rowMeans(carwash[,-6]),
                    CarWashID = carwash$CarWashID,
                    Week = rep(1:25,5))

wax_ranges <- tibble(Ranges = 
                       apply(carwash[,-6],1,FUN=function(x){
                         
                         max(x) - min(x)
                         
                         }),
                     CarWashID = carwash$CarWashID,
                     Week = rep(1:25,5)
)

## Grand Mean ##

xdbar <- mean(wax_means$Means)

rbar <- mean(wax_ranges$Ranges)

## Control Limits ##

A2 <- 0.577

UCL <- xdbar + A2*rbar

LCL <- xdbar - A2*rbar

## Obtain Minimum and Maximum Mean Values for Each Car Wash ##

max_means <- wax_means %>%
  dplyr::group_by(Week) %>%
  dplyr::summarize(Max = max(Means),
                   Min = min(Means))

## Plot Phase I Chart ##

max_means %>%
  ggplot() + geom_line(aes(x=Week,y=UCL),color="black") +
  geom_line(aes(x=Week,y=LCL),color="black") +
  geom_line(aes(x=Week,y=Max),color="red") +
  geom_line(aes(x=Week,y=Min),color="red") +
  geom_point(aes(x=Week,y=Max),color="red") +
  geom_point(aes(x=Week,y=Min),color="red") +
  geom_line(aes(x=Week,y=xdbar),color="black",linetype="dashed") +
  labs(x = "Week", y = "Gallons of Liquid Wax") +
  theme_classic() +
  ggtitle("Boyd's GCC Phase I Control Chart") +
  theme(plot.title=element_text(hjust=0.50)) +
  scale_x_continuous(breaks = seq(1,25))

## Okay, great! It looks like our Phase I analysis shows evidence of 
## statistical control, so let's move on to Phase II analysis ##

carwash2 <- readxl::read_xlsx("CarWash.xlsx",sheet=2)

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

max_means2 <- wax_means2 %>%
  dplyr::group_by(Week) %>%
  dplyr::summarize(Max = max(Means),
                   Min = min(Means))

## Plot Phase II Chart ##

max_means2 %>%
  ggplot() + geom_line(aes(x=Week,y=UCL),color="black") +
  geom_line(aes(x=Week,y=LCL),color="black") +
  geom_line(aes(x=Week,y=Max),color="red") +
  geom_line(aes(x=Week,y=Min),color="red") +
  geom_point(aes(x=Week,y=Max),color="red") +
  geom_point(aes(x=Week,y=Min),color="red") +
  geom_line(aes(x=Week,y=xdbar),color="black",linetype="dashed") +
  labs(x = "Week", y = "Gallons of Liquid Wax") +
  theme_classic() +
  ggtitle("Boyd's GCC Phase II Control Chart") +
  theme(plot.title=element_text(hjust=0.50)) +
  scale_x_continuous(breaks = seq(1,25))

## Okay, it looks like we have some points plotting above the UCL...
## but which stream is causing it? ##

## Violations ##

violations <- wax_means2 %>%
  dplyr::filter(Means > UCL | Means < LCL)

violations

## Looks like CarWash 1 is our culprit ##

## A Control Chart for Each Stream Approach ##

## Split up Phase I Data by CarWashID ##

p1 <- split(carwash,carwash$CarWashID)

p1

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

## Calculate Control Limits & Plot Charts ##

d2 <- IQCC::d2(5)

L <- 3.46

cfes <- lapply(samp_list,FUN=function(x){
  
  xdbar <- mean(x$Means)
  
  rbar <- mean(x$Ranges)
  
  UCL <- xdbar + L*rbar/(d2*sqrt(5))
  
  LCL <- xdbar - L*rbar/(d2*sqrt(5))
  
  p1_chart <- x %>%
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
               Violations = x %>%
                 dplyr::filter(Means > UCL | Means < LCL) %>%
                 dplyr::select(Week))
  
  print(p1_chart)
  
  return(info)
  
})

## Phase II Analysis ##

## Split up Phase I Data by CarWashID ##

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
  
  p2_chart <- df %>%
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
  
  info <- list(Violations = df %>%
                 dplyr::filter(Means > limits[2] | Means < limits[1]) %>%
                 dplyr::select(Week))
  
  cfes2[[i]] <- info
  
  print(p2_chart)
  
}

## So how can we know if any of our points are OOC? ##

cfes2[[1]]
cfes2[[2]]
cfes2[[3]]
cfes2[[4]]
cfes2[[5]]

