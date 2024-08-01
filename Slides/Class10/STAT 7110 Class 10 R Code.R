## STAT 7110 Class 10 R Code ##

library(tidyverse)

## Estimating the multivariate sample mean and variance-covariance matrix ##

## Generate Random Samples from MVN ##

set.seed(12345)

n <- 25

mus <- c(5,10,15)

Sigma <- matrix(c(1,0.5,0.5,
                  0.5,1,0.5,
                  0.5,0.5,1),byrow=T,ncol=3)

## Make sure it worked ##

Sigma

## Random Samples ##

sampz <- t(MASS::mvrnorm(n=n,mu=mus,Sigma=Sigma))

## Check out structure ##

sampz

## Estimate Mean Vector ##

mu_hat <- rowMeans(sampz)

## Check it out ##

mu_hat

## Estimate Variance-Covariance Matrix ##

var(t(sampz))

## Check Variance Estimates ##

apply(sampz,1,var)

## Joint Monitoring of Textile Data ##

## Read in Means ##

text_mean <- readxl::read_xlsx("Week10 Data.xlsx",sheet=1)

## Read in Variances & Covariances ##

text_vc <- readxl::read_xlsx("Week10 Data.xlsx",sheet=2)

## Obtain Sample Mean Vector ##

mean_vector <- matrix(colMeans(text_mean),ncol=1)

mean_vector

## Obtain Sample V-C Matrix ##

vc_matrix <- matrix(nrow=2,ncol=2)

vc_means <- colMeans(text_vc)

vc_means

vc_matrix[1,1] <- vc_means[1]

vc_matrix[2,2] <- vc_means[2]

vc_matrix[1,2] <- vc_means[3]

vc_matrix[2,1] <- vc_means[3]

## Check it out ##

vc_matrix

## Obtain T^2 Values ##

estat <- vector('list',length=3)

estat[[1]] <- mean_vector

estat[[2]] <- vc_matrix

estat[[3]] <- as.matrix(text_mean)

estat

T2 <- IQCC::T2.1(estat,m=20,n=10)

## Plot Chart ##

t2_chart <- IQCC::cchart.T2.1(T2,m=20,n=10,p=2)

## Notice alpha is fixed at 0.0027 in this function ##

## Phase I Example without pre-aggregated data (realistic situation) ##

## Monitoring 3 Can Filling Lines ##

## Read in Canning Data as List ##

## Get Sheet Names ##

sn <- readxl::excel_sheets("Can Volume Data.xlsx")

cans <- lapply(sn,FUN=function(x){
  
  d <- readxl::read_xlsx("Can Volume Data.xlsx",sheet=x) %>%
    dplyr::select(-`Sample Number`,-`Line Number`)
  
  return(d)
  
})

names(cans) <- paste("Line",1:3,sep="")

## Use mqcc to Plot the Phase 1 Chart! ##

t2 <- qcc::mqcc(cans,type='T2')

## Notice, t2 contains the estimated mean vector and v-cov matrix ##

t2$center

t2$cov

## More Hotelling's T2 Examples ##

## Suppose we're a department chair and we want to monitor the performance
## of students in 3 separate undergraduate statistics courses. 
## For Phase I monitoring, take random samples of 5 student assessment
## scores for each of the 16 weeks. Data are contained
## in the student_scores.xlsx file

sn1 <- readxl::excel_sheets("student_scores.xlsx")

class_list <- lapply(sn1,FUN=function(x){
  
  d <- readxl::read_xlsx("student_scores.xlsx",col_names=F,sheet=x)
  
  return(d)
  
})

names(class_list) <- sn1

## Use mqcc function to build chart ##

class_chart <- qcc::mqcc(class_list,type="T2",plot=T)

## Okay, let's say these phase I data were obtained in the fall semester.
## Since we have established phase I statistical control, let's take a new sample
## for the spring semester. Again, the sample size for each class is n = 5
## and we take one sample per 16 weeks of the semester. The data are contained
## in spring_scores.xlsx

class_list2 <- lapply(sn1,FUN=function(x){
  
  d <- readxl::read_xlsx("spring_scores.xlsx",col_names=F,sheet=x)
  
  return(d)
  
})

names(class_list2) <- sn1

spring_chart <- qcc::mqcc(class_list2,type="T2",
                          center=class_chart$center,
                          cov=class_chart$cov,
                          limits=F,pred.limits=T,
                          plot=T)

## Note, switching limits=F and pred.limits=T gives us Phase II limits

## Okay, so we have several points plotting above the UCL 
## (called upper prediction limit here)

## How can we know which classes aren't performing as they should be? ##

## One solution could be creating individual Shewhart x-bar charts

c4 <- IQCC::c4(5)

sig_hat1 <- sqrt(class_chart$cov[1,1])/c4

sig_hat2 <- sqrt(class_chart$cov[2,2])/c4

sig_hat3 <- sqrt(class_chart$cov[3,3])/c4

mu_hat1 <- class_chart$center[1]

mu_hat2 <- class_chart$center[2]

mu_hat3 <- class_chart$center[3]

xchart1 <- qcc::qcc(class_list2$Class1,type="xbar",
                    center=mu_hat1,
                    std.dev=sig_hat1,
                    plot=T)

xchart2 <- qcc::qcc(class_list2$Class2,type="xbar",
                    center=mu_hat2,
                    std.dev=sig_hat2,
                    plot=T)

xchart3 <- qcc::qcc(class_list2$Class3,type="xbar",
                    center=mu_hat3,
                    std.dev=sig_hat3,
                    plot=T)

## With this strategy, we have to be careful about our alpha level. 
## However, here it is pretty clear that the first class is the problem.
## Contextually, what might be some causes?
## Check out some alternatives to this in the text (p. 520)

## Let's look at another example when n = 1. Suppose now that you are a
## High School math teacher at a tiny high school (like the one I went to LOL)
## where we only have four students enrolled in our course for the academic year.
## We want to monitor student progress throughout the school year. 
## Let's say that, like in the previous example, we have weekly quizzes (0-100)
## and that we'll use the fall semester as Phase I and the spring as Phase II. 
## The Phase I data are contained in math1.xlsx and the Phase II data are contained
## in math2.xlsx

math1 <- readxl::read_xlsx("math1.xlsx",col_names=F)

math1_list <- list(Student1 = math1$...1,
                   Student2 = math1$...2,
                   Student3 = math1$...3,
                   Student4 = math1$...4)

math1_chart <- qcc::mqcc(math1_list,type="T2.single",plot=T)

math1_chart$violations$beyond.limits

## Okay, so observation 5 is OOC. This time, let's take a look at summary
## statistics to help us figure out if this is problematic or not.

## Means ##

apply(math1,2,mean)

math1

## Okay, based on this, it looks like during week 5, student 4 had a low
## test score (70.5) compared to where they had been previously performing.
## Assume that the student had been sick that week and that was more of the 
## reason why they didn't perform well compared to their average performance.
## We'll go ahead and retain the point and move on to Phase II.

math2 <- readxl::read_xlsx("math2.xlsx",col_names=F)

math2_list <- list(Student1 = math2$...1,
                   Student2 = math2$...2,
                   Student3 = math2$...3,
                   Student4 = math2$...4)

math2_chart <- qcc::mqcc(math2_list,type="T2.single",
                         center=math1_chart$center,
                         cov=math1_chart$cov,
                         limits=F,pred.limits=T,
                         plot=T)

math2_chart$violations$beyond.pred.limits

## Okay, which student(s) is performing at a different level than in the fall? 

## Control Charts for Individuals 

c4 <- IQCC::c4(2)

mu_s1 <- math1_chart$center[1]
mu_s2 <- math1_chart$center[2]
mu_s3 <- math1_chart$center[3]
mu_s4 <- math1_chart$center[4]

sd1 <- math1_chart$cov[1,1]**0.50/c4
sd2 <- math1_chart$cov[2,2]**0.50/c4
sd3 <- math1_chart$cov[3,3]**0.50/c4
sd4 <- math1_chart$cov[4,4]**0.50/c4

ci_1 <- qcc::qcc(math2$...1,type="xbar.one",
                 center=mu_s1,std.dev=sd1,
                 plot=T)

ci_2 <- qcc::qcc(math2$...2,type="xbar.one",
                 center=mu_s2,std.dev=sd2,
                 plot=T)

ci_3 <- qcc::qcc(math2$...3,type="xbar.one",
                 center=mu_s3,std.dev=sd3,
                 plot=T)

ci_4 <- qcc::qcc(math2$...4,type="xbar.one",
                 center=mu_s4,std.dev=sd4,
                 plot=T)

## Okay, so here it is pretty clear that student 2 is performing at a much
## better rate than they were in the fall. This is good! But why? Are the topics
## more clear to them? Did they get extra help via tutoring? 
