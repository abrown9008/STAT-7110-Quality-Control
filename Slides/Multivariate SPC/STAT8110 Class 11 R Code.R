## STAT 8110 Class 11 R Code ##

## Hotelling's T2 Examples ##

## Suppose we're a department chair and we want to monitor the performance
## of students in 3 separate undergraduate statistics courses. 
## For Phase I monitoring, take random samples of 5 student assessment
## scores for each of the 16 weeks. Data are contained
## in the student_scores.xlsx file

setwd("D://Teaching//STAT 8110//Data//")

class1 <- readxl::read_xlsx("student_scores.xlsx",sheet=1)
class2 <- readxl::read_xlsx("student_scores.xlsx",sheet=2)
class3 <- readxl::read_xlsx("student_scores.xlsx",sheet=3)

class_list <- list(class1 = class1,
                   class2 = class2,
                   class3 = class3)

## Use mqcc function to build chart ##

class_chart <- qcc::mqcc(class_list,type="T2",plot=T)

## Okay, let's say these phase I data were obtained in the fall semester.
## Since we have established phase I statistical control, let's take a new sample
## for the spring semester. Again, the sample size for each class is n = 5
## and we take one sample per 16 weeks of the semester. The data are contained
## in spring_scores.xlsx

class11 <- readxl::read_xlsx("spring_scores.xlsx",sheet=1)
class21 <- readxl::read_xlsx("spring_scores.xlsx",sheet=2)
class31 <- readxl::read_xlsx("spring_scores.xlsx",sheet=3)

class_list1 <- list(class11 = class11,
                   class21 = class21,
                   class31 = class31)

spring_chart <- qcc::mqcc(class_list1,type="T2",
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

xchart1 <- qcc::qcc(class_list1[[1]],type="xbar",
                    center=mu_hat1,
                    std.dev=sig_hat1,
                    plot=T)

xchart2 <- qcc::qcc(class_list1[[2]],type="xbar",
                    center=mu_hat2,
                    std.dev=sig_hat2,
                    plot=T)

xchart3 <- qcc::qcc(class_list1[[3]],type="xbar",
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