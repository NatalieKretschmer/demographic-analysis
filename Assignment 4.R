setwd("~/Desktop/FA21/Advanced Demographic Analysis/Assignment 4")
require(readr)

source("~/Desktop/FA21/Advanced Demographic Analysis/Assignment 4/APC IE programs/apcmat.fun.r")
source("~/Desktop/FA21/Advanced Demographic Analysis/Assignment 4/APC IE programs/apcrdg.fun.r")
source("~/Desktop/FA21/Advanced Demographic Analysis/Assignment 4/APC IE programs/apcrdgse.fun.r")
source("~/Desktop/FA21/Advanced Demographic Analysis/Assignment 4/APC IE programs/constr.fun.r")

### Use the
### National Health and Nutrition Examination Survey (NHANES) data posted on the Blackboard for this assignment. 
### In the class, we estimated the Hierarchical APC-Cross- Classified Random Effect Models
### (HACP-CCREM; Yang et al, 2008) to examine the age, period and cohort effects on body mass index (BMI).
### The data on the Blackboard include several other potential outcome variables:
###   -bmxwt: body weight in kilograms.
###   -bmxht: standing height in centimeters.
###   -bmxwaist: waist circumference in centimeters.
### Please choose one of them as your dependent variable and estimate a hierarchical APC model
### to examine the age, period and cohort effects:
###   -Interpret the coefficient estimates of the age effects.
###   -Visualize the period and cohort effects and provide a brief interpretation.

healthdata <- read_csv("APC_NHANES(1).csv")
names(healthdata)

healthdata$gender <- factor(healthdata$gender, exclude = c("", NA))
healthdata$period <- factor(healthdata$period, exclude = c("", NA))
healthdata$cohort <- factor(healthdata$cohort, exclude = c("", NA))

library(lme4)

APC <- lmer(bmxwaist ~ agec + agec2 + gender +(1 | period) + (1 | cohort), data = healthdata)
summary(APC)
confint(APC, level = .95)

#births <- matrix(healthdata$births, ncol = np,byrow = F)
#exposure <- matrix(healthdata$exposure, ncol = np,byrow = F)
#
#mx <- births/exposure
#
#swe_F_fit <- apcrdgse(r = mx, nrisk = exposure, fam = "qlik", 
#                     offst = T, lam = -1, Plot = F, CIplot = F, stderr = T)

age <- seq(17.5, 47.5, by = 5)
period <- seq(1947.5, 2002.5, by = 5)
cohort <- seq(1900, 1985, by = 5)

names(healthdata)
attach(healthdata)

u0 <- ranef(APC, condVar = TRUE)
u0

names(u0$cohort) <- "est"
names(u0$period) <- "est"
u0

cohort_eff <- data.frame(est = u0$cohort,
                         se = sqrt(attr(u0$cohort, "postVar")[1, ,]),
                         cohort = c("1931-40","1941-50","1951-60","1961-70", "1971-80","1981-90","1990-00",
                                    "2001-10","2011-20"))
cohort_eff

period_eff <- data.frame(est = u0$period,
                         se = sqrt(attr(u0$period, "postVar")[1, ,]),
                         period = c("2011-12","2013-14","2015-16","2017-18"))
period_eff

cohort_eff$upper <- cohort_eff$est + qnorm(0.975)*cohort_eff$se
cohort_eff$lower <- cohort_eff$est - qnorm(0.975)*cohort_eff$se
period_eff$upper <- period_eff$est + qnorm(0.975)*period_eff$se
period_eff$lower <- period_eff$est - qnorm(0.975)*period_eff$se


library(ggplot2)
ggplot(period_eff, aes(x = period, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  xlab("Period") + ylab("Period effect")

ggplot(cohort_eff, aes(x = cohort, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  xlab("Cohort") + ylab("Cohort effect")

