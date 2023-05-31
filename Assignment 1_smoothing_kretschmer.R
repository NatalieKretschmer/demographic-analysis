data_loc <- "~/Desktop/FA21/Advanced Demographic Analysis/Assignment 1"
setwd(data_loc)
getwd()

require(data.table)
#
#### The CSV file named “Estonia.TFR.csv” includes data on the time series of total fertility rate (TFR)
# in Estonia from 1959 to 2010. Please carry out spline smoothing to examine the trend of TFR.
# You may try a few different values of the argument spar and select the smoothness you like best.
# Briefly (in a few sentences) describe the trend in TFR in Estonia based on the smoothing results.

#     TFR in Estonia was, overall, increasing between 1959 and 1989, with some
#     variation in trend. Beginning in 1989, there was a sharp decrease in TFR
#     which continued until 1999. Beginning in 1999, TFR icreases again, at a
#     greater rate than prior to 1989 and continues until the end of given data.

estonia_fertility <- fread("Estonia.TFR.csv",
                           stringsAsFactors = F, 
                           data.table = F)
head(estonia_fertility)
names(estonia_fertility)

Year <- estonia_fertility$Year
TFR <-  estonia_fertility$TFR

est_fer_smooth <- smooth.spline(Year, TFR, spar = 0.6)
est_fer_smooth2 <- smooth.spline(Year, TFR, spar = 0.1)
str(est_fer_smooth)
names(est_fer_smooth)

plot(Year, TFR)
lines(Year, est_fer_smooth$y, lwd=2, col="red")
lines(Year, est_fer_smooth2$y, lwd=2, col="blue")



##   The CSV file named “NHANES.2005.sample.csv” includes a subset of the NHANES
# 2005 data. Please perform a local-polynomial regression (i.e., constructing a 
# LOESS curve) of diastolic blood pressure (BPXDI1) on age. You may try a few 
# different values of the argument span and select the smoothness you like best.
# Briefly (in one sentence or a few sentences) describe the age pattern of
# diastolic blood pressure based on the smoothing results.


#     Diastolic blood pressure rises slightly from early adulthood to middle 
#     adulthood, and falls again from middle adulthood to late adulthood.

nhanes_data <- fread("NHANES.2005.sample.csv",
                     stringsAsFactors = F, 
                     data.table = F)

head(nhanes_data)
tail(nhanes_data)
names(nhanes_data)

Age <- nhanes_data$AGE
BPXDI1 <- nhanes_data$BPXDI1

plot(Age, BPXDI1)
age_x_bp_smooth <- loess( BPXDI1 ~ Age, span = 1.5)
age_x_bp_smooth2 <- loess( BPXDI1 ~ Age, span = 0.25)
lines(Age, age_x_bp_smooth$fitted, lwd=2, col="red")
lines(Age, age_x_bp_smooth2$fitted, lwd=2, col="green")

# The CSV file named “CFPS_sample.csv” includes an adult subsample of the China 
# Family Panel Studies (CFPS) 2010 baseline survey. Please perform local
# -polynomial regression smoothing (i.e., constructing a LOESS curves) of math 
# (“mathtest”) and word (“wordtest”) test scores on age. You may try a few 
# different values of the argument span and select the smoothness you like best. 
# Briefly (in one sentence or a few sentences) describe the age patterns of math 
# and word test scores based on the smoothing results.

#       Math scores and word test scores both decline with age in an almost
#       linear pattern. Word test scores decline more sharply with age.


cfps_data <- fread("CFPS_sample.csv",
                     stringsAsFactors = F, 
                     data.table = F)

head(cfps_data)
tail(cfps_data)
names(cfps_data)

math <- cfps_data$mathtest
word <- cfps_data$wordtest
age <- cfps_data$age

plot(age, math)
age_x_math_smooth <- loess( math ~ age, span = 1.5)
age_x_math_smooth2 <- loess( math ~ age, span = 0.75)
lines(age, age_x_math_smooth$fitted, lwd=2, col="red")
lines(age, age_x_math_smooth2$fitted, lwd=2, col="green")

plot(age, word)
age_x_word_smooth <- loess( word ~ age, span = 1.5)
age_x_word_smooth2 <- loess( word ~ age, span = 0.75)
lines(age, age_x_word_smooth$fitted, lwd=2, col="red")
lines(age, age_x_word_smooth2$fitted, lwd=2, col="green")
