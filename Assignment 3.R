loc <- "~/Desktop/FA21/Advanced Demographic Analysis/Assignment 3"
setwd(loc)

library(readr)
data1 <- read_csv("GLF.csv")

data1$cohort = factor(data1$cohort)
data1$edu = factor(data1$edu)

table(data1$cohort)
table(data1$edu)

data1$cohort <- factor(data1$cohort, 
                      levels = c("Pre-famine", "Famine", "Post-famine"))
levels(data1$cohort)
table(data1$cohort)


data1$edu <- factor(data1$edu, 
                      levels = c("illiterate", "primary", ">=middle sch"))
levels(data1$edu)
table(data1$edu)

summary(data1$cohort)

bmi <- lm(r1bmi ~ city_cssi90 + cohort + male, data = data1)
summary(bmi)

bmi <- lm(r1bmi ~ city_cssi90 +
            I(cohort=="Pre-famine") +
          I(cohort=="Famine") +
            male, data = data1)
summary(bmi)

bmi <- lm(r1bmi ~ city_cssi90 +
            I(cohort=="Pre-famine") + I(cohort=="Pre-famine"):city_cssi90
          +
            I(cohort=="Famine") + I(cohort=="Famine"):city_cssi90 + male, data = data1)
summary(bmi)


#######

#DID for dichotomous "overweight" bmi variable
overweight <- lm(r1bmiwho_rsk ~ city_cssi90 +
            I(cohort=="Pre-famine") + I(cohort=="Pre-famine"):city_cssi90
          +
            I(cohort=="Famine") + I(cohort=="Famine"):city_cssi90 + male, data = data1)
summary(overweight)

#DID for dichotomous "overweight" waist circumference variable
waistcirc <- lm(r1wc_rsk ~ city_cssi90 +
            I(cohort=="Pre-famine") + I(cohort=="Pre-famine"):city_cssi90
          +
            I(cohort=="Famine") + I(cohort=="Famine"):city_cssi90 + male, data = data1)

summary(waistcirc)

#DID for height variable
height <- lm(r1htcm ~ city_cssi90 +
                        I(cohort=="Pre-famine") + I(cohort=="Pre-famine"):city_cssi90
                      +
                        I(cohort=="Famine") + I(cohort=="Famine"):city_cssi90 + male, data = data1)
summary(height)

#DID for dichotomous hypertension variable
hypertension <- lm(r1hbp ~ city_cssi90 +
            I(cohort=="Pre-famine") + I(cohort=="Pre-famine"):city_cssi90
          +
            I(cohort=="Famine") + I(cohort=="Famine"):city_cssi90 + male, data = data1)
summary(hypertension)

#DID for dichotomous diabetic variable
diabetes <- lm(r1diab ~ city_cssi90 +
            I(cohort=="Pre-famine") + I(cohort=="Pre-famine"):city_cssi90
          +
            I(cohort=="Famine") + I(cohort=="Famine"):city_cssi90 + male, data = data1)
summary(diabetes)

