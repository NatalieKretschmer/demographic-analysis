# Similar to the Austrian data used in class, the CSV file named
# “RUS.M.ASDR.1987&1994” includes data on age-specific death rates for Russian 
# men in 1987 (column name “ASDR1987”) and 1994 (column name “ASDR1994”).
# Following the similar procedure used in the Austrian example (see the “Example
# in R” in the course materials on the Blackboard), please conduct a 
# line-integral decomposition of the change in life expectancy at birth for 
# males in Russia between 1987 and 1994 into effects of changes in age-specific 
# death rates. Note that you need to use the R program e0.r just like the 
# Austrian example. The e0 function needs to be fed to the line- integral 
# decomposition () as an argument.

install.packages("DemoDecomp")

library(DemoDecomp) # Prior to execution of this command, DemoDecomp needs to be installed
getwd()
setwd("~/Desktop/FA21/Advanced Demographic Analysis/Assignment 2")
# enter the address of the folder in which the programs and data were saved
dir("~/Desktop/FA21/Advanced Demographic Analysis/Assignment 2")
source("e0.r")
X <- read.csv("RUS.M.ASDR.1987&1994(1).csv")
str(X); View(X)
e0.1987 <- e0(X$ASDR1987); e0.1994 <- e0(X$ASDR1994)

# In your solution, consider including the following components:

#  (a) the output of “print(round(effects, 2))” copied from the R console,
print(paste("e0 in 1987 ", round(e0.1987,2)))
print(paste("e0 in 1994 ", round(e0.1994,2)))
print(paste("difference ", round(e0.1994-e0.1987,2)))

effects <- horiuchi(e0,X$ASDR1987,X$ASDR1994,20)
effects <- c(sum(effects), effects)
names(effects) <- c("Total",paste(X$Age))

print(round(effects,2))
View(effects)

#  (b) a copy of the bar graph,
barplot(effects[2:23],las=1)
barplot(effects[2:23],horiz=TRUE,las=1)

#  (c) a summary table such as lecture slides 5 and 59 (feel free to be 
#    creative in formatting the table)
effects_df <- as.data.frame(effects)
install.packages("dplyr")                               # Install & load dplyr
library("dplyr")

effects_df2 <- effects_df
effects_df2 <- tibble::rownames_to_column(effects_df2, "Age")

merged_data <- merge(X, effects_df2, by = "Age")

write.csv(merged_data,"~/Desktop/FA21/Advanced Demographic Analysis/Assignment 2/merged data.csv", row.names = FALSE)

#  (d) a brief, few line description of the results.


