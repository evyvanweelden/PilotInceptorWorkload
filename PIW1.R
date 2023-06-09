### Calculation of PIW1 with Duty Cycle and Aggressiveness table created in Matlab ###

# Created on 11-01-2023 by Evy van Weelden: e.vanweelden@tilburguniversity.edu.
# Last updated on: 09-06-2023.

# This code includes equations taken from:	
# I. Niewind, “Pilot Gain and the Workload Buildup Flight Test Technique: A Closer Investigation of Pilot Inceptor Workload”, DLR-Interner Bericht, Report No. IB 111-2012/74, Oct. 2012, URL: https://elib.dlr.de/88208/. 

# This code calculates Normalized Aggressiveness and One-Dimensional measures of Pilot Inceptor Workload (PIW).


#######################
#### Load packages ####
library("tidyverse")
library("rstatix")
library("SciViews")

rm(list=ls())  #clear global environment

##################################
#### load and store mat table ####

setwd("C:/")                                              # set working directory to data storage
Table_DC_Agg <- readr::read_csv('Table_DC_Agg.csv')       # load table with values for Duty Cycle and Aggressiveness
Table_DC_Agg <- as.data.frame(Table_DC_Agg)               # Create data frame
setwd("C:/")                                              # change working directory if wanted

## Calculation of Normalized Aggressiveness and One-Dimensional PIW (PIW1) ##

# Context:
# Calculation of PIW1 requires a normalized value for Aggressiveness, which is then in the same range as Duty Cycle (0 to 1).
# It is based on the mathematical relationship between Aggressiveness and Duty Cycle.
# Try to fit an exponential regression model to this relationship. Then use its coefficients to create an inverse function.
# The inverse function creates Normalized Aggressiveness.
# For an in-depth explanation, please read the paper by Niewind (2012).

# Expontential functions #

model1 <- lm(log(Table_DC_Agg$Aggressiveness)~ Table_DC_Agg$Duty_cycle)
summary(model1)

# Using the coefficients from the output table, we can see that the fitted exponential regression equation ln(y) = a + b(x) is:
# ln(y) = model1$coefficients[1] + model1$coefficients[2](x)
# Applying e to both sides, we can rewrite the equation as:
a <- exp(model1$coefficients[1])
b <- exp(model1$coefficients[2])

# inverse function for normalization of Aggressiveness, eq. 13 in Niewind 2012. #
# ynorm = (ln(y/a))/b

Table_DC_Agg$Aggressiveness_norm <- (ln(Table_DC_Agg$Aggressiveness/a))/b
  
## Calculation of PIW1 measures, eq. 8 in Niewind 2012. #
# The square root of agg*dc.

Table_DC_Agg$OneD_PIW <- sqrt(Table_DC_Agg$Aggressiveness_norm*Table_DC_Agg$Duty_cycle)
