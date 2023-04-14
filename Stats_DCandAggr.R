### Visualizations and Statistics for Duty Cycle and Aggressiveness table created in Matlab ###

# Created on 11-01-2023 by Evy van Weelden: e.vanweelden@tilburguniversity.edu.
# Last updated on: 14-04-2023.

# This code includes equations taken from:	
# I. Niewind, “Pilot Gain and the Workload Buildup Flight Test Technique: A Closer Investigation of Pilot Inceptor Workload”, DLR-Interner Bericht, Report No. IB 111-2012/74, Oct. 2012, URL: https://elib.dlr.de/88208/. 

# This code calculates Normalized Aggressiveness and One-Dimensional measures of Pilot Inceptor Workload (PIW).
# Also includes statistical tests and modelling, and checking test/model assumptions.

#######################
#### Load packages ####
library("ggplot2")
library("tidyverse")
library("performance")
library("dplyr")
library("ggpubr")
library("PairedData")
library("car")
library("coin")
library("qpcR")  
library(plotly)
library(grid)
library(gridExtra)
library(lme4)
library(tidyr)
library(rstatix)
library(SciViews)
library(lmerTest)
library(nlme)
library(parameters)
library(merDeriv)
library(insight)
library(corrplot)
library(ggrepel)

rm(list=ls())  #clear global environment

##################################
#### load and store mat table ####

setwd("C:/")                                              # set working directory to data storage
Table_DC_Agg <- readr::read_csv('Table_DC_Agg.csv')       # load table with values for Duty Cycle and Aggressiveness
Table_DC_Agg <- as.data.frame(Table_DC_Agg)               # Create data frame
setwd("C:/")                                              # change working directory if wanted

## Making sure the variables for subject, condition and trial (columns) are a factor ##

Table_DC_Agg$Subject <- factor(Table_DC_Agg$Subject)
Table_DC_Agg$Trial <- factor(Table_DC_Agg$Trial)
Table_DC_Agg$Condition <- factor(Table_DC_Agg$Condition, levels = c("Low workload", "High workload"))

## visualization of data ##

colorBlindBlack8  <- c("#3d3d3b", "#E69F00", "#D55E00", "#009E73", 
                       "#CC79A7", "#56B4E9")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# boxplot Longitudinal Duty Cycle per condition
p <- ggboxplot(Table_DC_Agg, x="Condition", y="Duty_cycle_longitudinal")  +   geom_boxplot(fill='#A4A4A4', color="black") +
  theme_classic() + geom_jitter(aes(colour = Subject),width = 0.25)
p + labs(x = "Condition") + labs(title = "Duty cycle (longitudinal)") + ylab("Duty cycle") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 5),text = element_text(size = 16)) + scale_x_discrete("Workload", labels = c("High workload" = "High","Low workload" = "Low"))+
  theme(plot.margin = unit(c(1,2,1,2), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=5), size = 16)) + #adds margin to x-axis title
  theme(axis.title.y = element_text(size = 16)) + #adds margin to x-axis title
  theme(axis.text.x= element_text(size = 16, color = "black")) +
  theme(text = element_text(family = "serif")) # 

# boxplot lateral Duty Cycle per condition
p <- ggboxplot(Table_DC_Agg, x="Condition", y="Duty_cycle_lateral")  +   geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic() + geom_jitter(aes(colour = Subject),width = 0.25)
p + labs(x = "Condition") + labs(title = "Duty cycle (lateral)") + ylab("Duty cycle") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 5),text = element_text(size = 16)) + scale_x_discrete("Workload", labels = c("High workload" = "High","Low workload" = "Low"))+
  theme(plot.margin = unit(c(1,2,1,2), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=5), size = 16)) + #adds margin to x-axis title
  theme(axis.title.y = element_text(size = 16)) + #adds margin to x-axis title
  theme(axis.text.x= element_text(size = 16, color = "black")) +
  theme(text = element_text(family = "serif")) # 

# boxplot longitudinal Aggressiveness per condition
p <- ggboxplot(Table_DC_Agg, x="Condition", y="Aggressiveness_longitudinal")  +   geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()# + geom_jitter(aes(colour = Subject),width = 0.25)
p + labs(x = "Condition") + labs(title = "Aggressiveness (longitudinal)") + ylab("Aggressiveness") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 5),text = element_text(size = 16)) + scale_x_discrete("Workload", labels = c("High workload" = "High","Low workload" = "Low"))+
  theme(plot.margin = unit(c(1,2,1,2), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=5), size = 16)) + #adds margin to x-axis title
  theme(axis.title.y = element_text(size = 16)) + #adds margin to x-axis title
  theme(axis.text.x= element_text(size = 16, color = "black")) +
  theme(text = element_text(family = "serif")) # 

# boxplot lateral Aggressiveness per condition
p <- ggboxplot(Table_DC_Agg, x="Condition", y="Aggressiveness_lateral")  +   geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()# + geom_jitter(aes(colour = Subject),width = 0.25)
p + labs(x = "Condition") + labs(title = "Aggressiveness (lateral)") + ylab("Aggressiveness") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 5),text = element_text(size = 16)) + scale_x_discrete("Workload", labels = c("High workload" = "High","Low workload" = "Low"))+
  theme(plot.margin = unit(c(1,2,1,2), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=5), size = 16)) + #adds margin to x-axis title
  theme(axis.title.y = element_text(size = 16)) + 
  theme(axis.text.x= element_text(size = 16, color = "black")) +
  theme(text = element_text(family = "serif")) # 

# PIW plot - Longitudinal, shaped by condition and colored by subject
a <- ggplot(Table_DC_Agg,aes(x=Duty_cycle_longitudinal,y=Aggressiveness_longitudinal, color = Subject, shape = Condition)) +
  geom_point(size=3) + theme_classic()
a + labs(x = "Duty Cycle") + labs(title = "Pilot Inceptor Workload (Longitudinal)") + ylab("Aggressiveness")+
  theme(text = element_text(family = "serif")) + theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16))  +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))+
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values=cbPalette)

# gray scaled and numbered
a <- ggplot(Table_DC_Agg,aes(x=Duty_cycle_longitudinal,y=Aggressiveness_longitudinal, label = Subject, shape = Condition)) +
     geom_point(size=3) + geom_text(hjust = 0,vjust=0, nudge_x = 0.003)+ theme_minimal() 
a + labs(x = "Duty Cycle") + labs(title = "Pilot Inceptor Workload (Longitudinal)") + ylab("Aggressiveness")+
  theme(text = element_text(family = "serif")) + theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16))  +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))+
  scale_y_continuous(labels = scales::comma)

# PIW plot - Lateral, shaped by condition and colored by subject
a <- ggplot(Table_DC_Agg,aes(x=Duty_cycle_lateral,y=Aggressiveness_lateral, color = Subject, shape = Condition)) +
  geom_point(size=3) + theme_classic()
a + labs(x = "Duty Cycle") + labs(title = "Pilot Inceptor Workload (Lateral)") + ylab("Aggressiveness") +
  theme(text = element_text(family = "serif")) + theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::comma)+
  scale_colour_manual(values=cbPalette)

# gray scaled and numbered
a <- ggplot(Table_DC_Agg,aes(x=Duty_cycle_lateral,y=Aggressiveness_lateral, label = Subject, shape = Condition)) +
  geom_point(size=3) + geom_text(hjust = 0,vjust=0, nudge_x = 0.003) + theme_minimal()
a + labs(x = "Duty Cycle") + labs(title = "Pilot Inceptor Workload (Lateral)") + ylab("Aggressiveness") +
  theme(text = element_text(family = "serif")) + theme(axis.title.y = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16)) +
 scale_y_continuous(labels = scales::comma)

## Calculation of Normalized Aggressiveness and One-Dimensional PIW (PIW1)

# Context:
# Calculation of PIW1 requires a normalized value for Aggressiveness, which is then in the same range as Duty Cycle (0 to 1).
# It is based on the mathematical relationship between Aggressiveness and Duty Cycle.
# Try to fit an exponential regression model to this relationship. Then use its coefficients to create an inverse function.
# The inverse function creates Normalized Aggressiveness.
# For an in-depth explanation, please read the paper by Niewind (2012).

## Expontential functions ##

# longitudinal data #

model1 <- lm(log(Table_DC_Agg$Aggressiveness_longitudinal)~ Table_DC_Agg$Duty_cycle_longitudinal)
summary(model1)

# Using the coefficients from the output table, we can see that the fitted exponential regression equation is:
# ln(y) = -9.9299 + 3.6530(x)
# Applying e to both sides, we can rewrite the equation as:
a <- exp(model1$coefficients[1])
b <- exp(model1$coefficients[2])

# inverse function for normalization of Aggressiveness, eq. 13 in Niewind 2012.
# ynorm = (ln(y/a))/b

Table_DC_Agg$Aggressiveness_longitudinal_norm <- (ln(Table_DC_Agg$Aggressiveness_longitudinal/a))/b
  
## lateral data ##

model2 <- lm(log(Table_DC_Agg$Aggressiveness_lateral)~ Table_DC_Agg$Duty_cycle_lateral)
summary(model2)

# Using the coefficients from the output table, we can see that the fitted exponential regression equation is:
# ln(y) = -9.3420 + 3.5915(x)
# Applying e to both sides, we can rewrite the equation as:
a <- exp(model2$coefficients[1])
b <- exp(model2$coefficients[2])

# inverse function for normalization of Aggressiveness, eq. 13 in Niewind 2012.
# ynorm = (ln(y/a))/b
Table_DC_Agg$Aggressiveness_lateral_norm <- (ln(Table_DC_Agg$Aggressiveness_lateral/a))/b

## Calculation of PIW1 measures, eq. 8 in Niewind 2012.
# The square root of agg*dc.

Table_DC_Agg$OneD_PIW_longitudinal <- sqrt(Table_DC_Agg$Aggressiveness_longitudinal_norm*Table_DC_Agg$Duty_cycle_longitudinal)
Table_DC_Agg$OneD_PIW_lateral <- sqrt(Table_DC_Agg$Aggressiveness_lateral_norm*Table_DC_Agg$Duty_cycle_lateral)


## Visualize PIW1 data ##

# boxplot of Longitudinal PIW1 per condition
p <- ggboxplot(Table_DC_Agg, x="Condition", y="OneD_PIW_longitudinal")  +   geom_boxplot(fill='#A4A4A4', color="black") +
  theme_classic() + geom_jitter(aes(color = Subject),width = 0.25,size=2)
p + labs(x = "Condition") + labs(title = "One-dimensional PIW (longitudinal)") + ylab("PIW") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 5),text = element_text(size = 16)) + scale_x_discrete("Workload", labels = c("Low workload" = "Low","High workload" = "High"))+
  theme(plot.margin = unit(c(1,2,1,2), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=5), size = 16)) + #adds margin to x-axis title
  theme(axis.title.y = element_text(size = 16)) + #adds margin to x-axis title
  theme(axis.text.x= element_text(size = 16, color = "black")) +
  theme(text = element_text(family = "serif")) +
  scale_colour_manual(values=colorBlindBlack8)

# boxplot of Lateral PIW1 per condition
p <- ggboxplot(Table_DC_Agg, x="Condition", y="OneD_PIW_lateral")  +   geom_boxplot(fill='#A4A4A4', color="black") +
  theme_classic() + geom_jitter(aes(color = Subject),width = 0.25,size=2)
p + labs(x = "Condition") + labs(title = "One-dimensional PIW (lateral)") + ylab("PIW") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 5),text = element_text(size = 16)) + scale_x_discrete("Workload", labels = c("Low workload" = "Low","High workload" = "High"))+
  theme(plot.margin = unit(c(1,2,1,2), "cm")) +
  theme(axis.title.x = element_text(margin=margin(t=5), size = 16)) + #adds margin to x-axis title
  theme(axis.title.y = element_text(size = 16)) + #adds margin to x-axis title
  theme(axis.text.x= element_text(size = 16, color = "black")) +
  theme(text = element_text(family = "serif")) +
  scale_colour_manual(values=colorBlindBlack8)

#### Statistics ####

## Tests for normality ##

u <- shapiro.test(Table_DC_Agg$Duty_cycle_longitudinal)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg$Duty_cycle_lateral)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg$Aggressiveness_lateral)
u # significant, not normal
u <- shapiro.test(Table_DC_Agg$Aggressiveness_lateral_norm)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg$Aggressiveness_longitudinal)
u # significant, not normal
u <- shapiro.test(Table_DC_Agg$Aggressiveness_longitudinal_norm)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg$OneD_PIW_longitudinal)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg$OneD_PIW_lateral)
u # not significant, normal

Table_DC_Agg_split <- split(Table_DC_Agg,Table_DC_Agg$Condition) #split by condition

u <- shapiro.test(Table_DC_Agg_split$`High workload`$Duty_cycle_longitudinal)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$Duty_cycle_longitudinal)
u # not significant, normal

u <- shapiro.test(Table_DC_Agg_split$`High workload`$Duty_cycle_lateral)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$Duty_cycle_lateral)
u # not significant, normal

u <- shapiro.test(Table_DC_Agg_split$`High workload`$Aggressiveness_lateral)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$Aggressiveness_lateral)
u # significant, not normal

u <- shapiro.test(Table_DC_Agg_split$`High workload`$Aggressiveness_longitudinal)
u # significant, not normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$Aggressiveness_longitudinal)
u # not significant, normal

u <- shapiro.test(Table_DC_Agg_split$`High workload`$Aggressiveness_lateral_norm)
u # significant, not normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$Aggressiveness_lateral_norm)
u # significant, not normal

u <- shapiro.test(Table_DC_Agg_split$`High workload`$Aggressiveness_longitudinal_norm)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$Aggressiveness_longitudinal_norm)
u # not significant, normal

u <- shapiro.test(Table_DC_Agg_split$`High workload`$OneD_PIW_longitudinal)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$OneD_PIW_longitudinal)
u # not significant, normal

u <- shapiro.test(Table_DC_Agg_split$`High workload`$OneD_PIW_lateral)
u # not significant, normal
u <- shapiro.test(Table_DC_Agg_split$`Low workload`$OneD_PIW_lateral)
u # not significant, normal

## outlier detection ##

Boxplot(Table_DC_Agg$Duty_cycle_longitudinal,id.method="y") #tagged: none
Boxplot(Table_DC_Agg$Duty_cycle_lateral,id.method="y") #tagged: none
Boxplot(Table_DC_Agg$Aggressiveness_longitudinal,id.method="y") #tagged: 16,19,21
Boxplot(Table_DC_Agg$Aggressiveness_lateral,id.method="y") #tagged: 31
Boxplot(Table_DC_Agg$Aggressiveness_longitudinal_norm,id.method="y") #tagged: 16,19
Boxplot(Table_DC_Agg$Aggressiveness_lateral_norm,id.method="y") #tagged: 31
Boxplot(Table_DC_Agg$OneD_PIW_longitudinal,id.method="y")  #none
Boxplot(Table_DC_Agg$OneD_PIW_lateral,id.method="y") #none
Boxplot(Table_DC_Agg_split$`Low workload`$OneD_PIW_longitudinal,id.method="y") #none
Boxplot(Table_DC_Agg_split$`High workload`$OneD_PIW_longitudinal,id.method="y") #none

which(is_outlier(Table_DC_Agg$Aggressiveness_longitudinal)) # Default is > Q3 + 1.5*IQR and < Q1 - 1.5*IQR
which(is_outlier(Table_DC_Agg$Aggressiveness_longitudinal,coef=3)) # extreme datapoint # none

which(is_outlier(Table_DC_Agg$Aggressiveness_lateral)) # Default is > Q3 + 1.5*IQR and < Q1 - 1.5*IQR
which(is_outlier(Table_DC_Agg$Aggressiveness_lateral,coef = 3)) # extreme datapoint # none

which(is_outlier(Table_DC_Agg$Aggressiveness_lateral_norm)) # Default is > Q3 + 1.5*IQR and < Q1 - 1.5*IQR
which(is_outlier(Table_DC_Agg$Aggressiveness_lateral_norm,coef=3)) # extreme datapoint # none

which(is_outlier(Table_DC_Agg$Aggressiveness_longitudinal_norm)) # Default is > Q3 + 1.5*IQR and < Q1 - 1.5*IQR
which(is_outlier(Table_DC_Agg$Aggressiveness_longitudinal_norm,coef=3)) # extreme datapoint # none

## t-tests for normally distributed data ##

t.test(Table_DC_Agg_split$`High workload`$Duty_cycle_longitudinal,Table_DC_Agg_split$`Low workload`$Duty_cycle_longitudinal, paired = TRUE, alternative = "two.sided")
highsamples <- Table_DC_Agg_split$`High workload`$Duty_cycle_longitudinal
lowsamples  <- Table_DC_Agg_split$`Low workload`$Duty_cycle_longitudinal
Difference <- (lowsamples) - (highsamples)
cohensd <- ( mean(lowsamples) - mean(highsamples) ) / sd(Difference)
cohensd
# t(35) = 5.51, p < .001. cohen's d = -.92.
cohens_d(Table_DC_Agg,Duty_cycle_longitudinal ~ Condition,paired=TRUE)

t.test(Table_DC_Agg_split$`High workload`$Duty_cycle_lateral,Table_DC_Agg_split$`Low workload`$Duty_cycle_lateral, paired = TRUE, alternative = "two.sided")
highsamples <- Table_DC_Agg_split$`High workload`$Duty_cycle_lateral
lowsamples  <- Table_DC_Agg_split$`Low workload`$Duty_cycle_lateral
Difference <- (lowsamples) - (highsamples)
cohensd <- ( mean(lowsamples) - mean(highsamples) ) / sd(Difference)
cohensd
# t(35) = 1.15, p = .26. cohen's d = -.19.
cohens_d(Table_DC_Agg,Duty_cycle_lateral ~ Condition,paired=TRUE)

t.test(Table_DC_Agg_split$`High workload`$Aggressiveness_longitudinal_norm,Table_DC_Agg_split$`Low workload`$Aggressiveness_longitudinal_norm, paired = TRUE, alternative = "two.sided")
highsamples <- Table_DC_Agg_split$`High workload`$Aggressiveness_longitudinal_norm
lowsamples  <- Table_DC_Agg_split$`Low workload`$Aggressiveness_longitudinal_norm
Difference <- (lowsamples) - (highsamples)
cohensd <- ( mean(lowsamples) - mean(highsamples) ) / sd(Difference)
cohensd
# t(35) = 3.84, p < .001. cohen's d = -.64.

t.test(Table_DC_Agg_split$`High workload`$OneD_PIW_lateral,Table_DC_Agg_split$`Low workload`$OneD_PIW_lateral, paired = TRUE, alternative = "two.sided")
highsamples <- Table_DC_Agg_split$`High workload`$OneD_PIW_lateral
lowsamples  <- Table_DC_Agg_split$`Low workload`$OneD_PIW_lateral
Difference <- (lowsamples) - (highsamples)
cohensd <- ( mean(lowsamples) - mean(highsamples) ) / sd(Difference)
cohensd
# t(35) = 1.76, p = .09. cohen's d = -.29

t.test(Table_DC_Agg_split$`High workload`$OneD_PIW_longitudinal,Table_DC_Agg_split$`Low workload`$OneD_PIW_longitudinal, paired = TRUE, alternative = "two.sided")
highsamples <- Table_DC_Agg_split$`High workload`$OneD_PIW_longitudinal
lowsamples  <- Table_DC_Agg_split$`Low workload`$OneD_PIW_longitudinal
Difference <- (lowsamples) - (highsamples)
cohensd <- ( mean(lowsamples) - mean(highsamples) ) / sd(Difference)
cohensd
# t(35) = 4.99, p < .001. cohen's d = -.83.

## wilcoxon signed-rank tests (paired) for non-parametric data ##

res <- wilcox.test(Table_DC_Agg_split$`High workload`$Aggressiveness_lateral,Table_DC_Agg_split$`Low workload`$Aggressiveness_lateral, paired = TRUE, conf.int = TRUE, conf.level = 0.95, exact = FALSE) 
res 
Zstat<-qnorm(res$p.value/2) # 
Zstat 
effectsize <-  abs(Zstat)/sqrt(36) # Effect size r, uses number of pairs (n=36).
effectsize
pvaluedetailed<-res$p.value
pvaluedetailed
# Z = -2.02, p = .04.

res <- wilcox.test(Table_DC_Agg_split$`High workload`$Aggressiveness_longitudinal,Table_DC_Agg_split$`Low workload`$Aggressiveness_longitudinal, paired = TRUE, conf.int = TRUE, conf.level = 0.95, exact = FALSE) 
res 
Zstat<-qnorm(res$p.value/2) # 
Zstat 
effectsize <-  abs(Zstat)/sqrt(36) # Effect size r, uses number of pairs (n=36).
effectsize
pvaluedetailed<-res$p.value
pvaluedetailed
# Z = -3.40, p < .001.

## Correlation matrix ##

# create new data frame with target variables
df.dc_agg <- Table_DC_Agg %>%
  select(Duty_cycle_longitudinal, Duty_cycle_lateral, Aggressiveness_longitudinal, Aggressiveness_lateral, OneD_PIW_longitudinal, OneD_PIW_lateral)

# calculate correlations
dfcor1 <- cor(df.dc_agg, method = "spearman")

# plot matrix
corrplot(dfcor1, type = "upper",  
         tl.col = "black", tl.srt = 45, method = "number")

## check for a linear relationship of trial and PIW1 ##

# visualization of data
ggplot(data=Table_DC_Agg, aes(x=Trial,y=OneD_PIW_longitudinal, group=Subject)) + geom_line()
ggplot(data=Table_DC_Agg, aes(x=Trial,y=OneD_PIW_longitudinal, group=Subject)) + geom_line()+geom_smooth()

# lm for checking effect of trial on PIW1 #
modeltrial <- lm(OneD_PIW_lateral ~ Trial, data = Table_DC_Agg)
summary(modeltrial)
modeltrial <- lm(OneD_PIW_longitudinal ~ Trial, data = Table_DC_Agg)
summary(modeltrial)

## Testing for Multicollinearity ## 

# ARE LATERAL AND LONGITUDINAL PIW1 CORRELATED?
# PIW1s are normally distributed, hence Pearson method is chosen.
# Assumption of Homoscedasticity: ggplot(data=Table_DC_Agg,aes(x=OneD_PIW_longitudinal,y=OneD_PIW_lateral)) + geom_point()
cor.test(Table_DC_Agg$OneD_PIW_longitudinal, Table_DC_Agg$OneD_PIW_lateral, method = "pearson")
# Variables are correlated.
# Check previous correlation matrix, and the VIF values from models below.

## Testing models ##

Table_DC_Agg <- Table_DC_Agg %>%
  mutate(Condition = relevel(Condition, ref = "Low workload"))

## Model4 below has singular fit and has not been used in manuscript:
# model4 <- glmer(formula = Condition ~ OneD_PIW_lateral + OneD_PIW_longitudinal + (1|Subject), data = Table_DC_Agg, family = binomial)
# summary(model4)
# Anova(model4,type="III")
# check_model(model4)

## Model without random factor for subject, used in manuscript:
model4.2 <- glm(formula = Condition ~ OneD_PIW_lateral + OneD_PIW_longitudinal, data = Table_DC_Agg, family = binomial)
summary(model4.2)
Anova(model4.2,type="III")
check_model(model4.2)
model_performance(model4.2)
# odds ratios
exp(coef(model4.2))

# check model assumptions
check_model(model4.2)
model_parameters(model4.2)

# calculate VIF of PIW1 measures
vif(model4.2)
# also see: check_model(model4.2)
# vif values are 1.51. Which is low, hence does not require further attention.
