#############################################################################
# Description: Code for completing an illustrative ANOVA project for 2      #
# factors, each with 2 levels (or treatments)  All data is synthetic.       #
# The results are to be used to illustrate ANOVA for BUSA 603.              #
# 																			                                    #	
# Last Update Date:  12/15/22       										                    #
#                                          									                #
# Last Update Date Comments:												                        #
#	A) 12/15/22 - Version 1 of code.											                    #
#																			                                      #
# Inputs:                                                                   #
# A) Parameters for generating synthetic data.                              #
#																			                                      #
# Outputs:																	                                #
#	A)	ANOVA results.          					                                    #
#																			                                      #	
# References:                                                               #
# A) https://www.scribbr.com/statistics/anova-in-r/                         #
# B) https://rdrr.io/cran/grove/src/R/GenerateSyntheticAnova.R              #
#                                                                           #    
# Author:  Brian Weikel														                          #
#############################################################################

library(pillar)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(grove)
library(wavethresh)
library(ggpubr)
library(dplyr)
library(tibble)

###### Parameter Inputs #####
# Seed value
seed <- 61411
# number of stores
n <- 80 
# Average revenue per selling square foot.  
# At a mall, VS stores have about 6,000 selling feet in 2012 and (say) $240K  
# revenue per month.   Thus r_mean is set to 40.  r_std has been set to 4.    
r_mean <- 40
# Standard deviation for the error term. 
r_std <- 3.5
# Note that 1/4 of stores receive last year's treatments, FS1 and W1, 
# 1/4 receive test treatments FS2 and W2, 1/4 receive FS1 and W2, and 
# 1/4 receive test FS2 and W1.
# Factor 1 FS2 Coefficient 
alpha <- 1.1 
# Factor 1 W2 Coefficient 
beta <- 1.35 
# Factor Interaction
gamma <- 0.95

##### End Parameter Inputs #####

cell_per = (1/4)*n

# Generating the Store ID 
set.seed(seed)
store_id <-  data.frame(stringi::stri_rand_strings(n, 8, 
  pattern = "[A-Za-z0-9]"))
store_df0 <- data.frame(store_id)
colnames(store_df0)[1] <- "store_id"
# Add row ID, treat as an integer based on random selection. 
store_df0$rand_num <- seq.int(nrow(store_df0))
# Factors
store_df0$window <- as.factor(ifelse(store_df0$rand_num < 2*cell_per +1,
                                     "W1","W2"))

store_df0$floorset <- as.factor(ifelse(store_df0$rand_num < cell_per +1,"FS1",
                           ifelse(store_df0$rand_num < 2*cell_per +1,"FS2",
                                  ifelse(store_df0$rand_num < 3*cell_per +1,
                                         "FS1","FS2"))))
store_df0$window_ind <- ifelse(store_df0$window=="W2",1,0)
store_df0$floorset_ind <- ifelse(store_df0$floorset=="FS2",1,0)

# Error specification 
set.seed(seed+1)
store_df0$error <- rnorm(n, 0, r_std)

# Outcome
store_df0$gross_sales <- r_mean + 
  alpha*store_df0$floorset_ind + 
  beta*store_df0$window_ind + 
  gamma*store_df0$floorset_ind * store_df0$window_ind+ 
  store_df0$error

store_df1 <- subset(store_df0,select=c('store_id','gross_sales','floorset',
                                       'window'))
# Visualizing Data

bplot <- ggplot(data = store_df1, aes(x = window, y = gross_sales, colour = floorset)) + 
  geom_boxplot() + 
  scale_y_continuous(labels=scales::dollar_format())
png("C:/Users/bkwei/R_projects/BUSA_603_ANOVA/Output/Summary_stats.png")
print(bplot + theme_classic(base_size = 20)+labs(x = "Windows",y = "Gross Sales",colour = "Floorset"))
dev.off()

# 2 Way ANOVA
#https://dzchilds.github.io/stats-for-bio/two-way-anova-in-r.html
win_fs_model <- lm(gross_sales ~ window + floorset + window : floorset, data = store_df1)

win_fst_model_anova <- anova(win_fs_model)

sink("C:/Users/bkwei/R_projects/BUSA_603_ANOVA/Output/ANOVA_statistics.txt")
win_fst_model_anova
sink()

# Visualization of Statistical Results
# step 1. calculate means for each treatment combination
gross_sales_means <- store_df1 %>% group_by(window,floorset) %>% 
  summarise(Means=mean(gross_sales),SEs = sd(gross_sales)/sqrt(n))
# # step 2. plot these as an interaction plot
result_plot <- ggplot(gross_sales_means, 
       aes(x = window, y = Means, colour = floorset, group = floorset)) +
  geom_point(size = 4) + geom_line() + 
  scale_y_continuous(labels=scales::dollar_format())

png("C:/Users/bkwei/R_projects/BUSA_603_ANOVA/Output/ANOVA_RESULTS.png")
print(result_plot + theme_classic(base_size = 20)+labs(x = "Windows",y = "Gross Sales Means",colour = "Floorset"))
dev.off()

