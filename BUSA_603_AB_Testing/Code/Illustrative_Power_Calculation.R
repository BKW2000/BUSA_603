#############################################################################
# Description: Code for completing an illustrative power calculation for    #
# a two-sample t-test of a (say) conversion lift study.											#			                          		
# (https://developers.facebook.com/docs/marketing-api/guides/lift-studies/) #
# This data is to be used to illustrate A/B Testing power calculationrs for #
# BUSA 603.                                                                 #
# 																			                                    #	
# Last Update Date:  12/12/22       										                    #
#                                          									                #
# Last Update Date Comments:												                        #
#	A) 12/12/22 - Version 1 of code.											                    #
#																			                                      #
# Inputs:                                                                   #
# A) Parameters for generating synthetic data.  Values taken from a         #
# particular instantiation of the R code Generate_AB_Test_Data.R            #
#																			                                      #
# Outputs:																	                                #
#	A)	A power analysis graph. 					                                    #
#																			                                      #	
# Reference2:                                                               #
# A) https://stats.oarc.ucla.edu/r/dae/                                     #
# power-analysis-for-two-group-independent-sample-t-test/                   #
# Author:  Brian Weikel														                          #
#############################################################################

library(pwr)

# Effect Size:  Test less Control
effect_size <- 0.1967 - 0.1887 # Initial values: 0.2130741 - 0.2076830.
  # Second set of values: 0.2165 - 0.2080
# Standard Error of Test:  Test and Control Pooled Value under the assumption of
# equal group sample sizes. 
std_error <- sqrt((1.58^2+1.56^2)/2)

p_50 <- pwr.t.test(d=effect_size/std_error,power=0.50,sig.level=0.10,
           type="two.sample",alternative="two.sided")
p_55 <- pwr.t.test(d=effect_size/std_error,power=0.55,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_60 <- pwr.t.test(d=effect_size/std_error,power=0.60,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_65 <- pwr.t.test(d=effect_size/std_error,power=0.65,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_70 <- pwr.t.test(d=effect_size/std_error,power=0.70,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_75 <- pwr.t.test(d=effect_size/std_error,power=0.75,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_80 <- pwr.t.test(d=effect_size/std_error,power=0.80,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_85 <- pwr.t.test(d=effect_size/std_error,power=0.85,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_90 <- pwr.t.test(d=effect_size/std_error,power=0.90,sig.level=0.10,
                   type="two.sample",alternative="two.sided")
p_95 <- pwr.t.test(d=effect_size/std_error,power=0.95,sig.level=0.10,
                   type="two.sample",alternative="two.sided")

power_values <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95)
sample_size_each_group <-c(p_50$n,p_55$n,p_60$n,p_65$n,p_70$n,p_75$n,
                           p_80$n,p_85$n,p_90$n,p_95$n)/1000

plot(sample_size_each_group, power_values, type = "b", pch = 19, 
     ylim = c(0.5, 1),
     col = "black", xlab = "Sample Size for Each Group in Thousands", 
     ylab = "Power")

