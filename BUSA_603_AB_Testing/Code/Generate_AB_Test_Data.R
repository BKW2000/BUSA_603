#############################################################################
# Description: Code for generating synthetic data for a (say) Facebook      #
# conversion lift study														                          #		
# (https://developers.facebook.com/docs/marketing-api/guides/lift-studies/) #
# This data is to be used to illustrate A/B Testing for BUSA 603.           #
# 																			                                    #	
# Last Update Date:  12/8/22       											                    #
#                                          									                #
# Last Update Date Comments:												                        #
#	A) 12/8/22 - Version 1 of code.											                      #
#																			                                      #
# Inputs:                                                                   #
# A) Parameters for generating synthetic data.  See below.			            #
# B) State_Population_Proportions_2021.csv, state population proportions    #
#   for 2021.                                                               #
#																			                                      #
# Outputs:																	                                #
#	A)																		                                    #
#																			                                      #	
# Author:  Brian Weikel														                          #
#############################################################################

# Importing Libraries
library(random)
library(stringi)
library(extraDistr)
library(dplyr)
library(data.table)
library(EnvStats)
library(MASS)
library(actuar)
library(VGAM)
library(psych)
library(ISLR)
library(tidyverse)
library(Rfast)
library(foreign)
library(xtable)
library(stargazer)

# Read Input Data
state_pop0 <- as.data.frame(
  read.csv(file = 'Input_Data/State_Population_Proportions_2021.csv'))
names(state_pop0)
state_pop1 <- state_pop0[order(state_pop0$Upper),]

########### Inputs for synthetic data ###########

# Sample audience size;  an integer larger than 1.
n <- 1000000
# Test group percentage.
test_prop <- 0.5
# The variable g_alpha maps to the parameter alpha and g_beta to the parameter beta
# of the Beta Binomial distribution.  We recommend the g_alpha be 600
# and g_beta be 400. Given this structure and for the variable gender_3
# below, 0 maps to unknown, 1 maps to men, and 2 maps to women.  The recommended
# parameter setting will produce about 56% women, 38% men, and 6% unknown.  
g_alpha <- 600
g_beta <- 200
# Generating age and platform tenure via a multivarate normal distribution. 
# Mean, standard deviation, and covariance required. An age less than 18 or 
# larger than 89 maps to unknown.  Age is in years and tenure is in months.
a_mean <- 40
a_var <- 14^2
t_mean <- 42
t_var <- 20^2
a_t_cov <- 100

# First Date of Campaign
first_date <- as.Date(c("2022-02-06"))
# Last Date of Campaign
last_date <- as.Date(c("2022-03-19"))
# Last Date of Purchase
last_date_p <- as.Date(c("2022-04-02"))
# Exposure rate for Control group ... Currently not used. 
exp_rate <- 0.7
# Choose lambda for the Poisson distribution to determine the first day of 
# for those users exposed.
lambda <- 15
# Starting with " Generating potential first exposure date via Truncated Log 
# Normal Distribution" below, parameters were hard-coded FOR NOW!

#Seed Value
z <- 61411

########### Commence Generation of Data ###########

#Randomly Generate Account ID
set.seed(z)
account_id <-  stringi::stri_rand_strings(1, 14, pattern = "[A-Za-z0-9]")
# Randomly Generate String for Campaign ID
set.seed(z+1)
campaign_id <-  stringi::stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
# Randomly Generate 2 String for Cell ID
# If first character is T, then Test Group; otherwise Control Group.
set.seed(z+2)
cell_id0 <-  stringi::stri_rand_strings(2, 7, pattern = "[A-Za-z0-9]")
cell_id <- c(gsub(" ","",paste("T",cell_id0[1]))
            ,gsub(" ","",paste("C",cell_id0[2])))

# Randomly Generate String for User ID
set.seed(z+3)
user_id <-  stringi::stri_rand_strings(n, 12, pattern = "[A-Za-z0-9]")

# Generate Gender for User IDs via Beta Binomial
set.seed(z+4)
gender_3 <- rbbinom(n, 2, alpha = g_alpha, beta = g_beta)
table(gender_3)

# Generate Age for User IDs - Initial Univariate Code has been Commented Out
set.seed(z+5)
#age_norm0 <- round(rnorm(n, a_mean, a_std),digits=0)
#summary(age_norm0)
#age_norm1 <- ifelse(age_norm0 > 89, NA, age_norm0)
#summary(age_norm1)
#age <- ifelse(age_norm1 < 18, NA, age_norm1)
#table(age)

mvn_mean <- c(a_mean,t_mean)
mvn_sigma <-  matrix(c(a_var,a_t_cov,a_t_cov,t_var), ncol=2)
a_t <- round(mvrnorm(n=n, mu=mvn_mean, Sigma=mvn_sigma),digits=0)
summary(a_t)
quantile(a_t[,1], probs = seq(0.05, 0.95, by = 0.05))
quantile(a_t[,2], probs = seq(0.05, 0.95, by = 0.05))
cor(a_t[,1],a_t[,2])

age_norm1 <- ifelse(a_t[,1] > 90, NA, a_t[,1])
age <- ifelse(age_norm1 < 18, NA, age_norm1)

ten_norm1 <- ifelse(a_t[,2] > 160, NA, a_t[,2])
tenure <- ifelse(ten_norm1 < 1, 1, ten_norm1)

summary(age)
summary(tenure)
quantile(age, probs = seq(0.05, 0.95, by = 0.05),na.rm = TRUE)
quantile(tenure, probs = seq(0.05, 0.95, by = 0.05),na.rm = TRUE)

# Generating the State for User IDs using the Uniform Distribution where any 
# value greater than 1 maps to unknown.
set.seed(z+6)
u_0_11_0 <- data.frame(runif(n, min = 0, max = 1.1))
colnames(u_0_11_0) <- "p_uniform"
# https://stackoverflow.com/questions/23934361/merge-2-dataframes-if-value-within-range
u_0_11_1 <- setDT(u_0_11_0)[setDT(state_pop1), 
             on = .(p_uniform >= Lower, p_uniform < Upper), 
             State := State][]
u_0_11_2 <- subset(u_0_11_1, select = c(State))
u_0_11_2[is.na(u_0_11_2)] <- "Unkown"
colnames(u_0_11_2) <- "state"

state_list <-list("Florida","Hawaii","California","Arizona","Texas",
                  "New Mexico","Louisiana","Mississippi","Alabama","Georgia",
                  "Nevada")
state_list2 <- list("Alaska","Idaho","Montana","Wyoming","South Dakota",
                    "North Dakota","Minnesota","Wisconsin","Michigan",
                    "Utah","Colorado","Nebraska","Iowa")

warm <- ifelse(u_0_11_2$state %in% state_list,1,0)
cold <- ifelse(u_0_11_2$state %in% state_list2,1,0)

# Random Assignment to Treatment or Control
set.seed(z+7)
t_c_0 <- data.frame(runif(n, min = 0, max = 1))
t_c_1 <- ifelse(t_c_0 < test_prop, "Test", "Control")
colnames(t_c_1) <- "test_variant"

# Assignment for Exposure ... Is truly applicable to Treatment Group
# Exposure is a function of age, gender and climate.  Thus targeting! 

mean_age <- round(mean(age,na.rm = TRUE),digits=0)
age_impute <- as.vector(ifelse(is.na(age),mean_age,age))
age_35_54 <-ifelse(age_impute<55 & age_impute>35,1,0)
women <- ifelse(gender_3==2,1,0)
unknown <- ifelse(gender_3==0,1,0)

#Coefficients
eta_0 <- 0.5  # As initially designed, this value was 1.0.
eta_women <- -0.45
eta_unknown <- -1.8
eta_age_35_54 <- 0.15
eta_warm <- -0.095
eta_cold <- 0.105

lp_00 <- eta_0 + 
  eta_women*women +
  eta_unknown*unknown +
  eta_age_35_54*age_35_54 +
  eta_warm*warm +
  eta_cold*cold
pr_00 <- 1/(1+exp(-lp_00))
set.seed(z+8)
exp_1 = data.frame(rbinom(n,1,pr_00))      # bernoulli response variable
names(exp_1)[1] <- "exposed_0_1"
quantile(pr_00, probs = seq(0.01, 0.99, by = 0.01))
summary(exp_1)

##### Original Code for Exposures ... Which is Random ... is Below #####
#set.seed(z+8)
#exp_0 <- data.frame(runif(n, min = 0, max = 1))
#exp_1 <- ifelse(exp_0 < exp_rate, 1, 0)
#summary(exp_1)
#colnames(exp_1) <- "exposed_0_1"

# Generating potential first exposure date via Truncated Log Normal Distribution
camp_length <- last_date - first_date + 1
set.seed(z+9)
day_exp0 <-  round(rlnormTrunc(n, meanlog = 2.0, sdlog = 1.0, min = 0, 
                               max = camp_length -1), digits=0)
summary(day_exp0)
table(day_exp0)
day_exp1 <- data.frame(first_date + day_exp0) 
table(day_exp1)
colnames(day_exp1) <- "first_date_exp"

# Number of potential impressions via Truncated Poisson Distribution 
set.seed(z+10)
exposure_0 <-  data.frame(rtpois(n, 2, a = 0, b = 10))
summary(exposure_0)
table(exposure_0)
colnames(exposure_0) <- "impressions"

# Combining Data
df1 <- cbind(user_id,gender_3,age,tenure,u_0_11_2,warm, cold, t_c_1,
             exp_1,day_exp1,exposure_0)

# Modifying Exposure and Number of Impressions
# 1) IF TEST_VARIANT IS TEST THEN SET EXPOSED_0_1 TO 0, FIRST_DATA_EXP TO "" , 
# AND IMPRESSION TO 0
# 2) IF TEST_VARIANT IS CONTROL AND EXPOSED_0_1 = 0 SET FIRST_DATA_EXP TO "" , 
# AND IMPRESSION TO 0

df1$exposed_ind <- ifelse(df1$test_variant=="Test",df1$exposed_0_1,0)
df1$imps <- ifelse(df1$test_variant=="Test" & df1$exposed_ind==1,
                      df1$impressions,0)
df1$date_first_exp <- fifelse(df1$exposed_ind==0, as.Date(c("")),
                                 df1$first_date_exp)

########### df2 is the data frame of interest in what follows. ##########

df2 <- subset(df1, select = -c(exposed_0_1,first_date_exp,impressions))
# Renaming Columns
names(df2)[names(df2) == "gender_3"] <- "gender"

# Category Buyers

df2$mean_age <- round(mean(df2$age,na.rm = TRUE),digits=0)
df2$age_impute <- ifelse(is.na(df2$age),df2$mean_age,df2$age)
df2$women <- ifelse(df2$gender==2,1,0)
df2$age_impute2 <- df2$age_impute*df2$age_impute

#Coefficients
alpha_0 <- -0.8
alpha_women <- -0.85
alpha_age <- 0.05
alpha_age2 <- -0.00001
alpha_warm <- -0.025
alpha_cold <- 0.045

lp0 <- alpha_0 + 
  alpha_women*df2$women +
  alpha_age*df2$age_impute +
  alpha_age2*df2$age_impute2 +
  alpha_warm*df2$warm +
  alpha_cold*df2$cold
pr0 <- 1/(1+exp(-lp0))
set.seed(z+100)
df2$cat_buyer = rbinom(n,1,pr0)      # bernoulli response variable
summary(df2$cat_buyer)
aggregate(df2$cat_buyer, list(df2$gender), FUN=sum)
aggregate(df2$cat_buyer, list(df2$gender), FUN=length)

# Loyal Brand Buyers
pr1 <- rbinom(n,1,0.1)
summary(pr1)
df2$brand_loyal <- ifelse(df2$cat_buyer==1, pr1,0)  
summary(df2$brand_loyal)
aggregate(df2$brand_loyal, list(df2$cat_buyer), FUN=sum)
aggregate(df2$brand_loyal, list(df2$cat_buyer), FUN=length)

########## Conversion, Purchase Occasions and Average Value Sales ##########

# Conversion Probability and Binary Conversion Outcome
# https://stats.stackexchange.com/questions/46523/how-to-simulate-artificial-data-for-logistic-regression

df2$ln_imp_1 <- log(df2$imps+1)
df2$time_since_exp <- ifelse(is.na(df2$date_first_exp),0,
                             last_date_p-df2$date_first_exp)
df2$mean_tenure <- round(mean(df2$tenure,na.rm = TRUE),digits=0)
df2$tenure_impute <- ifelse(is.na(df2$tenure),df2$mean_tenure,df2$tenure)
df2$tenure_impute2 <- df2$tenure_impute*df2$tenure_impute

#Coefficients
beta_0 <- -5.4  # Initial design Value was -5.3 
beta_ln_imp_1 <- 0.015  # Initial design Value was  0.02 
beta_time_since_exp <- 0.000005  # Initial design Value was  0.00005  
beta_women <- -0.255
beta_age <- 0.01
beta_age2 <- -0.00001
beta_tenure <- 0.005
beta_tenure2 <- -0.00002
beta_warm <- -0.015
beta_cold <- 0.035
beta_cat_buyer <- 1.0
beta_brand_loyal <- 0.55

lp2 <- beta_0 + 
  beta_ln_imp_1*df2$ln_imp_1 +
  beta_time_since_exp*df2$time_since_exp +
  beta_women*df2$women +
  beta_age*df2$age_impute +
  beta_age2*df2$age_impute2 +
  beta_tenure*df2$tenure_impute +
  beta_tenure2*df2$tenure_impute2 +
  beta_warm*df2$warm +
  beta_cold*df2$cold +
  beta_cat_buyer*df2$cat_buyer +
  beta_brand_loyal*df2$brand_loyal
  
pr2 <- 1/(1+exp(-lp2))
set.seed(z+102)
df2$conversion = rbinom(n,1,pr2)      # bernoulli response variable

# The following glm model is just for review during development
# glm(conversion~ln_imp_1 + time_since_exp + women + age_impute + age_impute2 
#    + tenure_impute + tenure_impute2 + warm + cold + cat_buyer + brand_loyal ,
#    family=binomial,data = df2)

# Generate Purchase Occasions Given Purchase ... Use of Poisson Regression
# https://stats.stackexchange.com/questions/27443
# /generate-data-samples-from-poisson-regression
# https://www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/
# https://stats.oarc.ucla.edu/r/dae/zero-truncated-poisson/
# https://search.r-project.org/CRAN/refmans/actuar/html/ZeroTruncatedPoisson.html

gamma_0 <- -2.35
gamma_women <- -0.225
gamma_age <- 0.0025
gamma_age2 <- -0.000015
gamma_warm <- -0.00195
gamma_cold <- 0.00085
gamma_cat_buyer <- 0.0005
gamma_brand_loyal <- 0.025
gamma_ln_imp_1 <- 0.00045 
gamma_time_since_exp <- 0.000025

mu_poisson <- exp(gamma_0 + gamma_women*df2$women + gamma_age*df2$age_impute +
                    gamma_age2*df2$age_impute2 + gamma_warm*df2$warm +
                    gamma_cold*df2$cold + gamma_cat_buyer*df2$cat_buyer +
                    gamma_brand_loyal*df2$brand_loyal+
                    gamma_ln_imp_1*df2$ln_imp_1 + 
                    gamma_time_since_exp*df2$time_since_exp )
set.seed(z+104)
purch_occasions <- rztpois(n=n, lambda=mu_poisson)

head(mu_poisson,100)
head(purch_occasions,100)
mean(purch_occasions)
quantile(purch_occasions, probs = seq(0.05, 0.95, by = 0.05))

df2$purchase_occasions <- ifelse(df2$conversion==1, purch_occasions,0)

df3 <- df2[conversion>0]

# The following glm model is just for review during development. 
# vglm(purchase_occasions ~ women + age_impute + age_impute2 + warm  +cold + 
#       cat_buyer + brand_loyal + ln_imp_1 + time_since_exp, 
#     family = pospoisson(), data = df3)

aggregate(df2$purchase_occasions, list(df2$test_variant), FUN=mean)
aggregate(df2$purchase_occasions, list(df2$conversion), FUN=mean)

# Generate Value Sales Given Purchase ... Use of Gaussian Regression

delta_0 <- 2.6
delta_women <- -0.175
delta_age <- -0.00315
delta_age2 <- -0.000005
delta_warm <- -0.15495 
delta_cold <- 0.1955 
delta_cat_buyer <- 0.05515 
delta_brand_loyal <- 0.0055
delta_ln_imp_1 <- 0.003555 
delta_time_since_exp <- 0.0000305

set.seed(z+106)
error <- rnorm(n, mean=0, sd=0.20) 
value_sales <- round(exp(delta_0 
                   + delta_women*df2$women + delta_age*df2$age_impute 
                   + delta_age2*df2$age_impute2 + delta_warm*df2$warm 
                   + delta_cold*df2$cold + delta_cat_buyer*df2$cat_buyer 
                   + delta_brand_loyal*df2$brand_loyal+
                   + delta_ln_imp_1*df2$ln_imp_1 + 
                   + delta_time_since_exp*df2$time_since_exp 
                   + error),digits=2)
mean(value_sales)
quantile(value_sales, probs = seq(0.05, 0.95, by = 0.05))

df2$avg_value_sales <- ifelse(df2$conversion==1, value_sales,0)

df4 <- df2[conversion>0]
df4$ln_avg_value_sales <- log(df4$avg_value_sales)

# The following glm model is just for review during development
# glm(ln_avg_value_sales~ln_imp_1 + time_since_exp + women + age_impute + age_impute2 
#    + tenure_impute + tenure_impute2 + warm + cold + cat_buyer + brand_loyal ,
#    family=gaussian,data = df4)

df2$total_value_sales <- df2$avg_value_sales * df2$purchase_occasions

test_value_sales <- t.test(df2$total_value_sales[which(df2$test_variant=="Control")], 
                           df2$total_value_sale[which(df2$test_variant=="Test")], var.equal = TRUE)
test_value_sales

########### Testing ###########

# (1A) Two sample Test of Proportions - Test and Control
conv_by_group <- data.frame(aggregate(df2$conversion, list(df2$test_variant), FUN=sum))
obs_by_group <- aggregate(df2$conversion, list(df2$test_variant), FUN=length)
print(conv_by_group)
print(obs_by_group)

# A few eda statistics ... just for review
# aggregate(df2$conversion, list(df2$cat_buyer), FUN=sum)
# aggregate(df2$conversion, list(df2$cat_buyer), FUN=length)
# aggregate(df2$conversion, list(df2$brand_loyal), FUN=sum)
# aggregate(df2$conversion, list(df2$brand_loyal), FUN=length)

test_conv <- prop.test(x = conv_by_group[,2], 
                       n = obs_by_group[,2],
                       alternative="two.sided")
test_conv 

# (1B) Two sample Test of Proportions - Exposed and Unexposed

conv_by_exp_group <- data.frame(aggregate(df2$conversion, list(df2$exposed_ind), FUN=sum))
obs_by_exp_group <- aggregate(df2$conversion, list(df2$exposed_ind), FUN=length)
print(conv_by_exp_group)
print(obs_by_exp_group)

test_exp_conv <- prop.test(x = conv_by_exp_group[,2], 
                       n = obs_by_exp_group[,2],
                       alternative="two.sided")
test_exp_conv 

# (2A) Two Sample t-test for Purchase Occasions
# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

test_purch_occ <- t.test(df2$purchase_occasions[which(df2$test_variant=="Control")], 
                         df2$purchase_occasions[which(df2$test_variant=="Test")], var.equal = FALSE)
test_purch_occ

# (2B) Two Sample t-test for Purchase Occasions - Exposed and Unexposed

test_exp_purch_occ <- t.test(df2$purchase_occasions[which(df2$exposed_ind==0)], 
                         df2$purchase_occasions[which(df2$exposed_ind==1)], var.equal = FALSE)
test_exp_purch_occ

# (3A) Two Sample t-test for Value Sales
# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

test_value_sales <- t.test(df2$total_value_sales[which(df2$test_variant=="Control")], 
                         df2$total_value_sale[which(df2$test_variant=="Test")], var.equal = TRUE)
test_value_sales

test_exp_value_sales <- t.test(df2$total_value_sales[which(df2$exposed_ind==0)], 
                           df2$total_value_sale[which(df2$exposed_ind==1)], var.equal = TRUE)
test_exp_value_sales

##### EDA STATISTICS #####
# platform_df is to be distributed to the class.
platform_df <- subset(df2,select=c('user_id','gender','age','state',
                                   'test_variant','exposed_ind','imps',
                                   'date_first_exp','conversion',
                                   'purchase_occasions','total_value_sales'))

platform_df1 <- platform_df

# Age Buckets
#https://stackoverflow.com/questions/18012222/nested-ifelse-statement

platform_df1$age_cohort <- ifelse(age < 30, "18-29", 
                                  ifelse(age < 40, "30-39", 
                                         ifelse(age < 50, "40-49",
                                                ifelse(age < 60, "50-59",
                                                       ifelse(age > 59, "60+",
                                                              ifelse(is.na(age), "Missing","Missing")))))) 
# Contingency Tables and Printing LaTeX Tables
#https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
# https://rdrr.io/cran/xtable/man/xtableFtable.html

platform_df1$test_variant <- factor(platform_df1$test_variant,levels=c("Test","Control"))

ct1 <- xtabs(~age_cohort+gender+test_variant, addNA = TRUE, data=platform_df1)
ct1_export <- ftable(ct1)
xct1_export <- xtableFtable(ct1_export, method = "compact")
print.xtableFtable(xct1_export, booktabs = TRUE)

ct2 <- xtabs(~age_cohort+gender+exposed_ind, addNA = TRUE, data=platform_df1)
ct2_export <- ftable(ct2)
xct2_export <- xtableFtable(ct2_export, method = "compact")
print.xtableFtable(xct2_export, booktabs = TRUE)

# ct3 <- xtabs(~age_cohort+gender+exposed_ind+test_variant, addNA = TRUE, data=platform_df1)
# ct3_export <- ftable(ct3)

ct4 <- xtabs(~exposed_ind+test_variant, addNA = TRUE, data=platform_df1)
ct4_export <- ftable(ct4)
xct4_export <- xtableFtable(ct4_export, method = "compact")
print.xtableFtable(xct4_export, booktabs = TRUE)

# Will need to manually export results of 
# 1A) test_conv 
# 1B) test_exp_conv
# 2A) test_purch_occ
# 2B) test_exp_purch_occ
# 3A) test_value_sales
# 3B) test_exp_value_sales

# https://tex.stackexchange.com/questions/364225/export-tables-from-r-to-latex