#############################################################################
# Description: Code for completing an illustrative moderated regression     #
# example for BUSA 603.                                                     #
# 																			                                    #	
# Last Update Date:  1/20/23       										                      #
#                                          									                #
# Last Update Date Comments:												                        #
#	A) 11/20/23 - Version 1 of code.											                    #
#																			                                      #
# Inputs:                                                                   #
# A) Parameters for generating synthetic data.                              #
#																			                                      #
# Outputs:																	                                #
#	A)	Moderated regression examples			                                    #
#																			                                      #	
# References:                                                               #
# A) Spiller et. al. Spotlights, Floodlights and the Magic Number Zero:     #
# Simple Effects Tests in Moderated Regression                              #
#                                                                           #    
# Author:  Brian Weikel														                          #
#############################################################################

# Clear working directory
# rm(list = ls())

library(dplyr)
library(MASS)

# Output graphic and data path.
outpath <- "C:/Users/bkwei/R_projects/BUSA-603/BUSA_603_Moderated_Regression/Output/"

# Print options
options(max.print=999999)

############### Parameter Inputs for Regression 1 ###############
# Seed value
set.seed(80872)
# Number of 4-week time periods.
timep <- 26
# CPG brand Total Distribution Points (TDPs) standard deviation
tdp_sd <- 12
# Coefficients for Regression 1
alpha_1 <- 15000 # Intercept
beta_0 <- 595 # TDP
beta_1 <- 30 # GRPs
delta_1 <- 35000 # Dummy Variable, say for coupon drop
gamma_1 <- 150 # Interaction for coupons and grps
# Coefficients for Regression 2
constant <-5 
alpha1 <- 1500/constant # Intercept
beta0 <- 595/constant # TDP
beta1 <- 150/constant # Spotify ... 
beta2 <- 305/constant # National Radio                      
gamma1 <- 1.5/constant # Interaction of Spotify and National Radio

# Data Preparation for Regressoin 1

# 20 largest Supermarket Chains in U.S.
# https://progressivegrocer.com/pg-100-ranking-top-food-retailers-north-america
market <- c("The Kroger Co.","Albertsons Cos.",
"Ahold Delhaize USA","Publix Super Markets",
"H-E-B","C&S Wholesale Grocers",
"Aldi U.S.","Wakefern Food Corp.",
"Trader Joe’s Co.","Hy-Vee Food Stores Inc.",
"Wegmans Food Markets Inc.","Associated Wholesale Grocers",
"Giant Eagle Inc.","Southeastern Grocers LLC",
"SpartanNash Co.","Northeast Grocery Inc.",
"WinCo Foods Inc.","Raley’s Supermarkets",
"Demoulas Super Markets Inc.","The Save Mart Cos.")
# Number of Markets
nm <- length(market)
# Market ACV in billions of $
acv <- as.numeric(c(137.888,71.887,53.699,47.997,
34.000,33.022,18.200,17.800,
14.900,12.300,11.200,10.812,
10.600,9.600,8.900,8.120,
8.100,6.750,6.200,5.600))

df1 <- data.frame(market, acv)
df2 <- df1[rep(seq_len(nrow(df1)), each = timep), ]

time1 <- seq(timep)
time <- rep(time1, times = nm)
df2$time <- time

df2$tdpscale <- 1

#Warnings for the following cause no issues
df2$tdpscale[df2$market == "H-E-B" & df2$time > 4 ] <- (1+df2$time/208)
df2$tdpscale[df2$market == "Northeast Grocery Inc."& df2$time > 15] <- (1+df2$time/156) 
df2$tdpscale[df2$market == "Trader Joe’s Co." & df2$time > 13 ] <- (1-df2$time/52)
df2$tdpscale[df2$market == "Aldi U.S." & df2$time > 7 ] <- (1-df2$time/78)
df2$tdpscale[df2$market == "WinCo Foods Inc." & df2$time > 16 ] <- (1-df2$time/130)
df2$tdpscale[df2$market == "The Save Mart Cos." & df2$time > 12 ] <- (1-df2$time/182)

tdpb_noise <- rnorm(nm*timep,0,tdp_sd)
df2$brand_tdp <- df2$tdpscale*(350 + df2$acv/8) + tdpb_noise

# Coupon
df2$z <- 0
df2$z[df2$market == "Giant Eagle Inc." & df2$time == 4] <- 1
df2$z[df2$market == "Giant Eagle Inc." & df2$time == 17] <- 1
df2$z[df2$market == "C&S Wholesale Grocers" & df2$time == 5] <- 1
df2$z[df2$market == "C&S Wholesale Grocers" & df2$time == 18] <- 1
df2$z[df2$market == "H-E-B" & df2$time == 1] <- 1
df2$z[df2$market == "H-E-B" & df2$time == 14] <- 1
df2$z[df2$market == "Raley’s Supermarkets" & df2$time == 12] <- 1
df2$z[df2$market == "Raley’s Supermarkets" & df2$time == 25] <- 1

df2$z[df2$market == "The Kroger Co." & df2$time == 2] <- 1
df2$z[df2$market == "The Kroger Co." & df2$time == 15] <- 1
df2$z[df2$market == "Albertsons Cos."  & df2$time == 3] <- 1
df2$z[df2$market == "Albertsons Cos."  & df2$time == 16] <- 1
df2$z[df2$market == "Ahold Delhaize USA"  & df2$time == 6] <- 1
df2$z[df2$market == "Ahold Delhaize USA"  & df2$time == 19] <- 1
df2$z[df2$market == "Publix Super Markets"  & df2$time == 9] <- 1
df2$z[df2$market == "Publix Super Markets"  & df2$time == 22] <- 1

df2$z[df2$market == "Wakefern Food Corp."  & df2$time == 7] <- 1
df2$z[df2$market == "Wakefern Food Corp."  & df2$time == 20] <- 1
df2$z[df2$market == "Hy-Vee Food Stores Inc."  & df2$time == 8] <- 1
df2$z[df2$market == "Hy-Vee Food Stores Inc."  & df2$time == 21] <- 1
df2$z[df2$market == "Wegmans Food Markets Inc."  & df2$time == 10] <- 1
df2$z[df2$market == "Wegmans Food Markets Inc."  & df2$time == 23] <- 1
df2$z[df2$market == "Associated Wholesale Grocers"  & df2$time == 11] <- 1 
df2$z[df2$market == "Associated Wholesale Grocers"  & df2$time == 24] <- 1

set.seed(80872)
x <- rnorm(timep,500,60)
x1 <- data.frame(time1,x)
names(x1)[names(x1) == "time1"] <- "time"

df3 <- merge(df2,x1,by="time")
df3$xz <- df3$z * df3$x

# Regression 1

set.seed(614)
noise <- rnorm(nm*T,0,15000) # 8000

df3$brand_sales <- (df3$acv/timep)*(alpha_1 + beta_0*df3$brand_tdp 
                                    + beta_1 *df3$x
                                    + delta_1*df3$z
                                    + gamma_1*df3$xz
                                    + noise)
df3$equ <- df3$brand_sales/1.25
df3$velocity <- df3$equ/df3$acv 

reg1 <- lm(velocity ~ x + z + xz,data=df3)
summary(reg1)

reg1s <- summary(reg1)

reg1 <- paste(outpath,"coup_tz_reg.txt",sep="")
sink(reg1)
cat("Moderator Regression - TV GRPs and Coupons")
reg1s
sink()

############### Parameter Inputs for Regression 2 ###############

df4 <- subset(df3,select=c(time,market, acv, brand_tdp,equ))

n_equ <- df4 %>%
  group_by(time) %>% 
  summarise(n_equ = sum(equ))

set.seed(603)
r1 <- rnorm(timep,4.3,0.2)

n_equ$nequ <- n_equ$n_equ*r1

df_1 <- subset(n_equ,select=c(time,nequ)) 

n_acv <- df4 %>%
  group_by(time) %>% 
  summarise(n_acv = sum(acv))

set.seed(604)
r2 <- rnorm(timep,4.3,0.025)

n_acv$nacv <- n_acv$n_acv*r2

df_2 <- subset(n_acv,select=c(time,nacv)) 

n_tdp <- df4 %>%
  group_by(time) %>% 
  summarise(n_tdp = weighted.mean(brand_tdp,acv))

set.seed(605)
r3 <- rnorm(timep,1,0.05)

n_tdp$ntdp <- n_tdp$n_tdp*r3

df_3 <- subset(n_tdp,select=c(time,ntdp)) 

df_4 <- merge(df_1,df_2,by="time")
df_5 <- merge(df_4,df_3,by="time")
df_6 <- data.frame(df_5)

# Generating Correlated Spotify, Apple Music, Amazon Music and Pandora, and
# National Network Radio
rho <- 0.65
mu1 <- 70; s1 <- 70 # Digital Radio
mu2 <- 60; s2<- 60 # National Network Radio
mu <- c(mu1,mu2)
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),
                2)
set.seed(606)
bvn1 <- mvrnorm(timep, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn1) <- c("x_1","x_2")
bvn2 <- data.frame(bvn1,time1)
names(bvn2)[names(bvn2) == "time1"] <- "time"

bvn2$x1 <- ifelse(bvn2$x_1<20,0,bvn2$x_1)
bvn2$x2 <- ifelse(bvn2$x_2<20,0,bvn2$x_2)

bvn_3 <- subset(bvn2,select=c(time,x1,x2)) 

df_7 <- merge(df_6,bvn_3,by="time")

df_7$x1x2 <- df_7$x1*df_7$x2

# Coefficients for Regression 2
constant <-5 
alpha1 <- 1500/constant # Intercept
beta0 <- 595/constant # TDP
beta1 <- 150/constant # Spotify ... 
beta2 <- 305/constant # National Radio                      
gamma1 <- 1.5/constant # Interaction of Spotify and National Radio

set.seed(611) 
noise0 <- rnorm(timep,0,10750/constant) 

df_7$brand_sales <- (df_7$nacv)*(alpha1 + beta0*df_7$ntdp 
                                    + beta1*df_7$x1
                                    + beta2*df_7$x2
                                    + gamma1*df_7$x1x2
                                    + noise0)
df_7$natequ <- df_7$brand_sales/1.25

df_7$natequ/df_7$nequ

df_7$velocity <- df_7$natequ/df_7$nacv 

reg2 <- lm(velocity ~ x1 + x2 + x1x2,data=df_7)
reg2s <- summary(reg2)

reg2_ <- paste(outpath,"spotify_natradio_reg.txt",sep="")
sink(reg2_)
cat("Moderator Regression - Spotify, Amazon Music, Apple Music,")
cat("\n")
cat("and Pandora and National Radio Adult 18-54 Impressions")
reg2s
sink()

# Coefficients for Regression 2a - Mean Centering digital radio, x1

center_scale <- function(x) {scale(x, scale = FALSE)}

df_7$x1_meanc <- center_scale(df_7$x1)
df_7$x1_meanc_x2 <- df_7$x1_meanc * df_7$x2

reg2a <- lm(velocity ~ x1_meanc + x2 + x1_meanc_x2,data=df_7)
reg2sa <- summary(reg2a)

reg2a_ <- paste(outpath,"spotifymc_natradio_reg.txt",sep="")
sink(reg2a_)
cat("Moderator Regression - Spotify, Amazon Music, Apple Music,")
cat("\n")
cat("and Pandora and National Radio Adult 18-54 Impressions")
cat("\n")
cat("Spotlighting at the Moderator's Mean")
reg2sa
sink()

# Coefficients for Regression 2b - High Level Spotlighting

sd_x1 <- sd(df_7$x1_meanc)
df_7$sd_x1_vec <- rep(sd_x1, rep.int(timep,1))

df_7$x1_sdcn1 <- df_7$x1_meanc - df_7$sd_x1_vec
df_7$x1_sdcn1_x2 <- df_7$x1_sdcn1 * df_7$x2

reg2b <- lm(velocity ~ x1_sdcn1 + x2 + x1_sdcn1_x2,data=df_7)
reg2sb <- summary(reg2b)

reg2b_ <- paste(outpath,"spotifynegsdc_natradio_reg.txt",sep="")
sink(reg2b_)
cat("Moderator Regression - Spotify, Amazon Music, Apple Music,")
cat("\n")
cat("and Pandora and National Radio Adult 18-54 Impressions")
cat("\n")
cat("Spotlight is at 1 SD ABOVE the Mean-Centered Moderator")
reg2sb
sink()

# Coefficients for Regression 2c - Low Level Spotlighting

df_7$x1_sdcp1 <- df_7$x1_meanc + df_7$sd_x1_vec
df_7$x1_sdcp1_x2 <- df_7$x1_sdcp1 * df_7$x2

reg2c <- lm(velocity ~ x1_sdcp1 + x2 + x1_sdcp1_x2,data=df_7)
reg2sc <- summary(reg2c)

reg2c_ <- paste(outpath,"spotifypossdc_natradio_reg.txt",sep="")
sink(reg2c_)
cat("Moderator Regression - Spotify, Amazon Music, Apple Music,")
cat("\n")
cat("and Pandora and National Radio Adult 18-54 Impressions")
cat("\n")
cat("Spotlight is at 1 SD BELOW the Mean-Centered Moderator")
reg2sc
sink()


