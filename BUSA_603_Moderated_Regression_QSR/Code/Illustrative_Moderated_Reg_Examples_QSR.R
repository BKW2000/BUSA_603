#############################################################################
# Description: Code for completing an illustrative moderated regression     #
# example for BUSA 603. This example is for a quick service restaurant.     #
# Specifically, we are examining if a billboard within 0.25 miles of a      #
# restaurant moderates the effect of TV advertising.  While there are       #
# about 625 McDonalds in Ohio, we will only be examining stores in the      #
# Columbus (2.3M people 12+), Cincinnati (2.1M), Dayton (1.1M),             # 
# Cleveland-Akron (3.3M) and Toledo DMAs (0.9M people).  There are about    #
# 10M people in Ohio aged 12+.  Thus the 5 DMAs have 9.7M people aged 12+.  #
# Ergo our store count is 625*0.97 = 606.                                   #
# 																			                                    #	
# Last Update Date:  02/03/23      										                      #
#                                          									                #
# Last Update Date Comments:												                        #
#	A) 02/03/24 - Version 1 of code.											                    #
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
# B) https://scholar.harvard.edu/sigou-interdisciplinary-blog/              #
# regression-moderator-101#:~:text=Regression%20with%20a%20moderator%20is,  #
# can%20be%20predicted%20by%20W.                                            #
#                                                                           #    
# Author:  Brian Weikel														                          #
#############################################################################

# Clear working directory
# rm(list = ls())

library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(MASS)
library(scales)
library(stringi)
library(stringr)
library(tidyr)
library(VGAM)

# Output graphic and data path.
datapath <- "C:/Users/bkwei/R_projects/BUSA-603/BUSA_603_Moderated_Regression_QSR/Data/"
outfile <- paste(datapath,"Module_5_Supplement.csv",sep = "")

# Output graphic and data path.
outpath <- "C:/Users/bkwei/R_projects/BUSA-603/BUSA_603_Moderated_Regression_QSR/Output/"

# Print options
options(max.print=999999)

# Setting Seed
seedval <- 61411

##### Generating Synthetic Data                                         #####

# Number of stores without remodels and have drive-throughs and lobbies 
n_stores <- 555
n_stores_cmh <- 123
n_stores_clv <- 189
n_stores_cin <- 120
n_stores_tol <- 56
n_stores_day <- n_stores - n_stores_cmh - n_stores_clv - n_stores_cin -
  n_stores_tol
markets <- c("CMH","CLE","CIN","DAY","TOL")

# Average Transactions per Week per Store:  3,460
# Average Order Size:  $11.00
# Average Annual Dollar Sales per Store:  $1.98M
# Average Store Selling Square Feet:  4500 ... A restaurant floor plan is a map 
# of your restaurant's physical space, encompassing all of the elements of your 
# establishment. This includes the dining area, waiting area, kitchen, 
# prep areas, storage, and bathroom, and how they fit into your space together.

ave_floorspace <- 3.825
sd_floorspace <- 0.115

# Number of weeks
n_time <- 104

# Percent of stores with billboards
n_stores_bill <- 0.25

# Generating Store IDs

store_id <- seq(n_stores)

# Store IDs and Markets

set.seed(seedval+1)
store_id <-  stringi::stri_rand_strings(n_stores, 8, pattern = "[A-Za-z0-9]")
ranuni <- runif(n_stores, min = 0, max = 1)
market <- ifelse(ranuni < (n_stores_cmh/n_stores),"CMH",
  ifelse(ranuni < ((n_stores_cmh + n_stores_clv)/n_stores),"CLE",
    ifelse(ranuni < ((n_stores_cmh + n_stores_clv + n_stores_cin)/n_stores),
      "CIN",
        ifelse(ranuni < ((n_stores_cmh + n_stores_clv + n_stores_cin +
          n_stores_tol)/n_stores),"TOL","DAY"))))

df1 <- data.frame(cbind(store_id,market))

set.seed(seedval+2)
df1$floor_space <- as.numeric(round(100*rlnorm(
  n_stores, 
  meanlog = ave_floorspace, 
  sdlog = sd_floorspace),-1))
# summary(floor_space)

# Randomly generating coefficients for seasonality component creation 
# and (linear) trend
set.seed(seedval+3)
df1$b_sin1 <- round(rnorm(n_stores,-0.025,0.0025),6)
set.seed(seedval+4)
df1$b_cos1 <- round(rnorm(n_stores,0.001,0.0001),6)
set.seed(seedval+5)
df1$b_sin2 <- round(rnorm(n_stores,-0.00015,0.000015),6)
set.seed(seedval+6)
df1$b_cos2 <- round(rnorm(n_stores,0.0105,0.00105),6)
set.seed(seedval+7)
df1$b_sin3 <- round(rnorm(n_stores,0.003,0.0003),6)
set.seed(seedval+8)
df1$b_cos3 <- round(rnorm(n_stores,0.002,0.0002),6)
set.seed(seedval+9)
df1$b_trend <- round(rnorm(n_stores,0.00015,0.00015),6)

rm(market,ave_floorspace,sd_floorspace,ranuni)

# Time Dimensions
#  Week Starting Sunday Vector, starting 2022-01-02
week_starting <- as.Date(c("2022-01-02"))
for(i in seq(from = 0, by = 7, length.out = n_time)) {
  week_starting[i] = week_starting + i}
week_starting = week_starting[!is.na(week_starting)]
# week_ending

weeknumb <- lubridate::week(week_starting)
df2 <- data.frame(cbind(week_starting,weeknumb))
df2$week_starting <- as.Date(df2$week_starting)
df2$time_id <- as.numeric(row.names(df2))

# Trigonometric functions to create first three harmonics

df2$sin1 <- sin(2*pi*df2$time_id/52)
df2$cos1 <- cos(2*pi*df2$time_id/52)
df2$sin2 <- sin(2*2*pi*df2$time_id/52)
df2$cos2 <- cos(2*2*pi*df2$time_id/52)
df2$sin3 <- sin(3*2*pi*df2$time_id/52)
df2$cos3 <- cos(3*2*pi*df2$time_id/52)

df3 <- tidyr::crossing(df1,df2) 

rm(weeknumb,i)

# Simulate National People 18-54 TRPs using AR1 process.

set.seed(seedval+10)
trps <- 20*arima.sim(list(order = c(2,0,1), ar = c(0.7,0.2), ma=0.6), 
                     n = n_time) + 80
ts.plot(trps)

trps_p <- data.frame(trps)
trps_p$time_id <-as.numeric(row.names(trps_p))
trps_p$trps <- round(ifelse(
  trps_p$time_id < 16,0,
  ifelse(50 < trps_p$time_id & trps_p$time_id < 68,0,
         ifelse(trps_p$trps < 50,0,trps_p$trps))),digits=1)

df4 <- tidyr::crossing(markets,trps_p) 

df4 <- df4 %>% rename(market = markets)

set.seed(seedval+11)
df4$noise_trps <- round(rnorm(dim(df4)[1],0,7.5),1)
df4$trps_1854 <- ifelse(df4$trps > 0 & df4$trps + df4$noise_trps > 0,
                        df4$trps+df4$noise_trps,0)

df4 <- subset(df4,select = -c(trps,noise_trps))

df5 <- merge(x = df3, y = df4, by = c('market','time_id'), all.x = TRUE)

rm(trps_p,trps)

# Randomly selecting stores where billboards are places.  Then randomly 
# selecting number of weeks from 4, 8, 12, 16, 20, 24, 28, followed by randomly
# selecting weeks. 

df5 <- df5 %>% arrange(store_id,time_id)
bill1 <- subset(df5,select=c(store_id,time_id))

bill2 <- data.frame(store_id)

set.seed(seedval+11)
bill2$ranuni <- runif(n_stores, min = 0, max = 1)
set.seed(seedval+12)
bill2$num_weeks_bill <- 4*(rbetabinom.ab(n = n_stores, size = 6, 
  shape1 = 4, shape2 = 4) + 1)
bill2$first_time_id_bill <- rbetabinom.ab(n = n_stores, size = n_time, 
  shape1 = 4, shape2 = 10)

bill3 <- merge(x = bill1, y = bill2, by = c('store_id'), all.x = TRUE)

bill3$bill_brd <- ifelse(bill3$ranuni < n_stores_bill & 
  bill3$time_id >= bill3$first_time_id_bill & 
  bill3$time_id <= bill3$first_time_id_bill + bill3$num_weeks_bill - 1,1,0)

bill3 <- subset(bill3, select = -c(ranuni,num_weeks_bill,first_time_id_bill))

df6 <- merge(x = df5, y = bill3, by = c('store_id','time_id'), all.x = TRUE)
df6 <- df6 %>% arrange(store_id,time_id)

df6 <- df6 %>% rename(x=trps_1854,z=bill_brd)

# Generating transaction 

# Coefficients for Regression 
constant <- 1 
alpha1 <- 0.725/constant # Intercept
beta1 <- 0.000020/constant # TRPs (i.e., x) # 0.00035, 0.00005, 0.000025
beta2 <- 0.0125/constant # Billboard (i.e., z) # 0.025,  0.0315,  0.0140
gamma1 <- 0.00000/constant # Interaction 0.00005

set.seed(seedval+13)
df6$noise <- rnorm(n_stores*n_time,0,0.075) # 0.075, 0.175, 0.115, 0.085
df6$xz <- df6$x*df6$z

df6$trans <- (df6$floor_space)*(alpha1 + beta1 * df6$x
                                    + beta2 * df6$z
                                    + gamma1 * df6$xz
                                    + df6$b_trend * df6$time_id
                                    + df6$b_sin1 * df6$sin1
                                    + df6$b_cos1 * df6$cos1
                                    + df6$b_sin2 * df6$sin2
                                    + df6$b_cos2 * df6$cos2
                                    + df6$b_sin3 * df6$sin3
                                    + df6$b_cos3 * df6$cos3
                                    + df6$noise)


# Plot weekly aggregate transactions

sum1 <- df6 %>% group_by(week_starting) %>% summarize(agg = sum(trans))

ggplot(sum1, aes(x = week_starting, y =  agg)) +
  theme_classic() + 
  theme(plot.title = element_text(lineheight = .8,size = 14,hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1)) + 
  geom_line() + 
  ggtitle("Weekly Total Transactions \n")  +
  xlab("\n Week Starting Sunday") + 
  ylab("Total Transactions \n") +
  scale_x_date(breaks = seq(min(sum1$week_starting),max(sum1$week_starting),by="91 days"),
               labels = date_format(format = "%m-%d-%y")) +
  scale_y_continuous(label=comma) 

ggsave("output_1.png",path = outpath)

# Plot harmonics 

harmonics <- df2
harmonics$firstharm <- harmonics$sin1 + harmonics$cos1
harmonics$secharm <- harmonics$sin2 + harmonics$cos2
harmonics$thirdharm <- harmonics$sin3 + harmonics$cos3
harmonics$allharm <- harmonics$firstharm + harmonics$secharm + 
  harmonics$thirdharm
harm1 <- subset(harmonics, select=-c(weeknumb,time_id,sin1,cos1,
  sin2,cos2,sin3,cos3))

harm2 <- melt(setDT(harm1), id.vars = c("week_starting"), 
 variable.name = "harm")
  
plot1 <- ggplot(harm2, aes(x = week_starting, y =  value)) +
  theme_classic() + 
  theme(plot.title = element_text(lineheight = .8,size = 14,hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 1)) + 
  geom_line()  +
  ggtitle("First Three Harmonics and Their Linear Combination \n")  +
  xlab("\n Week Starting Sunday") + 
  ylab("Value \n") +
  scale_x_date(breaks = seq(min(sum1$week_starting),max(sum1$week_starting),by="91 days"),
               labels = date_format(format = "%m-%d-%y"))

axislabels <- list("1st Harm.", "2nd Harm.", "3rd Harm.",
  "Lin. Comb.") 

axis_labeller <- function(variable,value){return(axislabels[value])}

plot1 + facet_grid(harm ~ ., labeller = axis_labeller)
  
ggsave("output_2.png",path = outpath) 

# https://www.rdocumentation.org/packages/TSA/versions/1.3/topics/harmonic

# Estimating the model without time trend and trigometric functions 
df6$y <- df6$trans/df6$floor_space 

reg1 <- lm(y ~ x + z + xz ,data=df6)
reg1s <- summary(reg1)
reg1s

out_a <- paste(outpath,"output_3.txt",sep = "")
sink(out_a)
cat(str_wrap("Moderator Regression - National and Local TV Adult 18-54
             TRPs and Billboards"),"\n")
print(reg1s)
sink()

# Model with time trend and trigonometric functions  

reg2 <- lm(y ~ x + z + xz + time_id + sin1 + sin2 + sin3 + 
             cos1 + cos2 + cos3,data=df6)
reg2s <- summary(reg2)
reg2s

out_b <- paste(outpath,"output_4.txt",sep = "")
sink(out_b)
cat(str_wrap("Moderator Regression - National and Local TV Adult 18-54
             TRPs and Billboards Controling for Seasonality and Trend"),"\n")
print(reg2s)
sink()

# Outputting the data to a csv file

df7 <- subset(df6,select=c(store_id,time_id,market,floor_space,
  week_starting,weeknumb,trans,x,z,y))

df8 <- df7[,c(1,3,4,5,2,6,7,8,9,10)]

write.csv(df8, file=outfile)

rm(list=ls())

