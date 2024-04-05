#############################################################################
# Description: Code for generating plots for Module 5 for BUSA 603.         #
# 																			                                    #	
# Last Update Date:  1/11/23       											                    #
#                                          									                #
# Last Update Date Comments:												                        #
#	A) 1/11/23 - Version 1 of code.											                      #
#																			                                      #
# Inputs:                                                                   #
# A) Parameters for generating plots.  See below.			                      #
#																			                                      #
# Outputs:																	                                #
#	A) Plots written to paths as identified below.                            #
#																			                                      #	
# Author:  Brian Weikel														                          #
#############################################################################

library(ggplot2)

# Clear working directory
# rm(list = ls())

# Output graphic and data path.
outpath <- "C:/Users/bkwei/R_projects/BUSA-603/BUSA_603_Ad_Response/Output/"

########## Surmanek Chapter 29 Media Planning ##########

# Flighting
# https://r-graphics.org/recipe-bar-graph-adjust-width
time <- c(1:12)
media <- c(150,150,150,150,0,0,0,0,150,150,150,150)
df1 <- data.frame(cbind(time,media))

plot1_a <- ggplot(df1, aes(x = time, y = media)) +
  geom_col(width = 1) +
  theme_classic() +
  ggtitle("Scheduling Techniques \n Flighting") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous("\n Planning Week", labels = as.character(time), 
                    breaks = time) +
  scale_y_continuous(name = "TRPs per Week \n") +
  theme(text = element_text(size = 16))  

ggsave("m5_0_a.png",plot=plot1_a,path=outpath)
rm(time,media,df1)

# Continuity

time <- c(1:12)
media <- c(100,100,100,100,100,100,100,100,100,100,100,100)
df1 <- data.frame(cbind(time,media))

plot1_b <- ggplot(df1, aes(x = time, y = media)) +
  geom_col(width = 1) +
  theme_classic() +
  ggtitle("Scheduling Techniques \n Continuity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous("\n Planning Week", labels = as.character(time), 
                     breaks = time) +
  scale_y_continuous(name = "TRPs per Week \n") +
  theme(text = element_text(size = 16))

ggsave("m5_0_b.png",plot=plot1_b,path=outpath)
rm(time,media,df1)

# Pulsing

time <- c(1:12)
media <- c(110,110,110,110,80,80,80,80,110,110,110,110)
df1 <- data.frame(cbind(time,media))

plot1_c<- ggplot(df1, aes(x = time, y = media)) +
  geom_col(width = 1) +
  theme_classic() +
  ggtitle("Scheduling Techniques \n Pulsing") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous("\n Planning Week", labels = as.character(time), 
                     breaks = time) +
  scale_y_continuous(name = "TRPs per Week \n") +
  theme(text = element_text(size = 16))

ggsave("m5_0_c.png",plot=plot1_c,path=outpath)
rm(time,media,df1)

########## Tellis Chapter 24 Ad Responses and Shapes ##########

basesales <- c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100)

# Current Effect
# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/
scale1 <- 1
time <- c(1:16)
sales <- c(100,150,100,100,100,150,100,100,100,150,100,100,100,150,100,100)
advertising <- c(0,50,0,0,0,50,0,0,0,50,0,0,0,50,0,0)

df1 <- data.frame(cbind(time,sales,basesales,advertising))

colors <- c("Baseline Sales" = "black","Sales" = "blue","Advertising" = "red",
            "Sales1"="blue","Sales2"="blue","Sales3"="blue","Sales4"="blue",
            "Sales5"="blue")

linetypes <- c("Baseline Sales" = "dashed","Sales" = "solid",
               "Advertising" = "twodash",
               "Sales1"="solid","Sales2"="solid", "Sales3"="solid",
               "Sales4"="solid","Sales5"="solid")

plot1 <- ggplot(df1, aes(x=time)) +
 # geom_point(size=2, shape=1,fill="black",color="black") +
 # geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
 #  geom_line(aes(y=sales),color="blue") +
  ylim(0, 150) +
  geom_line(aes(y=basesales, color="Baseline Sales",
                linetype = "Baseline Sales")) +
  geom_segment(aes(x = 2, y = 100, xend = 2, yend = 150 ,color="Sales")) +
  geom_segment(aes(x = 6, y = 100, xend = 6, yend = 150,color="Sales")) +
  geom_segment(aes(x = 10, y = 100, xend = 10, yend = 150,color="Sales")) +
  geom_segment(aes(x = 14, y = 100, xend = 14, yend = 150,color="Sales")) +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 6, y = 0, xend = 6, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 10, y = 0, xend = 10, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 14, y = 0, xend = 14, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  theme_classic() +
  ggtitle("Temporal Effects of Advertising \n Current Effect") +
  theme(legend.position="top",axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "\n Time ", color = "Legend") +
  theme(legend.position="top") + 
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(
  # Features of the first axis
    name = "Sales \n",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./scale1,name="Advertising \n"))+ 
  guides(linetype=FALSE) +
  theme(text = element_text(size = 16))

ggsave("m5_1.png",plot=plot1,path=outpath)

rm(df1,sales,advertising)

# Carryover Effects of Short Duration
# Adstock function
adstock <- function(x, rate=0){
  return(as.numeric(stats::filter(x=x, filter=rate, method="recursive")))
}

advertising <- c(0,50,0,0,0,0,0,0,0,0,0,50,0,0,0,0)
eta <- 0.5^(1/1)
ad_stock <- adstock(advertising,eta)  
sales <- basesales + ad_stock
sales1 <- sales
sales1[1] <- NA
sales1[12] <- 100
sales1[13] <- NA
sales1[14] <- NA
sales1[15] <- NA
sales1[16] <- NA
sales2 <- sales
sales2[2:11] <- NA

df1 <- data.frame(cbind(time,sales,sales1,sales2,basesales,advertising))

plot2 <- ggplot(df1, aes(x=time)) +
  # geom_point(size=2, shape=1,fill="black",color="black") +
  # geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  #  geom_line(aes(y=sales),color="blue") +
  ylim(0, 150) +
  geom_line(aes(y=basesales, color="Baseline Sales",
                linetype = "Baseline Sales")) +
  geom_line(aes(y=sales1, color="Sales")) +
  geom_line(aes(y=sales2, color="Sales")) +
  geom_segment(aes(x = 2, y = 100, xend = 2, yend = 150,color="Sales")) +
  geom_segment(aes(x = 12, y = 100, xend = 12, yend = sales2[12] ,color="Sales")) +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 12, y = 0, xend = 12, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  theme_classic() +
  ggtitle("Temporal Effects of Advertising \n Carryover Effects of Short Duration") +
  theme(legend.position="top",axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "\n Time ", color = "Legend") +
  theme(legend.position="top") + 
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(
    # Features of the first axis
    name = "Sales \n",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./scale1,name="Advertising \n")) + 
  guides(linetype=FALSE) +
  theme(text = element_text(size = 16))
  
ggsave("m5_2.png",plot=plot2,path=outpath)

rm(df1,sales,advertising,sales1,sales2)

# Carryover Effects of Long Duration

advertising <- c(0,50,0,0,0,00,0,0,0,0,0,0,0,0,0,0)
eta <- 0.5^(1/5)
ad_stock <- adstock(advertising,eta)  
sales <- basesales + ad_stock
sales[1] <- NA

df1 <- data.frame(cbind(time,sales,basesales,advertising))

plot3 <- ggplot(df1, aes(x=time)) +
  # geom_point(size=2, shape=1,fill="black",color="black") +
  # geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  #  geom_line(aes(y=sales),color="blue") +
  ylim(0, 150) +
  geom_line(aes(y=basesales, color="Baseline Sales",
                linetype = "Baseline Sales")) +
  geom_line(aes(y=sales, color="Sales")) +
  geom_segment(aes(x = 2, y = 100, xend = 2, yend = 150,color="Sales")) +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  theme_classic() +
  ggtitle("Temporal Effects of Advertising \n Carryover Effects of Long Duration") +
  theme(legend.position="top",axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "\n Time ", color = "Legend") +
  theme(legend.position="top") + 
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(
    # Features of the first axis
    name = "Sales \n",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./scale1,name="Advertising \n")) + 
  guides(linetype=FALSE) +
  theme(text = element_text(size = 16))

ggsave("m5_3.png",plot=plot3,path=outpath)

rm(df1,sales,advertising)

# Persistant Effect

advertising <- c(0,50,0,0,0,00,0,0,0,0,0,0,0,0,0,0)
eta <- 0.5^(1/3)
ad_stock <- adstock(advertising,eta)
sales <- basesales + ad_stock
sales[1] <- NA
sales[12:16] <- sales[11]

df1 <- data.frame(cbind(time,sales,basesales,advertising))

plot4 <- ggplot(df1, aes(x=time)) +
  # geom_point(size=2, shape=1,fill="black",color="black") +
  # geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  #  geom_line(aes(y=sales),color="blue") +
  ylim(0, 150) +
  geom_line(aes(y=basesales, color="Baseline Sales",
                linetype = "Baseline Sales")) +
  geom_line(aes(y=sales, color="Sales")) +
  geom_segment(aes(x = 2, y = 100, xend = 2, yend = 150,color="Sales")) +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  theme_classic() +
  ggtitle("Temporal Effects of Advertising \n Persistant Effect") +
  theme(legend.position="top",axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "\n Time ", color = "Legend") +
  theme(legend.position="top") + 
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(
    # Features of the first axis
    name = "Sales \n",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./scale1,name="Advertising \n")) + 
   guides(linetype=FALSE) +
  theme(text = element_text(size = 16))

ggsave("m5_4.png",plot=plot4,path=outpath)

rm(df1,sales,advertising)

##########################################################################
#Wear-in and Wear-Out

advertising <- c(0,50,0,0,50,0,0,50,0,0,50,0,0,50,0,0)
eta <- 0.5^(1/1)
ad_stock <- adstock(advertising,eta)  
sales <- vector( "numeric" , length(time))
sales1 <- vector( "numeric" , length(time))
sales2 <- vector( "numeric" , length(time))
sales3 <- vector( "numeric" , length(time))
sales4 <- vector( "numeric" , length(time))
sales5 <- vector( "numeric" , length(time))

sales[1:4] <- basesales[1:4] + ad_stock[1:4]
sales[5:7] <- basesales[5:5] + 1.1*ad_stock[5:7]
sales[8:10] <- basesales[8:10] + 1.2*ad_stock[8:10]
sales[11:13] <- basesales[11:13] + 0.9*ad_stock[11:13]
sales[14:16] <- basesales[14:16] + 0.5*ad_stock[14:16]

sales1[1] <- NA
sales1[2:4] <- sales[2:4]
sales1[5] <- basesales[5] + 1*(ad_stock[5] - advertising[5])
sales1[6:16] <- NA

sales2[1:4] <- NA
sales2[5:7] <- sales[5:7]
sales2[8] <- basesales[8] + 1.1*(ad_stock[8] - advertising[8])
sales2[9:16] <- NA

sales3[1:8] <- NA
sales3[8:10] <- sales[8:10]
sales3[11] <- basesales[11] + 1.2*(ad_stock[11] - advertising[11])
sales3[12:16] <- NA

sales4[1:10] <- NA
sales4[11:13] <- sales[11:13]
sales4[14] <- basesales[14] + 0.5*(ad_stock[14] - advertising[14])
sales4[15:16] <- NA

sales5[1:13] <- NA
sales5[14:16] <- sales[14:16]

df1 <- data.frame(cbind(time,sales,sales1,sales2,
                        sales3,sales4,sales5,basesales,advertising))

plot5 <- ggplot(df1, aes(x=time)) +
  # geom_point(size=2, shape=1,fill="black",color="black") +
  # geom_smooth(method=lm,se=FALSE, fullrange=TRUE) +
  #  geom_line(aes(y=sales),color="blue") +
  ylim(0, 150) +
  geom_line(aes(y=basesales, color="Baseline Sales",
                linetype = "Baseline Sales")) +
  geom_line(aes(y=sales1, color="Sales")) +
  geom_line(aes(y=sales2, color="Sales")) +
  geom_line(aes(y=sales3, color="Sales")) +
  geom_line(aes(y=sales4, color="Sales")) +
  geom_line(aes(y=sales5, color="Sales")) +
  geom_segment(aes(x = 2, y = 100, xend = 2, yend = 150,
                   color="Sales")) +
  geom_segment(aes(x = 5, y = sales1[5], xend = 5, yend = sales2[5],
                   color="Sales")) +
  geom_segment(aes(x = 8, y = sales2[8], xend = 8, yend = sales3[8],
                   color="Sales")) +
  geom_segment(aes(x = 11, y = sales3[11], xend = 11, yend = sales4[11],
                   color="Sales")) +
  geom_segment(aes(x = 14, y = sales4[14], xend = 14, yend = sales5[14],
                   color="Sales")) +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 8, y = 0, xend = 8, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 11, y = 0, xend = 11, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  geom_segment(aes(x = 14, y = 0, xend = 14, yend = 50 ,color="Advertising",
                   linetype = "Advertising")) +
  theme_classic() +
  ggtitle("Wear-In and Wear-Out in Advertising Effectiveness") +
  theme(legend.position="top",axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "\n Time ", color = "Legend") +
  theme(legend.position="top") + 
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(
    # Features of the first axis
    name = "Sales \n",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~ ./scale1,name="Advertising \n")) + 
  guides(linetype=FALSE) +
  theme(text = element_text(size = 16)) +
  annotate("text", x=6, y=175, label= "Wear-In") + 
  annotate("text", x=12, y=165, label= "Wear-Out")

ggsave("m5_5.png",plot=plot5,path=outpath)

rm(df1,sales,advertising,sales1,sales2,sales3,sales4.sales5)

########## Response Curves ##########
# https://stackoverflow.com/questions/26091323/how-to-plot-a-function-curve-in-r
eq0 <- function(x,intercept=0,slope=0.2){intercept+x*slope} # CRTS
eq1 <- function(x,lambda=2.5,k=2.5){1-exp(-(x/lambda)^k)} # S-Shapes
eq2 <- function(x,lambda=2,k=0.75,scale=1.2){scale*(1-exp(-(x/lambda)^k))} # DRTS

plot_a <- ggplot(data.frame(x=c(0, 4)), aes(x=x)) + 
  stat_function(fun=eq0,aes(colour = "Linear")) +
  stat_function(fun=eq1,aes(colour = "S-shaped")) +
  stat_function(fun=eq2,aes(colour = "Concave")) +
  theme_classic() +
  ggtitle("Linear and Nonlinear Response to Advertising") +
  theme(legend.position="top",axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "\n Advertising ", color = "Legend") +
  theme(legend.position="top") + 
  scale_color_manual(values = c("black","blue","red")) +
  scale_linetype_manual(values = linetypes) +
  scale_y_continuous(
    # Features of the first axis
    name = "Sales \n") + 
  guides(linetype=FALSE) +
  theme(text = element_text(size = 16)) 
  
ggsave("m5_a.png",plot=plot_a,path=outpath)  
  
