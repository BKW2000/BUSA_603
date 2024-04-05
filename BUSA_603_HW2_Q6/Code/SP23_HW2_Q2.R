#################################################################################
# Description: Code that addresses question 6 of homework 2 of BUSA 603 for     #
# Spring 2023, and possibly for future terms.                                   #
#																				#
# Source Data:  SP23_HW2_Q2.xlsx, which was souced from the Word document of 	#
# of homework 2.                                                                #
#																				#																		                                          #
# Output:                                                                       #
# (1) Excel file with return of SQL query.                                      #
# Last update date:  03/02/23                                                   #
# Author:  Brian Weikel                                                         #
# For SQL in R, see: 															#
# https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html		#
#################################################################################

# rm(list=ls())

library(readxl)
library(data.table)
library(sqldf)
library(writexl)

options(max.print=10000)
getwd()
inpath1 <- "C:/Users/bkwei/R_projects/BUSA-603/Excel_files/"
outpath1 <- "C:/Users/bkwei/R_projects/BUSA-603/BUSA_603_HW2_Q6/Output/"
infile0 <- paste(inpath1,"SP23_HW2_Q2.xlsx",sep="")
outfile0 <- paste(outpath1,"P23_HW2_Q2_Query.xlsx",sep="")

orders <- data.frame(read_excel(infile0,sheet="orders",range="A1:E13",col_names=TRUE))
customer <- data.frame(read_excel(infile0,sheet="customer",range="A1:E9",col_names=TRUE)) 

# Query of interest 

df_0 <- sqldf('SELECT orders.ord_no, orders.purch_amt, orders.customer_id,
	customer.city, customer.cust_name 
	FROM orders LEFT JOIN customer 
	ON orders.customer_id  = customer.customer_id
	WHERE orders.purch_amt > 500 AND orders.purch_amt < 2000')

write_xlsx(df_0,outfile0)