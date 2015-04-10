#Purpose: loads the master 2009 snow project soil moisture and temperature plot level data into an R data frame, and analyzes and plots some of the the data.
#
#Author : Nicole Trahan
#Date: 03/24/2015
#
#Data located in local Snow repository; original datafile on iplant "/iplant/home/davidjpmoore/Niwot_SnowPaper/Soil_Temp_Moisture
#load data using read table into a dataframe called "SMT"
SMT = read.table("data/Temp_Moisture//Soil_temp_moisture2009.csv", na.strings=c('.'), stringsAsFactors=FALSE, head=TRUE, sep=",")

#input file asks r to assign data with characters in it as char - NOT FACTORS |stringsAsFactors=FALSE | but Treatment and DOY need to be defines as factors for ANOVA analyses - so we'll designate them as such: 

#####################################Data Manipulation and Analysis########################################
#This data needs to go from wide to long format, so using tidyr package- install.packages("tidyr") and the gather() command:
library(tidyr)
#First we need to ensure r knows that DOY and hhmm, important for anaysis, are  factors
SMT$DOY = as.factor(SMT$DOY)
SMT$hhmm = as.factor(SMT$hhmm)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values


listNames = colnames(SMT)

SMT1 <- gather(SMT, Probe_ID, Measurement, SMT$V1A:SMT$T18B)
#checking the new data frame structure
str(STM1)

# using tapply to find the mean of variables of interest by factors of interest
daily_mean <- tapply(SMT$V1:SMT$T18B,SMT[,c("DOY")],mean) 

