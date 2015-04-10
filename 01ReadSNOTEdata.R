#Purpose: To load the SNOTEL DATA INTO A DATAFRAME
#Author : Dave Moore
#Date: 03/13/2015
#
#
#load data using read table into a dataframe called "SNOWnwt"
SNOWnwt = read.table("data/SNOTELNiwot Tue Mar 10 2015.txt", head=TRUE, sep=",")
#SNOTELNiwot Tue Mar 10 2015.txt was downloaded from SNOTEL site see README file

#asking R to read the date as a date;
rDate=as.Date(SNOWnwt$Date)

#create variable "Year" from the date
Year=as.numeric(format(rDate, "%Y"))

SNOWnwt$SWE_mm=SNOWnwt$SWE_in*2.54

SNOWnwt99_2013 = subset(SNOWnwt,Year>1998 & Year<2014)
#save(SNOWnwt99_2013,file="data/SNOWnwt99_2013.rda")

#calculating annual snow totals
snowpeaks_ann=tapply(SNOWnwt$SWE_in*2.54,Year,max)
snowsum_ann=tapply(SNOWnwt$SWE_in*2.54,Year,sum)



#defining the wateryear and waterdate
waterdate=rDate+90
wateryear=as.numeric(format(waterdate, "%Y"))


Yind=(min(wateryear):max(wateryear))
barplot(tapply(SNOWnwt$SWE_in,Year,sum))

#caculate wateryear snow totals
snowpeaks=tapply(SNOWnwt$SWE_in*2.54,wateryear,max)
snowsum=tapply(SNOWnwt$SWE_in*2.54,wateryear,sum)
prec_acc=tapply(SNOWnwt$PrecAcc_in*2.54,wateryear,sum)


#simple plots
barplot(tapply(SNOWnwt$SWE_in,wateryear,max))
plot(snowsum,snowpeaks)

#quick plot showing how cummulative SWE for the year and water year differ
plot(snowsum_ann,prec_acc)


#TO DO: I've made a mess and made a lot of variables in the global environment  merge new data vectors to SNOWnwt and store for later use

