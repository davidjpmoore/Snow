
####Example of a loop from Mike's course
# NT = 100        ## number of time steps
# N0 = 1              ## initial population size
# r = 0.2             ## population growth rate
# K = 10              ## carrying capacity
# N = rep(N0,NT)
# for(t in 2:NT){
#   N[t] = N[t-1] + r*N[t-1]*(1-N[t-1]/K)    ## discrete logistic growth
# }
# plot(N)

#setwd("D:/Dropbox/rProjectsShare/Snow/data/FluxData/") 
#define object SWE as the SWE in Inches from the SNOWnwt dataframe
SWE = SNOWnwt$SWE_in
PrecAcc_in=SNOWnwt$PrecAcc_in
#define Yind (year index) from the first to the last Water Year
Yind=(min(wateryear):max(wateryear))
#limits

NT=max(Yind)
#define starting year
syear=min(Yind)
#allocate size of each of the quantities to be calculated
snowpeaks = rep(1,length(Yind))
PeakSWEday = rep(1,length(Yind))
ZeroSWEday = rep(1,length(Yind))
PrecAcc_in_total = rep(1,length(Yind))

##
## Loop to calculate similar quantities on all years

for(t in 1:length(Yind)){
  Yt = t-1+syear #Yt used to just convert from timestep of loop (t) to a year
  snowpeaks[t]=max(SWE[wateryear==(Yt)]) #within each wateryear calculate max SWE
  PrecAcc_in_total[t]=max(PrecAcc_in[wateryear==(Yt)])
  tempdate =rDate[wateryear==Yt] #assign a temporary date (this wateryear)
  tempSWE =SWE[wateryear==Yt] #Assign a temporary SWE for this wateryear
  PeakSWEday[t]=max((tempdate[tempSWE==(snowpeaks[t])])) #calculate the LATEST date of max SWE within this year
  postPeakdate = tempdate[tempdate>PeakSWEday[t]] #limit to dates AFTER peak SWE
  postPeakSWE = tempSWE[tempdate>PeakSWEday[t]] #limit to dates AFTER peak SWE
  ZeroSWEday[t]=min((postPeakdate[postPeakSWE==(0)])) #minimum(first) date where SWE =0, after peak
}
PeakSWEDate=as.Date(PeakSWEday,origin = "1970-01-01") #convert dates into rDate format
ZeroSWEdate=as.Date(ZeroSWEday,origin = "1970-01-01") #convert dates into rDate format
doyPeakSWE= as.numeric(strftime(PeakSWEDate, format = "%j")) #convert to Day of Year
doyZeroSWE= as.numeric(strftime(ZeroSWEdate, format = "%j")) #convert to Day of Year

#Saved 4 columns Yind, PeakSWEdate, ZeroSWEDate and MaxSWE_in(snowpeaks renamed) 

SNOTELsummary =data.frame(Yind,snowpeaks, snowsum_ann, prec_acc, PrecAcc_in_total, PeakSWEDate, ZeroSWEdate)
# keeps=c("Yind", "PeakSWEDate", "ZeroSWEDate", "MaxSWE_in")
#  SNOTELsummary=SNOTELtotals[keeps]
#  save(SNOTELsummary,file="data/SNOTELsummary.rda")

SNOTELsummary
