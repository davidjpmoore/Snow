#DplR
library(magrittr)
library(dplyr)





#Annual Plots

SNOTELsummary_flux = SNOTELsummary %>%
  filter(Yind >1998 & Yind <2015)
NR1_AnnualRECO = NR1_AnnualRECO %>%
  filter(TIMESTAMP>1998 & TIMESTAMP <2015)

plot(SNOTELsummary_flux$PrecAcc_in_total, NR1_AnnualRECO$RECO_NT_VUT_MEAN, ann=FALSE)
title(paste("Annual RECO vs Accumulated Precip"),xlab="Precipitation",ylab="Annual Flux")

plot(SNOTELsummary_flux$PrecAcc_in_total, NR1_AnnualRECO$GPP_NT_VUT_MEAN, ann=FALSE)
title(paste("Annual GPP vs Accumulated Precip"),xlab="Precipitation",ylab="Annual Flux")

plot(NR1_AnnualRECO$GPP_NT_VUT_MEAN, NR1_AnnualRECO$RECO_NT_VUT_MEAN, ann=FALSE)
title(paste("Annual RECO vs Annual RECO"),xlab="Annual GPP",ylab="Annual RECO")




plot(SNOTELsummary_flux$snowpeaks, NR1_AnnualRECO$RECO_NT_VUT_MEAN, ann=FALSE)
title(paste("Annual RECO vs Peak Snowpack"),xlab="Precipitation",ylab="Annual Flux")

plot(SNOTELsummary_flux$snowpeaks, NR1_AnnualRECO$GPP_NT_VUT_MEAN, ann=FALSE)
title(paste("Annual GPP vs Peak Snowpack"),xlab="Precipitation",ylab="Annual Flux")


plot(SNOTELsummary_flux$PeakSWEDate, NR1_AnnualRECO$RECO_NT_VUT_MEAN, ann=FALSE)
title(paste("Annual RECO vs Date of Peak Snowpack"),xlab="Date of Peak Snowpack",ylab="Annual Flux")

plot(SNOTELsummary_flux$PeakSWEDate, NR1_AnnualRECO$GPP_NT_VUT_MEAN , ann=FALSE)
title(paste("Annual GPP vs Date of Peak Snowpack"),xlab="Date of Peak Snowpack",ylab="Annual Flux")





plot((SNOTELsummary_flux$PeakSWEDate-SNOTELsummary_flux$ZeroSWEdate), NR1_AnnualRECO$RECO_NT_VUT_MEAN, ann=FALSE)
title(paste("Annual RECO vs Melt Duration"),xlab="Melt Duration (days)",ylab="Annual Flux")

plot((SNOTELsummary_flux$PeakSWEDate-SNOTELsummary_flux$ZeroSWEdate), NR1_AnnualRECO$GPP_NT_VUT_MEAN, ann=FALSE)
title(("Annual GPP vs Melt Duration"), xlab="Melt Duration (days)",ylab="Annual Flux")


#regression and residual analysis
Flux.REG = lm(RECO_NT_VUT_MEAN ~ GPP_NT_VUT_MEAN, data = NR1_AnnualRECO)
plot(Flux.REG$residuals)
check=Flux.REG$residuals
plot(NR1_AnnualRECO$YEAR, Flux.REG$residuals, ann=FALSE,type="n")
title(paste("Annual RECO residuals vs Year"),xlab="Year",ylab="Residual Annual Flux")


x1=NR1_AnnualRECO$TIMESTAMP
y1=Flux.REG$residuals
plot(y1~x1,ann=FALSE,type="n", ylim=c(-40, 40))
lines(y1~x1,lwd=2)
title(paste("Variance in RECO not explained by GPP"),xlab="Date",ylab="Residuals")




x1=NR1_AnnualRECO$TIMESTAMP
y1=NR1_AnnualRECO$RECO_NT_VUT_MEAN
plot(y1~x1,ann=FALSE,type="n")
lines(y1~x1,lwd=2)
title(paste("RECO through time"),xlab="Date",ylab="Annual RECO")

x1=NR1_AnnualRECO$TIMESTAMP
y1=NR1_AnnualRECO$GPP_NT_VUT_MEAN
plot(y1~x1,ann=FALSE,type="n")
lines(y1~x1,lwd=2)
title(paste("GPP through time"),xlab="Date",ylab="Annual GPP")


x1=NR1_AnnualRECO$TIMESTAMP
y1=SNOTELsummary_flux$snowpeaks
plot(y1~x1,ann=FALSE,type="n")
lines(y1~x1,lwd=2)
title(paste("SNOWpack through time"),xlab="Date",ylab="Peak Snow SWE (in)")





#total Precip accumulated (solid), SWE (long dash) and Precip increment each year
for( i in min(wateryear):max(wateryear)){
  
x1=rDate[wateryear==(i)]
y3=SNOWnwt$PrecInc_in[wateryear==(i)]
y2=SNOWnwt$SWE_in[wateryear==(i)]
y1=SNOWnwt$PrecAcc_in[wateryear==(i)]

plot(y1~x1,ann=FALSE,type="n")
lines(y1~x1,lwd=2)
lines(y2~x1,lwd=2,lty=2)
lines(y3~x1,lwd=2,lty=3)
title(paste("Snow accumulation in", i),xlab="Date",ylab="Precipitation")
}


#defining the wateryear and waterdate
SNOWnwt$rDate=as.Date(SNOWnwt$Date)
SNOWnwt$Year=as.numeric(format(SNOWnwt$rDate, "%Y"))
SNOWnwt$waterdate=SNOWnwt$rDate+90
SNOWnwt$wateryear=as.numeric(format(SNOWnwt$waterdate, "%Y"))
#subset data for 1999 through 2014
SNOWflux = subset(SNOWnwt,SNOWnwt$Year > 1998 & SNOWnwt$Year < 2015)

FLUXflux = subset(NR1_DailyRECO,NR1_DailyRECO$YEAR > 1998 & NR1_DailyRECO$YEAR < 2015)

fyear=FLUXflux$YEAR
fRECO_NT_VUT_MEAN = FLUXflux$RECO_NT_VUT_MEAN*3.5
fGPP_NT_VUT_MEAN = FLUXflux$GPP_NT_VUT_MEAN*3.5
fTA_F_MDS = FLUXflux$TA_F_MDS
fLE_F_MDS= FLUXflux$LE_F_MDS/5
frDate=NR1_DailyRECO$rDate
fPrecInc_in=SNOWflux$PrecInc_in
fSWE_in=SNOWflux$SWE_in
fPrecAcc_in=SNOWflux$PrecAcc_in
fSWC=FLUXflux$SWC_F_MDS_1

for( i in min(fyear):max(fyear)){
  
  x1=frDate[fyear==(i)]
  y6=fTA_F_MDS[fyear==(i)]
  y5=fGPP_NT_VUT_MEAN[fyear==(i)]
  y4=fRECO_NT_VUT_MEAN[fyear==(i)]
  y3=fPrecInc_in[fyear==(i)]
  y2=fSWE_in[fyear==(i)]
  y1=fPrecAcc_in[fyear==(i)]
  
  plot(y1~x1,ann=FALSE,type="n", ylim=c(0, 35))
  lines(y1~x1,lwd=2)
  lines(y2~x1,lwd=2,lty=2)
  lines(y3~x1,lwd=2,lty=3)
  lines(y4~x1,lwd=2,col=34)
  lines(y5~x1,lwd=2,col=27)
  lines(y6~x1,lwd=2,lty=3, col=84)
  title(paste("Snow accumulation & GPP (Green) RECO (RED) in", i),xlab="Date",ylab="Precipitation")
}


for( i in 2008:2009){
  
  x1=frDate[fyear==(i)]
  y5=fLE_F_MDS[fyear==(i)]
  y4=fGPP_NT_VUT_MEAN[fyear==(i)]
  y3=fRECO_NT_VUT_MEAN[fyear==(i)]
  y2=fSWE_in[fyear==(i)]
  y1=fPrecAcc_in[fyear==(i)]
  
  plot(y1~x1,ann=FALSE,type="n", ylim=c(0, 35))
  lines(y1~x1,lwd=2,lty=3, col="black")
  lines(y2~x1,lwd=2, col="black")
  lines(y3~x1,lwd=2,col="red")
  lines(y4~x1,lwd=2,col="green")
  lines(y5~x1,lwd=2, col=29) 
  
  title(paste("Snow accumulation & GPP (Green) RECO (RED) in", i),xlab="Date",ylab="Precipitation")
}



for( i in 2008:2009){
  
  x1=frDate[fyear==(i)]
  y5=fSWC[fyear==(i)]
  y4=fGPP_NT_VUT_MEAN[fyear==(i)]
  y3=fRECO_NT_VUT_MEAN[fyear==(i)]
  y2=fSWE_in[fyear==(i)]
  y1=fPrecAcc_in[fyear==(i)]
  
  plot(y1~x1,ann=FALSE,type="n", ylim=c(0, 35))
  lines(y1~x1,lwd=2,lty=3, col="black")
  lines(y2~x1,lwd=2, col="black")
  lines(y3~x1,lwd=2,col="red")
  lines(y4~x1,lwd=2,col="green")
  lines(y5~x1,lwd=2, col=29) 
  
  title(paste("SWE (black) & GPP (Green) RECO (RED), SWC(blue)", i),xlab="Date",ylab="Value")
}
















FLUXflux2008 = FLUXflux  %>%
  subset(YEAR ==2008) %>%
filter (rDate !="2008-12-31") 

FLUXflux2009 = subset(FLUXflux,FLUXflux$YEAR ==2009)

SNOWflux2008 = SNOWflux  %>%
  subset(Year ==2008) %>%
  filter (rDate !="2008-12-31") 

SNOWflux2009 = SNOWflux  %>%
  subset(Year ==2009)
  
         
#year to year differences
GPPdiff2009_2008 = FLUXflux2009$GPP_NT_VUT_MEAN-FLUXflux2008$GPP_NT_VUT_MEAN
RECOdiff2009_2008 = FLUXflux2009$RECO_NT_VUT_MEAN-FLUXflux2008$RECO_NT_VUT_MEAN
LE_F_MDSDiff2009_2008 = (FLUXflux2009$LE_F_MDS - FLUXflux2008$LE_F_MDS)/5
AirTempDiff2009_2008 = (FLUXflux2009$TA_F_MDS - FLUXflux2008$TA_F_MDS)/5
SWRadDiff2008_2008 = FLUXflux2009$SW_IN_F_MDS - FLUXflux2008$SW_IN_F_MDS
SWCDiff2009_2008 = FLUXflux2009$SWC_F_MDS_1 - FLUXflux2008$SWC_F_MDS_1

SWE_inDiff2009_2008 = SNOWflux2009$SWE_in - SNOWflux2008$SWE_in
PrecAcc_inDiff2009_2008 = SNOWflux2009$PrecAcc_in - SNOWflux2008$PrecAcc_in

  x1=frDate[fyear==(2009)]
  y6=LE_F_MDSDiff2009_2008
  y5=SWCDiff2009_2008
  y4=RECOdiff2009_2008
  y3=GPPdiff2009_2008
  y2=SWE_inDiff2009_2008
  y1=PrecAcc_inDiff2009_2008
  
  plot(y1~x1,ann=FALSE,type="n", ylim=c(-8, 12))
  lines(y1~x1,lwd=2)
  lines(y2~x1,lwd=2,lty=2)
  lines(y3~x1,lwd=2,lty=3)
  lines(y3~x1,lwd=2,col=34)
  lines(y4~x1,lwd=2,col=27)
  lines(y5~x1,lwd=2,lty=1, col=12) 
  lines(y6~x1,lwd=2,lty=3, col=27) 
  title(paste("Differences 2009 - 2008"),xlab="Date",ylab="Differences")


