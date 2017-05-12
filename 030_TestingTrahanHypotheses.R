#Trahan et al 2017

#enhancement of soil efflux during the 25 days immediately after the plots become snow free
#hypothesis that additional soil moisture will lead to additional respiration flux in the spring

#### higher SWE pre melt => greater respiration post melt ---- FALSE
### high RECO post melt is associated with high soil temperature 


#late snowmelt might mitigate against summer moisture limitation
##### later snow melt (weighted by ammount?) should mitigate against GPP limits in late season
###Test GPP 100 days AFTER melt vs melt out DATE

#### RUN 01ReadSNOTEdata.R
#### RUN 02CalculatePeak_and_Snowfree_Dates.R

library(dplyr)
library(ggplot2)

NR1_YY=read.csv("data/FluxData/FLX_US-NR1_FLUXNET2015_FULLSET_1998-2014_1-3/FLX_US-NR1_FLUXNET2015_FULLSET_YY_1998-2014_1-3.csv")
plot(NR1_YY$TIMESTAMP[NR1_YY$GPP_DT_CUT_MEAN>-1000], NR1_YY$GPP_DT_CUT_MEAN[NR1_YY$GPP_DT_CUT_MEAN>-1000])


NR1_DD=read.csv("data/FluxData/FLX_US-NR1_FLUXNET2015_FULLSET_1998-2014_1-3/FLX_US-NR1_FLUXNET2015_FULLSET_DD_1998-2014_1-3.csv")
plot(NR1_DD$TIMESTAMP[NR1_DD$GPP_DT_CUT_MEAN>-1000], NR1_DD$GPP_DT_CUT_MEAN[NR1_DD$GPP_DT_CUT_MEAN>-1000])

#create data 
NR1_DD_SNOW = NR1_DD %>%
  mutate (Yind =as.numeric(substr(TIMESTAMP,1,4))) %>%
  mutate (MONTH =as.numeric(substr(TIMESTAMP, 5, 6))) %>%
  mutate (DAY =as.numeric(substr(TIMESTAMP, 8, 8))) %>%
  mutate (rDate  = as.Date(ISOdate(Yind,MONTH,DAY))) %>%
  filter (Yind >1998) %>%
  filter (Yind <2015) %>%
  select(Yind, TIMESTAMP, rDate, MONTH, DAY, GPP_DT_CUT_MEAN, RECO_DT_CUT_MEAN, NEE_CUT_MEAN, TS_F_MDS_1, SWC_F_MDS_1)

#Create filters for 25 days after snowmelt

SNOTELsummary_filter = SNOTELsummary %>%
  filter (Yind >1998) %>%
  filter (Yind <2015) %>%
  mutate (EndDate = as.Date(ZeroSWEdate+25))

NR1_DD_SNOW_1 = inner_join(NR1_DD_SNOW, SNOTELsummary_filter)

NR1_DD_SNOW_postMelt = NR1_DD_SNOW_1 %>%
  mutate(meltperiod = as.numeric(ZeroSWEdate-PeakSWEDate) ) %>%
  filter(rDate>ZeroSWEdate) %>%
  filter(rDate<EndDate) %>%
  mutate (YEAR = as.factor(Yind)) 



NR1_DD_SNOW_postMelt_byYear= NR1_DD_SNOW_postMelt %>%
  group_by(YEAR) %>%
  summarise (MeanReco=mean(RECO_DT_CUT_MEAN), stdevReco=sd(RECO_DT_CUT_MEAN),
             MeanGPP=mean(GPP_DT_CUT_MEAN), stdevGPP=sd(GPP_DT_CUT_MEAN),
            MeanNEE=mean(NEE_CUT_MEAN), stdevNEE=sd(NEE_CUT_MEAN),
            MeanTSoil=mean(TS_F_MDS_1), stdevTSoil=sd(TS_F_MDS_1),
            MeanSWC=mean(SWC_F_MDS_1), stdevSWC=sd(SWC_F_MDS_1),
            ZeroSWEdate=min(ZeroSWEdate),
            snowpeaks=min(snowpeaks),
            snowsum_ann=min(snowsum_ann),
            meltperiod=min(meltperiod)
            )
            

plot(NR1_DD_SNOW_postMelt_byYear$snowsum_ann, NR1_DD_SNOW_postMelt_byYear$MeanNEE)

plot(NR1_DD_SNOW_postMelt_byYear$snowsum_ann, NR1_DD_SNOW_postMelt_byYear$MeanReco)


#meltperiod
####
x=NR1_DD_SNOW_postMelt_byYear$meltperiod
avg=NR1_DD_SNOW_postMelt_byYear$MeanReco
sdev=NR1_DD_SNOW_postMelt_byYear$stdevReco

plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Sum of SWE", ylab="Ecosystem Respiration +/- SD",
     main="Response of Respiration in the 25 days after snowmelt to melt period"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
#####


####
x=NR1_DD_SNOW_postMelt_byYear$snowsum_ann
avg=NR1_DD_SNOW_postMelt_byYear$MeanReco
sdev=NR1_DD_SNOW_postMelt_byYear$stdevReco

plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Sum of SWE", ylab="Ecosystem Respiration +/- SD",
     main="Response of Respiration in the 25 days after snowmelt to accumulated SWE"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
#####








####
x=NR1_DD_SNOW_postMelt_byYear$snowpeaks
avg=NR1_DD_SNOW_postMelt_byYear$MeanReco
sdev=NR1_DD_SNOW_postMelt_byYear$stdevReco

plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Peak of SWE", ylab="Ecosystem Respiration +/- SD",
     main="Response of Respiration in the 25 days after snowmelt to Peak SWE"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
#####



####
x=NR1_DD_SNOW_postMelt_byYear$MeanTSoil
avg=NR1_DD_SNOW_postMelt_byYear$MeanReco
sdev=NR1_DD_SNOW_postMelt_byYear$stdevReco

plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Soil Temperature", ylab="Ecosystem Respiration +/- SD",
     main="Response of Respiration in the 25 days after snowmelt to Temp"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
#####


m1 <- lm(avg~x)  #Create a linear model
resid(m1) #List of residuals
 
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
#####







####
x=NR1_DD_SNOW_postMelt_byYear$MeanSWC[NR1_DD_SNOW_postMelt_byYear$MeanSWC>-5000]
avg=NR1_DD_SNOW_postMelt_byYear$MeanReco[NR1_DD_SNOW_postMelt_byYear$MeanSWC>-5000]
sdev=NR1_DD_SNOW_postMelt_byYear$stdevReco[NR1_DD_SNOW_postMelt_byYear$MeanSWC>-5000]

plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="Soil Water Content", ylab="Ecosystem Respiration +/- SD",
     main="Response of Respiration in the 25 days after snowmelt to Soil Water Content"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
#####





# 
#   
#   NR1_DD_SNOW_postMelt0809 = NR1_DD_SNOW_postMelt %>%
#     filter(Yind>2007, Yind<2010)
  

  
  NR1_DD_SNOW_100days = NR1_DD_SNOW_1 %>%
  mutate (YEAR = as.factor(Yind)) %>%
    filter(rDate>(ZeroSWEdate+100)) %>%
    filter(rDate<(EndDate+100)) %>%
  mutate(meltperiod = as.numeric(ZeroSWEdate-PeakSWEDate))
    

  plot(NR1_DD_SNOW_100days$snowpeaks, NR1_DD_SNOW_100days$RECO_DT_CUT_MEAN)
  plot(NR1_DD_SNOW_100days$snowpeaks, NR1_DD_SNOW_100days$GPP_DT_CUT_MEAN)
  
  
  NR1_DD_SNOW_100days_byYear= NR1_DD_SNOW_100days %>%
    group_by(YEAR) %>%
    summarise (MeanReco=mean(RECO_DT_CUT_MEAN), stdevReco=sd(RECO_DT_CUT_MEAN),
               MeanGPP=mean(GPP_DT_CUT_MEAN), stdevGPP=sd(GPP_DT_CUT_MEAN),
               MeanNEE=mean(NEE_CUT_MEAN), stdevNEE=sd(NEE_CUT_MEAN),
               MeanTSoil=mean(TS_F_MDS_1), stdevTSoil=sd(TS_F_MDS_1),
               MeanSWC=mean(SWC_F_MDS_1), stdevSWC=sd(SWC_F_MDS_1),
               ZeroSWEdate=min(ZeroSWEdate),
               snowpeaks=min(snowpeaks),
               snowsum_ann=min(snowsum_ann),
               meltperiod=min(meltperiod)
    )
  
  
  
  #### SOIL WATER CONTENT
  x=NR1_DD_SNOW_100days_byYear$MeanSWC[NR1_DD_SNOW_100days_byYear$MeanSWC>-5000]
  avg=NR1_DD_SNOW_100days_byYear$MeanReco[NR1_DD_SNOW_100days_byYear$MeanSWC>-5000]
  sdev=NR1_DD_SNOW_100days_byYear$stdevReco[NR1_DD_SNOW_100days_byYear$MeanSWC>-5000]
  
  plot(x, avg,
       ylim=range(c(avg-sdev, avg+sdev)),
       pch=19, xlab="Soil Water Content", ylab="Ecosystem Respiration +/- SD",
       main="Response of Respiration in the 25 days after snowmelt to Soil Water Content"
  )
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  #####
  
  
  
  #### SOIL WATER CONTENT V SNOW
  x=NR1_DD_SNOW_100days_byYear$snowsum_ann[NR1_DD_SNOW_100days_byYear$MeanSWC>-5000]
  avg=NR1_DD_SNOW_100days_byYear$MeanSWC[NR1_DD_SNOW_100days_byYear$MeanSWC>-5000]
  sdev=NR1_DD_SNOW_100days_byYear$stdevSWC[NR1_DD_SNOW_100days_byYear$MeanSWC>-5000]
  
  plot (x,avg)
  
  plot(x, avg,
       ylim=range(c(avg-sdev, avg+sdev)),
       pch=19, xlab="Soil Water Content", ylab="Soil Water Content +/- SD",
       main="Response of Respiration in the 25 days after snowmelt to Soil Water Content"
  )
  # hack: we draw arrows but with very special "arrowheads"
  arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
  #####
  
  
  
  #first DEFINE the plot you want to see *NOTE instead of using ENZdat$pH as x you define the dataframe FIRST and then the x and y axes
  a <- ggplot(NR1_DD_SNOW_postMelt, aes(x=snowsum_ann, y=RECO_DT_CUT_MEAN))
  
  #make a plot with big red symbols
  a + geom_point(colour = "red", size = 6)
  a+ aes(color = Yind) + geom_boxplot(varwidth = T, group=snowpeaks_ann)
  
  #Show the site differences with different shapes and sizes
  a + aes(shape = factor(YEAR)) +
    geom_point(aes(colour = factor(YEAR)), size = 4) +
    geom_point(colour="grey90", size = 1.5)
  
  #show a color gradient of pH
  a + geom_point(colour="grey50", size = 4) + geom_point(aes(colour = pH)) + scale_colour_gradient(low = "yellow", high="red")
  
  #show the pH as a color and a size gradient
  a + geom_point(aes(fill=pH, size=pH), colour="black", shape=21) + scale_size(range = c(1, 20))
  
  
  
  
