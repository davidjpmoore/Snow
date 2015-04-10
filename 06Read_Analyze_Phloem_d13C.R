#Purpose: This is exploratory R script as a learning process that uses a data set of interest to the Snow project. Secondarily, it loads the cleaned master 13C soil and phloem isotope data into an R data frame, and analyzes and plots some of the the data.
#
#Author : Nicole Trahan
#Date: 03/17/2015
#
#
#load data using read table into a dataframe called "iphloem"
# iphloem = read.table("data/13C//Phloem13C_master.csv", na.strings=c('.'), stringsAsFactors=FALSE, head=TRUE, sep=",")

#read.table is confused by some of the special characters in the header file. Read.csv does not appear to have this issue so I used it here.
iphloem = read.csv("data//13c//Phloem13C_master.csv", na.strings=c('.'), stringsAsFactors=FALSE, head=TRUE)


#input file asks r to assign data with characters in it as char - NOT FACTORS |stringsAsFactors=FALSE | but Treatment and DOY need to be defines as factors for ANOVA analyses - so we'll designate them as such: 
iphloem$Treatment=as.factor(iphloem$Treatment)
iphloem$DOY=as.factor(iphloem$DOY)

#####################################Data Manipulation and Analysis########################################
#creating a standard error function, "se"
stderr <- function(x) sqrt(var(x)/length(x))

# using tapply to find the mean of variables of interest by factors of interest
tapply(iphloem$d13C,iphloem[,c("Treatment","DOY")],mean) 
# calculates mean of d13C of phloem exudates by snow treatment and julian day of year
tapply(iphloem$d13C,iphloem[,c("Treatment","DOY")],stderr) 
# calculates the standard error of d13C of phloem exudates by snow treatment and julian day of year

#Or you can create a table of desired functions by whatever factor you like with 'aggregate'. This one does mean and standard err of d13C by Treatment and julian day of year to create 'Table 1'

Table1 = aggregate(iphloem$d13C ~ iphloem$Treatment + iphloem$DOY, FUN =  function(x) c( MN = mean(x), SE= stderr(x) ) ) 

Table1

#Or you can use the ddply() function from the plyr package, which is probably the most common and flexible. A usefull reference is: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

install.packages("plyr")
library(plyr)

# Run the functions length, mean, sd, and standard error on the value of "d13C" for each group, 
# broken down by Treatment + DOY
summarydata <- ddply(iphloem, c("Treatment", "DOY"), summarise,
               N    = length(d13C),
               mean = mean(d13C),
               sd   = sd(d13C),
               se   = sd / sqrt(N) )
summarydata

#Exporting data. There are numerous methods for exporting R objects into other formats. For SPSS, SAS and Stata, you will need to load the foreign packages. For Excel, you will need the xlsx package.
#To A Tab Delimited Text File

write.table(summarydata, "/Users/nicoletrahan/Desktop/Snow_paper_SigmaPlot_Data/Phloem_d13C.txt", sep="\t")

#To an Excel Spreadsheet
install.packages("xlsx")
library(xlsx)
write.xlsx(summarydata, "/Users/nicoletrahan/Desktop/Snow_paper_SigmaPlot_Data/Phloem_d13C.xlsx", sheetName="Sheet1")


#A two-way factorial ANOVA for d13C by Treatment and Julian DOY and their interactions. Remember to define the factors as such (see above).

anova_d13C = aov(d13C ~ Treatment + DOY + Treatment*DOY,data=iphloem)

#diagnostic plots for checking heteroscedasticity, normality, and influential observerations.
plot(anova_d13C)

#display Type I ANOVA table
summary(anova_d13C)
#display Type III ss and F tests
drop1(anova_d13C,~.,test="F")


#Tukey's HSD post-hoc test for our significant difference, which in this case is DOY
TukeyHSD(anova_d13C)

#It would be useful to develop an apply function or a for loop to run multiple ANOVA's and create plots on variables relevant to the hypotheses you would like to test. I have a sense you'd vectorize your dependent variables to start, but not sure how to get there....dplyr?

#####################################Let's Graph!#########################################################
#Some easy graphs in the base r package:

#Simple scatterplot matrices for visualizing the data (all):
pairs(~DOY+Treatment+d13C+WtpN+WtpC+CNratio,data=iphloem, 
      main="Simple Scatterplot Matrix")
#simplified by treatment
pairs(~Treatment+d13C+WtpN+WtpC+CNratio,data=iphloem, 
      main="Simple Scatterplot Matrix")

# Boxplot of d13C by Treatment 
boxplot(d13C ~ Treatment, data=iphloem, main="Phloem d13C by Snow Treatment", 
        xlab="Snow Treatment", ylab="Phloem d13C (per mil)")
# Boxplot of Percent C by Treatment 
boxplot(WtpC ~ Treatment, data=iphloem, main="Phloem Percent C by Snow Treatment", 
        xlab="Snow Treatment", ylab="Phloem C(%)")
# Boxplot of Percent N by Treatment 
boxplot(WtpN ~ Treatment, data=iphloem, main="Phloem Percent N by Snow Treatment", 
        xlab="Snow Treatment", ylab="Phloem N (%)")
# Boxplot of C:N by Treatment 
boxplot(CNratio ~ Treatment, data=iphloem, main="Phloem C:N by Snow Treatment", 
        xlab="Snow Treatment", ylab="Phloem C:N")
# Boxplot of d13C by DOY 
boxplot(d13C ~ DOY, data=iphloem, main="Phloem d13C by day of year", 
        xlab="Day of Year", ylab="d13C (per mil)")
# Boxplot of %C by DOY 
boxplot(WtpC ~ DOY, data=iphloem, main="Phloem percent C by day of year", 
        xlab="Day of Year", ylab="C (%)")
# Boxplot of %N by DOY 
boxplot(WtpN ~ DOY, data=iphloem, main="Phloem percent N by day of year", 
        xlab="Day of Year", ylab="N (%)")
# Boxplot of C:N by DOY 
boxplot(CNratio ~ DOY, data=iphloem, main="Phloem C:N by day of year", 
        xlab="Day of Year", ylab="C:N")

#OK, so how about the mean and the confidence interval and the standard error? Here's one example that uses the package 'sciplot', A collection of functions that creates graphs with error bars for data collected from one-way or higher factorial designs: http://cran.r-project.org/web/packages/sciplot/index.html
#example code###############
#install.packages("sciplot")
#library(sciplot)
#bargraph.CI(
  #class,  #categorical factor for the x-axis
  #hwy,    #numerical DV for the y-axis
  #year,   #grouping factor
  #legend=T, 
  #x.leg=19,
  #ylab="Highway MPG",
  #xlab="Class")

library(sciplot)
#plotting mean and CI of d13C over DOY grouped by treatment
bargraph.CI(iphloem$DOY,iphloem$d13C,iphloem$Treatment,legend=TRUE,ylab="Phloem d13C (permil)", xlab = "Snow Treatment by DOY", ylim = c(-30,-25), y.leg = -28)

#######ggplot2########################
#So ggplot2 appears to be a common powerful and preferred graphics package.  Let's learn some! (adapted fromhttp://www.r-bloggers.com/basic-introduction-to-ggplot2/ and https://github.com/echen/ggplot2-tutorial
#Verdict = qplot kicks ass, except for barplots 

install.packages("ggplot2")
library(ggplot2)

#histogram
qplot(data=iphloem, x = d13C, main = "Histogram of d13C")

#basic scatter organized by color, then size
qplot(data=iphloem, x = DOY, y= d13C, color=Treatment)
# By setting the alpha of each point to 0.7, we reduce the effects of overplotting points.
qplot(data=iphloem, x = Treatment, y= d13C, color=WtpC, size=DOY, alpha = I(0.7))
#adding labels
qplot(data=iphloem, x = Treatment, y= d13C, color=WtpC, size=DOY, alpha = I(0.7), xlab= "Snow Treatment", ylab= "d13C(per mil)", main = "Phloem d13C, percent C and day of year by snowpack treatment")

#box plot
qplot(data=iphloem, x= Treatment, y=d13C, geom="boxplot")

#looking at %C vs d13C faceted into seperate plots over the various snow treatments, color coded by DOY
qplot(data=iphloem, x=WtpC, y=d13C,color=DOY,facets = ~Treatment)

#adding transformation layers- in this case a statistical trend line
#1) Convert plot into a variable
Awesomeplot1 <- qplot(data=iphloem, x=WtpC, y=d13C,color=Treatment,facets = ~Treatment)
#2) Add a layer: (OLS trendline)
Awesomeplot1 <- Awesomeplot1 + stat_smooth(method="lm")
Awesomeplot1

#barcbart of d13C means
#This actually gets complicated. . .I couldn't figure out how to do this in qplot, but used our summary data generated by the ddply() and ggplot2; this requires your data to be in the "long" format; see http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# "Simple" standard error of the mean bar plot
pd <- position_dodge(0.9) # move objects to the left and right by some ? distance
ggplot(summarydata, aes(x=DOY, y=mean, fill=Treatment)) + 
  #geom_line() +
  #geom_point()
   geom_bar(stat = "identity", position= "dodge")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), color="black", width=.1, position= pd)+
  xlab("Day of Year") +
  ylab("d13C(per mil") +
  ggtitle("Mean Phloem d13C over\nsnow manipulations, 2009") +
  scale_colour_hue(name="Snow treatment", # Legend label, use darker colors
                   breaks=c(AG,CG,RG),
                   labels=c("Amendment", "Control", "Removal"),
                   l=40) +                  # Use darker colors, lightness=40
  
  scale_y_continuous(limits=c(0, max(mean + se)),    # Set y range
                     breaks=0:20*4) +                       # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) # Position legend in bottom right

#Must use position="dodge", otherwise ggplot defaults to stacked bars; must use stat="identity" or ggplot refuses to plot a y variable, instead plotting a count values of your indepent variable

#Getting Fancy:

ggplot(summarydata, aes(x=DOY, y=mean, colour=Treatment, group=Treatment)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Day of Year") +
  ylab("d13C(per mil")) +
  scale_colour_hue(name="Snow treatment", # Legend label, use darker colors
                   breaks=c(),
                   labels=c("Amendment", "Control"),
                   l=40) +                  # Use darker colors, lightness=40
  ggtitle("Mean Phloem d13C over\nsnow manipulations 2009") +
  scale_y_continuous(limits=c(0, max(mean + se)),    # Set y range
                     breaks=0:20*4) +                       # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) # Position legend in bottom right


#saving and exporting your awesome plots
#use ggsave() and specify file path and extension
ggsave("/Users/nicoletrahan/Desktop/Snow_paper_SigmaPlot_Data/Phloem_d13C_vs_C.jpg")
#specifying size and dpi (see ?ggsave for more options)
ggsave("/Users/nicoletrahan/Desktop/Snow_paper_SigmaPlot_Data/Phloem_d13C_vs_C2.png", width = 4, height = 4, dpi=600)
#make twice as big as on the screen:
ggsave("/Users/nicoletrahan/Desktop/Snow_paper_SigmaPlot_Data/Phloem_d13C_vs_C3.png", scale=2)


