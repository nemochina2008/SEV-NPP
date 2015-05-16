rm(list=ls(all=TRUE))
setwd("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP")
library(car)

#Import data on cover-biomass relationships
Data <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/sev157_nppweight_20150121.txt")

#Just BOER-------------------------------------------
BOER <- subset(Data, Species=="BOER4")
summary(BOER)
table(BOER[, c(1,2,4)])#Data was collected only 1 or 2 times per species site combo

#Subset data to just BOER at sites G (BOER) and C (creosote), both harvest in 1999/2000
BOERsub <- subset(BOER, Site=="G"|Site=="C")
BOERsub$Vol<-BOERsub$Cover*BOERsub$Height*100 #for units in cm3
BOERsub<-subset(BOERsub,Vol>0)#get rid of missing values
BOERsub<-subset(BOERsub,Live_Weight>0)#get rid of missing values
plot(BOERsub$Vol[1:10],BOERsub$Live_Weight[1:10])#1999, season 1, site C
  #Doesn't actually seem particularly linear, but also not obviously nonlinear
m0<-lm(BOERsub$Live_Weight[1:10]~BOERsub$Vol[1:10])
summary(m0)#R-sq=0.95, slope=6.152e-04
plot(m0)#resids don't look good
m1<-lm(log(BOERsub$Live_Weight[1:10])~BOERsub$Vol[1:10])
summary(m1)#R-sq=0.82
plot(m1)#resids more normal, but there's a pattern vs. fitted
  #Stick with linear model

#Visualize
palette(rainbow(15))#set color palette to draw from, since as.numeric(YearSeasonSite) exceeds the default 8
BOERsub$YearSeasonSite<-factor(paste(BOERsub$Year,BOERsub$Season,BOERsub$Site,sep=""))
plot(BOERsub$Vol,BOERsub$Live_Weight,col=as.numeric(BOERsub$Year),pch=19)#definite differences by year
plot(BOERsub$Vol,BOERsub$Live_Weight,col=as.numeric(BOERsub$Season),pch=19)#fewer diffs by season
plot(BOERsub$Vol,BOERsub$Live_Weight,col=as.numeric(BOERsub$Site),pch=19)#fewer diffs by site
plot(BOERsub$Vol,BOERsub$Live_Weight,col=as.numeric(BOERsub$YearSeasonSite),pch=19)#fewer diffs by site

#Calculate slopes 
BOERslopes<-BOERsub[1,c(1,2,4)]
for (i in 1:length(unique(BOERsub$YearSeasonSite))){
  subdata<-subset(BOERsub,BOERsub$YearSeasonSite==unique(BOERsub$YearSeasonSite)[i])
  BOERslopes[i,c(1,2,3)]<-subdata[1,c(1,2,4)]
  model<-lm(subdata$Live_Weight~subdata$Vol)
  BOERslopes$Slope[i]<-model$coeff[2]
  BOERslopes$Intercept[i]<-model$coeff[1]
  BOERslopes$CV[i]<-(sd(subdata$Live_Weight/subdata$Vol)/mean(subdata$Live_Weight/subdata$Vol))
}

#Visualize:slopes, intercepts, CVs
boxplot(BOERslopes$Slope~BOERslopes$Year)#slopes higher in 1999
boxplot(BOERslopes$Slope~BOERslopes$Season)#slopes lower in winter
boxplot(BOERslopes$Slope~BOERslopes$Site)#slopes more variable in C
boxplot(BOERslopes$Slope~BOERslopes$Year*BOERslopes$Season)
  #careful, only 2 estimates per mean on this one
  #likely a year*season interaction
boxplot(BOERslopes$Intercept~BOERslopes$Year)#more variable in 1999
boxplot(BOERslopes$Intercept~BOERslopes$Season)#more variable in spring and fall
boxplot(BOERslopes$Intercept~BOERslopes$Site)#more variable in G
boxplot(BOERslopes$CV~BOERslopes$Year)#Higher slope CV in 2000
boxplot(BOERslopes$CV~BOERslopes$Season)#about same between seasons
boxplot(BOERslopes$CV~BOERslopes$Site)#Higher slope CV in G

#BOGR----------------------------------------------------
BOGR <- subset(Data, Species=="BOGR2")
summary(BOGR)
table(BOGR[, c(1,2,4)])
  #Data was collected only 1 time per species site season combo for core sites
  #more long-term data available for site P (Cerro Montoso PJ 1999-2014,skips 2002)
  #lots of data for site L, which is a mix of B, C, G??

#Try looking at just Cerro Montosa PJ (P), and Deep Well area (L)
BOGRsub<-subset(BOGR,Site=="P"|Site=="L")
BOGRsub$Vol<-BOGRsub$Cover*BOGRsub$Height*100 #for units in cm3
BOGRsub<-subset(BOGRsub,Vol>0)#get rid of missing values
BOGRsub<-subset(BOGRsub,Live_Weight>0)#get rid of missing values
BOGRsub$YearSeasonSite<-factor(paste(BOGRsub$Year,BOGRsub$Season,BOGRsub$Site,sep=""))
  #most of the data is in spring and fall

#Visualize just fall through the years (most comprenhensive data)
BOGRsub3<-subset(BOGRsub, Season==3)
table(BOGRsub3[,4:5])#only L has burn vs. control
BOGRsub3$newSite<-factor(paste(BOGRsub3$Site,BOGRsub3$Treatment,sep=""))
BOGRsub3$YearSite<-factor(paste(BOGRsub3$Year,BOGRsub3$newSite,sep=""))
palette(rainbow(length(unique(BOGRsub3$YearSite))))
plot(BOGRsub3$Vol,BOGRsub3$Live_Weight,col=as.numeric(BOGRsub3$YearSite),pch=19)
  #hmm colors really hard to tell
palette("default")

#Calculate slopes 
BOGRslopes<-BOGRsub3[1,c(1,2,17)]
BOGRresid<-BOGRsub3[,c(1,17)]
BOGRresid$Resid<-rep(0,length(BOGRresid[,1]))
BOGRresid$Fitted<-rep(0,length(BOGRresid[,1]))
j=0
for (i in 1:length(unique(BOGRsub3$YearSite))){
  subdata<-subset(BOGRsub3,YearSite==unique(BOGRsub3$YearSite)[i])
  BOGRslopes[i,c(1,2,3)]<-subdata[1,c(1,2,17)]
  model<-lm(subdata$Live_Weight~subdata$Vol-1)
  BOGRslopes$Slope[i]<-model$coeff
  BOGRslopes$CV[i]<-(sd(subdata$Live_Weight/subdata$Vol)/mean(subdata$Live_Weight/subdata$Vol))
  BOGRresid$Resid[(j+1):(j+length(subdata[,1]))]<-model$resid
  BOGRresid$Fitted[(j+1):(j+length(subdata[,1]))]<-fitted(model)
  j<-j+length(subdata[,1])
}

#Visualize
plot(BOGRslopes$Year,BOGRslopes$Slope,type="p",pch=as.numeric(BOGRslopes$newSite))
  #symbols: PC=cross LB=circle LC=triangle
  #pretty cool, annual trends in slopes look similar
  #Except for one year, burned grass always has higher slope than unburned
plot(BOGRslopes$Year,BOGRslopes$CV,type="p",pch=as.numeric(BOGRslopes$newSite))
plot(BOGRresid$Fitted,BOGRresid$Resid,pch=as.numeric(BOGRslopes$newSite))
abline(0,0)
    #variance poofs out, but no trend in over-contribution of large indivs
    #but overall suggests nonlinearity?
m0<-lm(Resid~Fitted+Year+newSite+Fitted:Year+Fitted:newSite,data=BOGRresid)
summary(m0)
  #site may be significant

#Add in precip data
PJClim<- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/Cerro_Montoso_Climate_Clean.csv")
DWClim <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/Deep_Well_Precip_Clean.csv")
BOGRslopes$FallPPT<-rep(0,length(BOGRslopes[,1]))
BOGRslopes$SpringPPT<-rep(0,length(BOGRslopes[,1]))
for(i in 1:length(BOGRslopes[,1])){
  if (BOGRslopes$newSite[i]=="PC"){
    BOGRslopes$AnnPPT[i]<-PJClim$AnnPPT[which(PJClim$Year==BOGRslopes$Year[i])]
  }
  else {
    subDW<-subset(DWClim,Year==BOGRslopes$Year[i])
    BOGRslopes$AnnPPT[i]<-subDW$Precip[which(subDW$Season=="Annual")]
    BOGRslopes$FallPPT[i]<-subDW$Precip[which(subDW$Season=="Fall")]
    BOGRslopes$SpringPPT[i]<-subDW$Precip[which(subDW$Season=="Spring")]
  }
  
}
#Visualize--no patterns
plot(BOGRslopes$AnnPPT,BOGRslopes$Slope,pch=as.numeric(BOGRslopes$newSite))
L<-subset(BOGRslopes,newSite=="LB"|newSite=="LC")
plot(L$SpringPPT,L$Slope,pch=as.numeric(L$newSite))
plot(L$FallPPT,L$Slope,pch=as.numeric(L$newSite))

#Do a quick/dirty regression
qqPlot(BOGRslopes$Slope)#okay
m0<-lm(Slope~AnnPPT*newSite,data=BOGRslopes)
summary(m0)#Annual PPT p=0.04, positive slope...seems unlikely and prob dirven by the two low points bottom right corner
m1<-lm(Slope~AnnPPT+SpringPPT+FallPPT,data=L)
summary(m1)#weirdly all barely significant

#Double check the year where slopes were super high
BOGRslopes$Year[which(BOGRslopes$Slope>0.0035)]#2009
LB2009<-subset(BOGRsub3,newSite=="LB"&Year==2009)
PC2009<-subset(BOGRsub3,newSite=="PC"&Year==2009)
LC2009<-subset(BOGRsub3,newSite=="LC"&Year==2009)
mLB<-lm(Live_Weight~Vol-1,data=LB2009)
mLC<-lm(Live_Weight~Vol-1,data=LC2009)
mPC<-lm(Live_Weight~Vol-1,data=PC2009)
summary(mLB)
summary(mLC)
summary(mPC)
plot(LB2009$Vol,LB2009$Live_Weight)
plot(LC2009$Vol,LC2009$Live_Weight)
plot(PC2009$Vol,PC2009$Live_Weight)
  #All looks pretty linear
