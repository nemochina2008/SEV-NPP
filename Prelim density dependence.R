rm(list=ls(all=TRUE))
setwd("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP")
library(car)

#Data Import Core sites-------------------
NPP <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/sev182_anpp_07042012.txt")
Cover <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/sev129_nppcorequadrat_20141215.txt")

#Check data availability for BOER
BOERNPP<-subset(NPP,species=="BOER4")
table(BOERNPP[,2:3])#present at all three sites to various degrees, all unburned

#Try just site G (Black grama) first-------------
GNPP<-subset(NPP,site=="G")
GCover<-subset(Cover,site=="G")
GCover$quadID<-factor(paste(GCover$web,GCover$plot,GCover$quad,sep=""))
GNPP$quadID<-factor(paste(GNPP$web,GNPP$plot,GNPP$quad,sep=""))
table(GNPP[,c(1,14)])
  #not all quads observed all years
  #only 1999-2011
GNPP2011<-subset(GNPP,year==2011)
unique(GNPP2011$quadID)
  #only 30 quads all years
  #All 1's and 3's of each web

#Just do 2011 data only
GNPP2011<-subset(GNPP,year==2011)
GCover2011<-subset(GCover,year==2011)
unique(GCover2011$species)
  #Only BOER, written BOER4 and BOER
for (i in 1:length(GNPP2011[,1])){
  if (GNPP2011$species[i]=="BOER") GNPP2011$species[i]<-"BOER4"
}
for (i in 1:length(GCover2011[,1])){
  if (GCover2011$species[i]=="BOER") GCover2011$species[i]<-"BOER4"
}
BOERNPP<-subset(GNPP2011,species=="BOER4"&fwt>0)
BOERCover<-subset(GCover2011,species=="BOER4")
  #BOER present in all 30 quads
for (i in 1:length(unique(BOERCover$quadID))){
  subdata<-subset(BOERCover,quadID==unique(BOERCover$quadID)[i])
  index<-length(subdata[,1])#to know how many indivs of BOER counted
  BOERNPP$Dens[i]<-subdata$obs[index]
}
BOERNPP$PerCap<-BOERNPP$fwt/BOERNPP$Dens
plot(BOERNPP$Dens,BOERNPP$PerCap)#There's a super outlier (last quad)

#Do multiple years for those quads observed 1999-2011 at site G--------------
#which quads were observed for all years?
quadIndex<-c()
j<-1
for(i in 1:length(unique(GNPP$quadID))){
  subdata<-subset(GNPP,quadID==unique(GNPP$quadID)[i])
  if (length(unique(subdata$year))==13) {
    quadIndex[j]<-as.character(subdata$quadID[1])
    j<-j+1
  }
  else next
}
timeCover<-subset(GCover,GCover$quadID%in%quadIndex&GCover$year<2012)
timeNPP<-subset(GNPP,GNPP$quadID%in%quadIndex)
#Use only Fall data
timeCover<-subset(timeCover,timeCover$season==3)
timeCover$quadYear<-paste(timeCover$quadID,timeCover$year,sep="")
timeNPP$quadYear<-paste(timeNPP$quadID,timeNPP$year,sep="")

#Calculate densities and total covers
timeBOERCover<-subset(timeCover,species=="BOER4")
timeBOERNPP<-subset(timeNPP,species=="BOER4")
for (i in 1:length(unique(timeBOERCover$quadYear))){
  subdata<-subset(timeBOERCover,quadYear==unique(timeBOERCover$quadYear)[i])
  index<-length(subdata[,1])#to know how many indivs of BOER counted
  timeBOERNPP$Dens[i]<-subdata$obs[index]
}
timeBOERNPP$PerCap<-timeBOERNPP$fwt/timeBOERNPP$Dens
timeBOERNPP$Tot.cov<-c()
for (i in 1:length(unique(timeCover$quadYear))){
  subdata<-subset(timeCover,quadYear==unique(timeCover$quadYear)[i])
  timeBOERNPP$Tot.cov[i]<-sum(subdata$cover)
}
timeBOERNPP$Het.cov<-c()
for (i in 1:length(unique(timeCover$quadYear))){
  subdata<-subset(timeCover,quadYear==unique(timeCover$quadYear)[i]&species!="BOER4")
  timeBOERNPP$Het.cov[i]<-sum(subdata$cover)
}
quadcol<-palette(rainbow(length(unique(timeBOERNPP$quadID))))
plot(timeBOERNPP$Dens,timeBOERNPP$PerCap,col=quadcol,pch=19)#neg
plot(timeBOERNPP$Tot.cov,timeBOERNPP$PerCap,col=quadcol,pch=19)#pos
plot(timeBOERNPP$Het.cov,timeBOERNPP$PerCap,col=quadcol,pch=19)#neg
palette("default")


#Try comparing between core sites for BOER in 2011--------
#Just do 2011 data only
NPP2011<-subset(NPP,year==2011)
Cover2011<-subset(Cover,year==2011)
unique(Cover2011$species)
  #both BOER4 and BOGR2
#Get rid of data mistakes
NPP2011<-subset(NPP2011,fwt>0)
Cover2011<-subset(Cover2011,cover>0&season==3)#only use fall data
NPP2011$quadID<-paste(NPP2011$site,NPP2011$web,NPP2011$plot,NPP2011$quad,sep="")
Cover2011$quadID<-paste(Cover2011$site,Cover2011$web,Cover2011$plot,Cover2011$quad,sep="")
BOERNPP<-subset(NPP2011,species=="BOER4")
BOERCover<-subset(Cover2011,species=="BOER4")
table(BOERNPP$site)
  #BOER occurs in 17 BOGR quads, 3 creosote quads, and 30 BOER quads in 2011

for (i in 1:length(unique(BOERCover$quadID))){
  subdata<-subset(BOERCover,quadID==unique(BOERCover$quadID)[i])
  index<-length(subdata[,1])#to know how many indivs of BOER counted
  BOERNPP$Dens[i]<-subdata$obs[index]
}
BOERNPP$PerCap<-BOERNPP$fwt/BOERNPP$Dens

for (i in 1:length(unique(BOERCover$quadID))){
  subdata<-subset(Cover2011,quadID==unique(BOERCover$quadID)[i])
  BOERNPP$Tot.cov[i]<-sum(subdata$cover)
}
plot(BOERNPP$Dens,BOERNPP$PerCap,pch=as.character(BOERNPP$site),col=as.numeric(BOERNPP$site))#There's a super outlier (last quad)
  #Interesting, the BOGR core site almost looks like facilitation
hist(BOERNPP$PerCap)
qqPlot(BOERNPP$PerCap)
m1<-lm(PerCap~Dens*site,data=BOERNPP)
summary(m1)#only site G sig diff from site B
plot(BOERNPP$Tot.cov,BOERNPP$PerCap,pch=as.character(BOERNPP$site),col=as.numeric(BOERNPP$site))#There's a super outlier (last quad)
  #Interesting, definitely positive
m2<-lm(PerCap~Tot.cov*site,data=BOERNPP)
summary(m2)#Tot.cov p<0.001, siteG p=0.02, Tot.cov:siteG p=0.03
plot(m2)

#Try doing site-based comparison through time

#Data Import Burn/Unburned sites----------------
NPP <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/sev185_nppburnbiomass_20150304.txt")
Cover <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/sev156_nppburnquadrat_20141215.txt")
  #glommed by spp for each quad in NPP, but not in Cover

#Try null case-----------------
#Numbers here correspond to the greenhouse BOGR biomass data
density<-rnorm(500,mean=3.633562,sd=1.600892)
hist(density)
biomass<-rnorm(500,mean=-1.692934,sd=0.6069168)
hist(biomass)
range(biomass)#all negative
percap<-biomass/density
hist(percap)#well thats weird
plot(density,percap)#super bizarre... is this reassuring?

#Try with numbers from NPP data?
meanDens<-mean(timeBOERNPP$Dens)
sdDens<-sd(timeBOERNPP$Dens)
meanMass<-mean(timeBOERNPP$fwt)
sdMass<-sd(timeBOERNPP$fwt)
density<-rnorm(500,mean=meanDens,sd=sdDens)
hist(density)#goes below zero
biomass<-rnorm(500,mean=meanMass,sd=sdMass)
hist(biomass)#goes below zero
percap<-biomass/density
hist(percap)
plot(density,percap)#weird null pattern
  #ignoring the negative numbers, seems like null is a log function
  #which is kind of what the data are showing...
#alternatively, draw percap weight directly
meanPerCap<-mean(timeBOERNPP$PerCap)
sdPerCap<-sd(timeBOERNPP$PerCap)
percap<-rnorm(500,mean=meanPerCap,sd=sdPerCap)
plot(density,percap)#now this is random

#Try to loop through a variety of numbers
densMeans<-c(10,20,50)
densSD<-c(1,4,10)
massMeans<-c(100,500,1500,5000)
massSD<-c(10,40,100)
#Make data frame of all combos of all variables
data<-expand.grid(densMeans=densMeans,massMeans=massMeans,densSD=densSD,massSD=massSD)
data$slope<-c()
data$n<-c()
#Loop and calculate slopes
for(i in 1:length(data[,1])){
  density<-rnorm(100,mean=data$densMeans[i],sd=data$densSD[i])
  biomass<-rnorm(100,mean=data$massMeans[i],sd=data$massSD[i])
  percap<-biomass/density
  index<-which(percap>0&density>0)
  model<-lm(percap[index]~density[index])
  data$slope[i]<-coef(model)[2]
  data$n[i]<-length(index)
}
hist(data$slope)#mostly negative
hist(data$n)
plot(data$densMeans,data$slope)
plot(data$massMeans,data$slope)
plot(data$n,data$slope)
library(scatterplot3d)
scatterplot3d(data$densMeans,data$massMeans,data$slope,type="h")
