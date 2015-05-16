rm(list=ls(all=TRUE))
setwd("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP")

library(car)
library(vegan)
library(reshape2)

#Import core site quadrat data-----------------
Core <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/sev129_nppcorequadrat_20141215.txt")
  #uesless in current form
any(Core$cover<0)#True
any(Core$cover==0)#True
Core<-subset(Core,cover>0)#get rid of the ones that are not entered

#Core Site-based NMDS analysis-----------------------------------
#Recast with "site" as level of inference
bysite<-dcast(Core,site+year+season~species,value.var="cover",fun.aggregate=sum)
SiteYearSeason<-factor(paste(bysite$site,bysite$year,bysite$season))
bysitematrix<-bysite[,4:164]
row.names(bysitematrix)<-SiteYearSeason
siteInfo<-bysite[,1:3]

#NMDS
dist<-vegdist(bysitematrix)
nmds<-metaMDS(dist)#stress under 1.6

#Plot
plot(nmds,type="n")
points(nmds,pch=as.character(siteInfo$site),col=as.numeric(siteInfo$season))
  #B=blue grama, G=black grama, C=creosote
  #difficult to see the separation based on season
  #two black grama site/year/seasons are weird
plot(nmds,type="t")#the weird ones are 2013 and 2012 winter
palette(gray(1:length(unique(siteInfo$year))/length(unique(siteInfo$year))))
plot(nmds,type="n")
points(nmds,pch=19,col=as.numeric(siteInfo$year))
  #later years perhaps more spread out among seasons, but not in same direction
  #goes from black to white
palette("default")

#PERMANOVA
adonis(dist~siteInfo$year+siteInfo$season+siteInfo$site)
  #all significant, although site account for 64% variation and others only 2-3%

#Core Quad-based NMDS analysis----------------------------
#Recast with "quad" as level of inference
byquad<-dcast(Core,site+year+season+web+quad~species,value.var="cover",fun.aggregate=sum)
#Figure out which quads observed through all years
SiteWebQuad<-factor(paste(byquad$site,byquad$web,byquad$quad))
table(SiteWebQuad)#all different...
#Separate by site
Bmatrix<-byquad[byquad$site=="B",6:166]
Cmatrix<-byquad[byquad$site=="C",6:166]
Gmatrix<-byquad[byquad$site=="G",6:166]
row.names(Bmatrix)<-factor(paste(byquad$web,byquad$quad,byquad$year,byquad$season))[1:380]
row.names(Cmatrix)<-factor(paste(byquad$web,byquad$quad,byquad$year,byquad$season))[381:997]
row.names(Gmatrix)<-factor(paste(byquad$web,byquad$quad,byquad$year,byquad$season))[998:1523]
BInfo<-byquad[byquad$site=="B",1:5]
CInfo<-byquad[byquad$site=="C",1:5]
GInfo<-byquad[byquad$site=="G",1:5]
BInfo$quadID<-paste(BInfo$web,BInfo$quad,sep="")
CInfo$quadID<-paste(CInfo$web,CInfo$quad,sep="")
GInfo$quadID<-paste(GInfo$web,GInfo$quad,sep="")

#Site B (blue grama) NMDS
Bdist<-vegdist(Bmatrix)
#Bnmds<-metaMDS(Bdist)#stress is strangely high (hovers around the 0.2 mark)
Bnmds<-metaMDS(Bdist,k=3)#stress is lower (0.12) with 3D
stressplot(Bnmds)#looks okay
plot(Bnmds,type="text")#this doesn't look so weird
#Visualize
plot(Bnmds,type="n")
points(Bnmds,type="p",col=as.numeric(BInfo$season),pch=19)
  #movement through winter (black), spring (red), fall (green) obvious
palette(gray(1:length(unique(BInfo$year))/length(unique(BInfo$year))))
plot(Bnmds,type="n")
points(Bnmds,pch=21,bg=as.numeric(BInfo$year),col="black")
  #hard to tell strong pattern/gradient, but years in the middle all same place?
  #goes from black to white
quadcol<-palette(rainbow(length(unique(BInfo$quadID))))
points(Bnmds,pch=19,col=quadcol[match(BInfo$quadID,unique(BInfo$quadID))])
  #No super tight clustering per quad
palette("default")
points(Bnmds,pch=19,col=as.numeric(BInfo$web))
  #some spatial gradients
#PERMANOVA
adonis(Bdist~BInfo$year+BInfo$season+BInfo$web)
  #all significant, but all explaining only 5-6% variation

#Site C (creosote) NMDS
Cdist<-vegdist(Cmatrix)
#Cnmds<-metaMDS(Cdist)#stress is a bit high (hovers around the 0.17 mark)
Cnmds<-metaMDS(Cdist,k=3)#3D stress lower (0.11)
stressplot(Cnmds)#sudden step at the end
plot(Cnmds,type="text")#a bit weird pattern
#Visualize
plot(Cnmds,type="n")
points(Cnmds,type="p",col=as.numeric(CInfo$season),pch=19)
  #no obvious movement like before, but the winter plots super congregated
palette(gray(1:length(unique(CInfo$year))/length(unique(CInfo$year))))
plot(Cnmds,type="n")
points(Cnmds,pch=21,bg=as.numeric(CInfo$year),col="black")
  #hard to tell strong pattern/gradient
quadcol<-palette(rainbow(length(unique(CInfo$quadID))))
points(Cnmds,pch=19,col=quadcol[match(CInfo$quadID,unique(CInfo$quadID))])
  #More lustering per quad
palette("default")
points(Cnmds,pch=19,col=as.numeric(CInfo$web))
  #not much spatial gradients
#PERMANOVA
adonis(Cdist~CInfo$year+CInfo$season+CInfo$web)
  #all significant, but all explaining only 1-5% variation

#Site G (black grama) NMDS
Gdist<-vegdist(Gmatrix)
Gnmds<-metaMDS(Gdist)#stress <0.16
stressplot(Gnmds)#sudden step at the end
plot(Gnmds,type="text")
  #two outliers (like the bigger plot)
  #same quad in 2012 and 2013 winter, only thing in it is a bit of LATR
#Visualize
plot(Gnmds,type="n")
points(Gnmds,type="p",col=as.numeric(GInfo$season),pch=19)
  #again some winter to fall movement
palette(gray(1:length(unique(GInfo$year))/length(unique(GInfo$year))))
plot(Gnmds,type="n")
points(Gnmds,pch=19,col=as.numeric(GInfo$year))
  #WOW there is definitely a gradient here!! Directional change!
quadcol<-palette(rainbow(length(unique(GInfo$quadID))))
points(Gnmds,pch=19,col=quadcol[match(GInfo$quadID,unique(GInfo$quadID))])
  #More lustering per quad
palette("default")
points(Gnmds,pch=19,col=as.numeric(GInfo$web))
  #not much spatial gradients
#PERMANOVA
adonis(Gdist~GInfo$year+GInfo$season+GInfo$web)
  #all significant, year accounts for 10% of variation!
#Try again excluding the two weird points
Gmatrix<-Gmatrix[-c(489,502),]
GInfo<-GInfo[-c(489,502),]
Gdist<-vegdist(Gmatrix)
Gnmds<-metaMDS(Gdist)#stress <0.16
stressplot(Gnmds)
plot(Gnmds,type="text")
#Visualize
plot(Gnmds,type="n")
points(Gnmds,type="p",col=as.numeric(GInfo$season),pch=19)
  #again some winter to fall movement
palette(gray(1:length(unique(GInfo$year))/length(unique(GInfo$year))))
plot(Gnmds,type="n")
points(Gnmds,pch=21,bg=as.numeric(GInfo$year),col="black")
  #WOW there is definitely a gradient here!! Directional change!
quadcol<-palette(rainbow(length(unique(GInfo$quadID))))
points(Gnmds,pch=19,col=quadcol[match(GInfo$quadID,unique(GInfo$quadID))])
  #More clustering per quad
palette("default")
points(Gnmds,pch=19,col=as.numeric(GInfo$web))
  #spatial gradients more evident here
#PERMANOVA
adonis(Gdist~GInfo$year+GInfo$season+GInfo$web)
  #all significant, year accounts for 10% of variation!

#Burn vs. Un-burned data import-------------------
Burn <- read.csv("C:/Users/user/Dropbox/Anny/My research/Bouteloua demography/SEV NPP/sev156_nppburnquadrat_20141215.txt")
any(Burn[,1:11]=="NA")#False
any(Burn$cover<0)#True
any(Burn$cover==0)#True
Burn<-subset(Burn,cover>0)#get rid of the ones that are not entered
table(Burn[,4:5])
  #3 sites: G, MG, MS
  #G is all burned, MG/MS both burned and control
#Use MG and MS sites only
subBurn<-subset(Burn,site=="MG"|site=="MS")

#Site-based burned vs. unburned----------
#Recast with "site" as level of inference
bysite<-dcast(subBurn,site+treatment+year+season~species,value.var="cover",fun.aggregate=sum)
SiteTreatYearSeason<-factor(paste(bysite$site,bysite$treatment,bysite$year,bysite$season))
bysitematrix<-bysite[,5:133]
row.names(bysitematrix)<-SiteTreatYearSeason
siteInfo<-bysite[,1:4]

#NMDS
dist<-vegdist(bysitematrix)
nmds<-metaMDS(dist)#super tiny stress...
stressplot(nmds)#vertical step at the end
plot(nmds,type="t")
  #interesting... all MG's together, but MS has a few outlier years
  #looks like may be all winter surveys

#Visualize
plot(nmds,type="n")
points(nmds,pch=as.numeric(siteInfo$site),col=as.numeric(siteInfo$treatment))
  #MG=triangles, MS=crosses
  #More obvious burn separation for shrub site
points(nmds,pch=as.numeric(siteInfo$site),col=as.numeric(siteInfo$season))
  #The weird MS points are all winter
  #There seem to be very few MG winter surveys??
table(siteInfo[,c(1,4)])#yup, only 4 winter survey years for MG
palette(gray(1:length(unique(siteInfo$year))/length(unique(siteInfo$year))))
plot(nmds,type="n")
points(nmds,pch=19,col=as.numeric(siteInfo$year))
  #all the weird MS points are later years
  #there may be year effect for MS points, separate by treatment
  #goes from black to white
palette("default")

#PERMANOVA
adonis(dist~siteInfo$year+siteInfo$season+siteInfo$site+siteInfo$treatment)
  #all significant
  #site account for 30% variation, season 12%, and burn 9%

#quad-based burn vs. unburned--------------------------------
#Recast with "quad" as level of inference
byquad<-dcast(subBurn,site+year+season+quad+treatment~species,value.var="cover",fun.aggregate=sum)
#Figure out which quads observed through all years
SiteTreatQuad<-factor(paste(byquad$site,byquad$treatment,byquad$web,byquad$quad))
table(SiteTreatQuad)#pretty similary numbers
#Separate by site
MGmatrix<-byquad[byquad$site=="MG",6:134]
MSmatrix<-byquad[byquad$site=="MS",6:134]
row.names(MGmatrix)<-factor(paste(byquad$treatment,byquad$quad,byquad$year,byquad$season))[1:1679]
row.names(MSmatrix)<-factor(paste(byquad$treatment,byquad$quad,byquad$year,byquad$season))[1680:3716]
MGInfo<-byquad[byquad$site=="MG",1:5]
MSInfo<-byquad[byquad$site=="MS",1:5]
MGInfo$quadID<-paste(MGInfo$treatment,MGInfo$quad,sep="")
MSInfo$quadID<-paste(MSInfo$treatment,MSInfo$quad,sep="")

#Site MG (mix grass) NMDS
MGdist<-vegdist(MGmatrix)
#MGnmds<-metaMDS(MGdist)
  #stress is strangely high (hovers around the 0.2 mark)
  #Also takes forever (likely because of giant matrix)
MGnmds<-metaMDS(MGdist,k=3)#3D stress down to 0.13
stressplot(MGnmds)#holy shit that's a lot of points
  #80 quadrats through 11 years (3 seasons) is a lot of points
plot(MGnmds,type="text")#yeah this might be untenable, also what's up with square shape??
#Visualize
plot(MGnmds,type="n")
points(MGnmds,type="p",col=as.numeric(MGInfo$season),pch=19)
  #some movement (up) through seasons
points(MGnmds,type="p",col=as.numeric(MGInfo$treatment),pch=19)
  #burn is black, control is red
  #mostly intermixed
palette(gray(1:length(unique(MGInfo$year))/length(unique(MGInfo$year))))
plot(MGnmds,type="n")
points(MGnmds,pch=19,col=as.numeric(MGInfo$year))
  #hard to tell strong pattern/gradient
  #goes from black to white
quadcol<-palette(rainbow(length(unique(MGInfo$quadID))))
points(MGnmds,pch=19,col=quadcol[match(MGInfo$quadID,unique(MGInfo$quadID))])
  #No super tight clustering per quad, looks like confetti
  #also, 80 rainbow colors is mostly indistinguishable at fine scale
palette("default")
#PERMANOVA
adonis(MGdist~MGInfo$year+MGInfo$season+MGInfo$treatment)
  #takes 8 mins
  #all significant
  #year explains 14%, season 12%, treatment 6%

#Site MS (mix shrub) NMDS
MSdist<-vegdist(MSmatrix)
MSnmds<-metaMDS(MSdist)
  #stress super low, and warning for insufficient data
stressplot(MSnmds)#this is super weird... R-sq is 1
plot(MSnmds,type="text")#there is one super outlier spot 
  #B 15 2012 3: only has 25% NONE and nothing else...
#Retry without superoutlier
which(MSInfo$treatment=="B"&MSInfo$quad==15&MSInfo$year==2012&MSInfo$season==3)
  #1649
MSmatrix<-MSmatrix[-1649,]
MSInfo<-MSInfo[-1649,]
MSdist<-vegdist(MSmatrix)
#MSnmds<-metaMDS(MSdist)
  #took 22 mins on high performance
  #stress hovers around 0.2
MSnmds<-metaMDS(MSdist,k=3)#3D stress lower at 0.13
stressplot(MSnmds)
plot(MSnmds,type="t")#super weird patterns
#Visualize
plot(MSnmds,type="n")
points(MSnmds,type="p",col=as.numeric(MSInfo$season),pch=19)
  #a bunch of the winter points super duper weird
points(MSnmds,type="p",col=as.numeric(MSInfo$treatment),pch=19)
  #burn is black, control is red
  #definite separation
palette(gray(1:length(unique(MSInfo$year))/length(unique(MSInfo$year))))
plot(MSnmds,type="n")
points(MSnmds,pch=19,col=as.numeric(MSInfo$year))
  #lighter points seem to be in the middle?
  #that could point to recovery/homogenization post fire
quadcol<-palette(rainbow(length(unique(MSInfo$quadID))))
points(MSnmds,pch=19,col=quadcol[match(MSInfo$quadID,unique(MSInfo$quadID))])
  #some potential quad-based clustering
  #also, 80 rainbow colors is mostly indistinguishable at fine scale
palette("default")
#PERMANOVA
adonis(MSdist~MSInfo$year+MSInfo$season+MSInfo$treatment)
  #a heck of a long time, lost track
  #all sig, treatment explains most variation (8.5%)
  #all significant
  #year explains 14%, season 12%, treatment 6%
