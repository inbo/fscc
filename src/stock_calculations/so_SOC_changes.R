### so SOC stock changes & sequestration rates
### Bruno De Vos
### May 2024


#### Plotstocks

library(dplyr)
library(bootstrap)


### readin files Level II

so_coords<-read.csv2("C:/DATA/FSCCdata/FSCDB_SOC_stocks/ESB_outputs/20240324_c_stocks/coordinates_so.csv"); dim(so_coords)
so_strat<-read.csv2("C:/DATA/FSCCdata/FSCDB_SOC_stocks/ESB_outputs/20240513_c_stocks/so_strat.csv"); dim(so_strat)
so_plotSOCs<-read.csv2("C:/DATA/FSCCdata/FSCDB_SOC_stocks/ESB_outputs/20240513_c_stocks/so_plot_c_stocks.csv"); dim(so_plotSOCs)

###----SOM based SOC stocks over time-------------

names(so_plotSOCs)

so_som_plotSOCs<-so_plotSOCs[so_plotSOCs$survey_form=="so_som", ]
View(so_som_plotSOCs)
names(so_som_plotSOCs)

# plotfile with stratifiers
so_som_SOC_change<-distinct(so_som_plotSOCs[, c(1,6,38:54) ])   # distinct plotrows with strata
names(so_som_SOC_change)

DS<-so_som_plotSOCs[ ,c(6,8,12,18,24)]

plotlist<-so_som_SOC_change$plot_id
nplots<-length(plotlist)

OUT<-array(NA,dim=c(nplots,22))
OUTNAMES<-cbind("plot_id","NumSv","SV_yr1","c_stock_yr1","c_stock_bg_yr1","c_stock_ff_yr1","SV_yr2","c_stock_yr2","c_stock_bg_yr2","c_stock_ff_yr2",
                "SV_yr3","c_stock_yr3","c_stock_bg_yr3","c_stock_ff_yr3","SV_yr4","c_stock_yr4","c_stock_bg_yr4","c_stock_ff_yr4","SV_yr5",
                "c_stock_yr5","c_stock_bg_yr5","c_stock_ff_yr5")
for (i in 1:nplots) {
  PLOTSET<-DS[DS$plot_id==plotlist[i], ]
  nSV<-dim(PLOTSET)[1]
  #store in vars
  OUT[i,1]<-plotlist[i]
  OUT[i,2]<-nSV   #max 5
  OUT[i,3]<-PLOTSET[1,2]
  OUT[i,4]<-PLOTSET[1,3]
  OUT[i,5]<-PLOTSET[1,4]
  OUT[i,6]<-PLOTSET[1,5]
  OUT[i,7]<-PLOTSET[2,2]
  OUT[i,8]<-PLOTSET[2,3]
  OUT[i,9]<-PLOTSET[2,4]
  OUT[i,10]<-PLOTSET[2,5]
  OUT[i,11]<-PLOTSET[3,2]
  OUT[i,12]<-PLOTSET[3,3]
  OUT[i,13]<-PLOTSET[3,4]
  OUT[i,14]<-PLOTSET[3,5] 
  OUT[i,15]<-PLOTSET[4,2] 
  OUT[i,16]<-PLOTSET[4,3] 
  OUT[i,17]<-PLOTSET[4,4]
  OUT[i,18]<-PLOTSET[5,5]
  OUT[i,19]<-PLOTSET[6,2]
  OUT[i,20]<-PLOTSET[6,3]
  OUT[i,21]<-PLOTSET[6,4]
  OUT[i,22]<-PLOTSET[6,5]
  }

OUTFILE<-as.data.frame(OUT)
names(OUTFILE)<-OUTNAMES

OUTFILE<-type.convert(OUTFILE,as.is=TRUE)
str(OUTFILE)



#### calculate diffstocks

names(OUTFILE)


## yr2-yr1
## difference in number of years
OUTFILE$diffYR_yr2_min_yr1<-OUTFILE$SV_yr2-OUTFILE$SV_yr1
## difference in SOC stocks
OUTFILE$diffSOC_yr2_min_yr1<-OUTFILE$c_stock_yr2-OUTFILE$c_stock_yr1
OUTFILE$diffSOC_bg_yr2_min_yr1<-OUTFILE$c_stock_bg_yr2-OUTFILE$c_stock_bg_yr1
OUTFILE$diffSOC_ff_yr2_min_yr1<-OUTFILE$c_stock_ff_yr2-OUTFILE$c_stock_ff_yr1
## calculate sequestration rate in t Ca ha-1 yr-1
OUTFILE$SOCseqrate_yr2_min_yr1<-round(OUTFILE$diffSOC_yr2_min_yr1/OUTFILE$diffYR_yr2_min_yr1,2)
OUTFILE$SOCseqrate_bg_yr2_min_yr1<-round(OUTFILE$diffSOC_bg_yr2_min_yr1/OUTFILE$diffYR_yr2_min_yr1,2)
OUTFILE$SOCseqrate_ff_yr2_min_yr1<-round(OUTFILE$diffSOC_ff_yr2_min_yr1/OUTFILE$diffYR_yr2_min_yr1,2)

## yr3-yr2
## difference in number of years
OUTFILE$diffYR_yr3_min_yr2<-OUTFILE$SV_yr3-OUTFILE$SV_yr2
## difference in SOC stocks
OUTFILE$diffSOC_yr3_min_yr2<-OUTFILE$c_stock_yr3-OUTFILE$c_stock_yr2
OUTFILE$diffSOC_bg_yr3_min_yr2<-OUTFILE$c_stock_bg_yr3-OUTFILE$c_stock_bg_yr2
OUTFILE$diffSOC_ff_yr3_min_yr2<-OUTFILE$c_stock_ff_yr3-OUTFILE$c_stock_ff_yr2
## calculate sequestration rate in t Ca ha-1 yr-1
OUTFILE$SOCseqrate_yr3_min_yr2<-round(OUTFILE$diffSOC_yr3_min_yr2/OUTFILE$diffYR_yr3_min_yr2,2)
OUTFILE$SOCseqrate_bg_yr3_min_yr2<-round(OUTFILE$diffSOC_bg_yr3_min_yr2/OUTFILE$diffYR_yr3_min_yr2,2)
OUTFILE$SOCseqrate_ff_yr3_min_yr2<-round(OUTFILE$diffSOC_ff_yr3_min_yr2/OUTFILE$diffYR_yr3_min_yr2,2)


## yr4-yr3
## difference in number of years
OUTFILE$diffYR_yr4_min_yr3<-OUTFILE$SV_yr4-OUTFILE$SV_yr3
## difference in SOC stocks
OUTFILE$diffSOC_yr4_min_yr3<-OUTFILE$c_stock_yr4-OUTFILE$c_stock_yr3
OUTFILE$diffSOC_bg_yr4_min_yr3<-OUTFILE$c_stock_bg_yr4-OUTFILE$c_stock_bg_yr3
OUTFILE$diffSOC_ff_yr4_min_yr3<-OUTFILE$c_stock_ff_yr4-OUTFILE$c_stock_ff_yr3
## calculate sequestration rate in t Ca ha-1 yr-1
OUTFILE$SOCseqrate_yr4_min_yr3<-round(OUTFILE$diffSOC_yr4_min_yr3/OUTFILE$diffYR_yr4_min_yr3,2)
OUTFILE$SOCseqrate_bg_yr4_min_yr3<-round(OUTFILE$diffSOC_bg_yr4_min_yr3/OUTFILE$diffYR_yr4_min_yr3,2)
OUTFILE$SOCseqrate_ff_yr4_min_yr3<-round(OUTFILE$diffSOC_ff_yr4_min_yr3/OUTFILE$diffYR_yr4_min_yr3,2)


## yr5-yr4
## difference in number of years
OUTFILE$diffYR_yr5_min_yr4<-OUTFILE$SV_yr5-OUTFILE$SV_yr4
## difference in SOC stocks
OUTFILE$diffSOC_yr5_min_yr4<-OUTFILE$c_stock_yr5-OUTFILE$c_stock_yr4
OUTFILE$diffSOC_bg_yr5_min_yr4<-OUTFILE$c_stock_bg_yr5-OUTFILE$c_stock_bg_yr4
OUTFILE$diffSOC_ff_yr5_min_yr4<-OUTFILE$c_stock_ff_yr5-OUTFILE$c_stock_ff_yr4
## calculate sequestration rate in t Ca ha-1 yr-1
OUTFILE$SOCseqrate_yr5_min_yr4<-round(OUTFILE$diffSOC_yr5_min_yr4/OUTFILE$diffYR_yr5_min_yr4,2)
OUTFILE$SOCseqrate_bg_yr5_min_yr4<-round(OUTFILE$diffSOC_bg_yr5_min_yr4/OUTFILE$diffYR_yr5_min_yr4,2)
OUTFILE$SOCseqrate_ff_yr5_min_yr4<-round(OUTFILE$diffSOC_ff_yr5_min_yr4/OUTFILE$diffYR_yr5_min_yr4,2)





## Output file
OUTFILE

names(OUTFILE)


## extra seqrates

#calculate average seqrate over all survey periods
OUTFILE$avgSOCseqrate<-round(rowMeans(OUTFILE[,c(27,34,41,48)],na.rm=TRUE),2)
OUTFILE$avgSOCseqrate_bg<-round(rowMeans(OUTFILE[,c(28,35,42,49)],na.rm=TRUE),2)
OUTFILE$avgSOCseqrate_ff<-round(rowMeans(OUTFILE[,c(29,36,43,50)],na.rm=TRUE),2)

# check OUTFILE[,c(2,27,34,41,48,51)]
# check OUTFILE[,c(2,28,35,42,49,52)]
# check OUTFILE[,c(2,29,36,43,50,53)]


##### combine data with stratifiers & save file

som_SOC_evol<-left_join(so_som_SOC_change,OUTFILE, by=c("plot_id"))
View(som_SOC_evol)

write.csv2(som_SOC_evol,"C:/DATA/FSCCdata/Ecological_Study_Book/SOCsequestration/so_som_SOC_sequestration.csv", row.names=FALSE)


############################################################
##### Make plot graphs based on som_SOC_evol dataset

names(som_SOC_evol)


### Graphing function

Graph_Cseqrate<-function(plotID) {

plotDS<-som_SOC_evol[som_SOC_evol$plot_id==PID, c(2,3,20:68)]
names(plotDS)
SV<-as.numeric(plotDS[ ,c(4,8,12,16,20)])
SOCstot<-as.numeric(plotDS[ ,c(5,9,13,17,21)])
SOCsbg<-as.numeric(plotDS[ ,c(6,10,14,18,22)])
SOCsff<-as.numeric(plotDS[ ,c(7,11,15,19,23)])

SQSOCtot<-as.numeric(plotDS[ ,c(28,35,42,49)])   ## difference seq rate
SQSOCbg<-as.numeric(plotDS[ ,c(29,36,43,50)]) 
SQSOCff<-as.numeric(plotDS[ ,c(30,37,44,51)]) 

ymax<-max(SOCstot,na.rm=T)

plot(SV,SOCstot,pch=16,main=paste0("LII Plot: ",plotDS$plot_id," // nSurv: ", plotDS$NumSv), 
     xlab="Survey year", ylab="SOC stock (t C/ha)", ylim=c(0,ymax))
lines(SV,SOCstot, col="black", lwd=2)
lines(SV,SOCsbg, col="brown", lwd=2)
lines(SV,SOCsff, col="orange", lwd=2)

midYRs<-c(mean(c(plotDS$SV_yr1,plotDS$SV_yr2),na.rm=F),mean(c(plotDS$SV_yr2,plotDS$SV_yr3),na.rm=F),
          mean(c(plotDS$SV_yr3,plotDS$SV_yr4),na.rm=F),mean(c(plotDS$SV_yr4,plotDS$SV_yr5),na.rm=F))
midSOCstot<-c(mean(c(plotDS$c_stock_yr1,plotDS$c_stock_yr2),na.rm=F),mean(c(plotDS$c_stock_yr2,plotDS$c_stock_yr3),na.rm=F),
           mean(c(plotDS$c_stock_yr3,plotDS$c_stock_yr4),na.rm=F),mean(c(plotDS$c_stock_yr4,plotDS$c_stock_yr5),na.rm=F))
midSOCsbg<-c(mean(c(plotDS$c_stock_bg_yr1,plotDS$c_stock_bg_yr2),na.rm=F),mean(c(plotDS$c_stock_bg_yr2,plotDS$c_stock_bg_yr3),na.rm=F),
              mean(c(plotDS$c_stock_bg_yr3,plotDS$c_stock_bg_yr4),na.rm=F),mean(c(plotDS$c_stock_bg_yr4,plotDS$c_stock_bg_yr5),na.rm=F))
midSOCsff<-c(mean(c(plotDS$c_stock_ff_yr1,plotDS$c_stock_ff_yr2),na.rm=F),mean(c(plotDS$c_stock_ff_yr2,plotDS$c_stock_ff_yr3),na.rm=F),
              mean(c(plotDS$c_stock_ff_yr3,plotDS$c_stock_ff_yr4),na.rm=F),mean(c(plotDS$c_stock_ff_yr4,plotDS$c_stock_ff_yr5),na.rm=F))


#plot the seqrates on graph
text(midYRs,midSOCstot*0.9,SQSOCtot,cex=1.2,col="black") 
text(midYRs,midSOCsbg*0.9,SQSOCbg,cex=1.2,col="brown") 
text(midYRs,midSOCsff*0.9,SQSOCff,cex=1.2,col="orange") 

}


## make graphs of all Cstocks and changes
par(mfrow=c(3,4))

### select only plots with at least 2 surveys
PIDlist<-unique(som_SOC_evol$plot_id[som_SOC_evol$NumSv>=2])
numplots<-length(PIDlist)

#numplots
### print all plots
for (i in 1:10) {
    PID<-PIDlist[i]
    Graph_Cseqrate(PID)
}





### print plots by country
PIDlist

#Austria
par(mfrow=c(3,7))
for (i in 1:19) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}

#Belgium
par(mfrow=c(3,6))
for (i in 20:37) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}

#63 Bulgaria
par(mfrow=c(3,6))
for (i in 38:40) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}

#58 Czech Rep
par(mfrow=c(3,6))
for (i in 41:52) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}

#4 Germany
par(mfrow=c(3,6))
for (i in 53:106) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#8 Denmark
par(mfrow=c(3,6))
for (i in 107:108) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}

#59 Estonia
par(mfrow=c(3,6))
for (i in 109:113) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#15 Finland
par(mfrow=c(3,6))
for (i in 114:144) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#1 France
par(mfrow=c(3,6))
for (i in 145:243) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#9 Greece
par(mfrow=c(3,6))
for (i in 244:247) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#51 Hungary
par(mfrow=c(3,6))
for (i in 248:248) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}



#51 Hungary
par(mfrow=c(3,6))
for (i in 248:248) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}

#7 Ireland
par(mfrow=c(3,6))
for (i in 249:249) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#5 Italy
par(mfrow=c(3,6))
for (i in 250:258) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}



#3 Netherlands
par(mfrow=c(3,6))
for (i in 259:269) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}



#53 Poland
par(mfrow=c(3,6))
for (i in 270:395) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}




#52 Romania
par(mfrow=c(3,6))
for (i in 396:407) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#67 Serbia
par(mfrow=c(3,6))
for (i in 408:408) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}



#54 Slovakia
par(mfrow=c(3,6))
for (i in 409:414) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}


#11 Spain
par(mfrow=c(3,6))
for (i in 415:424) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}



#6 UK
par(mfrow=c(3,6))
for (i in 424:431) {
  PID<-PIDlist[i]
  Graph_Cseqrate(PID)
}




#################################
## bootstrap sequestration rates.

par(mfrow=c(1,1))
names(som_SOC_evol)


### add variable last survey after 2000yr

som_SOC_evol$lastSVafter2000<-ifelse(som_SOC_evol$SV_yr5>=2000,TRUE,FALSE)
som_SOC_evol$lastSVafter2000<-ifelse(som_SOC_evol$SV_yr4>=2000,TRUE,som_SOC_evol$lastSVafter2000)
som_SOC_evol$lastSVafter2000<-ifelse(som_SOC_evol$SV_yr3>=2000,TRUE,som_SOC_evol$lastSVafter2000)
som_SOC_evol$lastSVafter2000<-ifelse(som_SOC_evol$SV_yr2>=2000,TRUE,som_SOC_evol$lastSVafter2000)

summary(som_SOC_evol$lastSVafter2000)



x<-som_SOC_evol$avgSOCseqrate_ff[som_SOC_evol$lastSVafter2000==TRUE] 
x<-na.omit(x)
length(x)

#png("c:/R/OUT/histtest.png")
hist(x,nclass=100, prob=TRUE)
lines(density(x), col="red", lwd=2)
#dev.off()

# define function
theta <- function(x){mean(x, na.rm=TRUE)} 
med <- function(x){median(x,na.rm=TRUE)} 

# bootstrap B=1000
results <- bootstrap(x,5000,theta)
outcome<-results$thetastar
hist(outcome, nclass=100, prob=TRUE, col="peachpuff", 
     border="black",main="Sequestration rate (t C ha-1 yr-1)")
abline(v=mean(outcome), col="blue", lwd=2)



length(x)
##### Non parametric BCa CI
BCa<-bcanon(x, 5000, theta, alpha=c(0.025,0.5,0.975))
## estimated BCA conf limits
BCa$confpoints
CIpoints<-BCa$confpoints[,2]
abline(v=CIpoints, col=c("red","red","red"))
## estimated acceleration constant
BCa$acc
##### Non parametric BCa CI
BCa<-bcanon(x, 5000, med, alpha=c(0.025,0.5,0.975))
## estimated BCA conf limits
BCa$confpoints
## estimated acceleration constant
BCa$acc
results <- bootstrap(x,5000,med)
quantile(results$thetastar,probs=c(0.025,0.5,0.975))








#### compare density curves before and after year 2000
library(sm)


names(so_som_plotSOCs)

datset<-so_som_plotSOCs[, c(6,8,12,15,18,24)]
names(datset)
datset$bf2000<-ifelse(datset$survey_year<2000,1,2)

# create value labels
fac.f <- factor(datset$bf2000, levels= c(1,2),
                labels = c("Before 2000", "After 2000"))


# plot densities
sm.density.compare(as.numeric(datset$c_stock),datset$bf2000, xlab="SOC stock", lwd=2)
title(main="SOC distribution before and after year 2000")

# add legend via mouse click
colfill<-c(2:(2+length(levels(fac.f))))
legend(locator(1), levels(fac.f), fill=colfill)




#####################################################
## Bootstrapped SOC stocks by biogegraphical region


so_plotSOCs
###----SOM based SOC stocks over time-------------
so_som_plotSOCs<-so_plotSOCs[so_plotSOCs$survey_form=="so_som", ]
View(so_som_plotSOCs)
names(so_som_plotSOCs)
str(so_som_plotSOCs)

par(mfrow=c(1,1))

unique(so_som_plotSOCs$biogeographical_region)

x<-as.numeric(so_som_plotSOCs$c_stock[so_som_plotSOCs$biogeographical_region=="Pannonian"])
y<-as.numeric(so_som_plotSOCs$c_stock[so_som_plotSOCs$biogeographical_region=="Pannonian"&so_som_plotSOCs$survey_year<2000])
z<-as.numeric(so_som_plotSOCs$c_stock[so_som_plotSOCs$biogeographical_region=="Pannonian"&so_som_plotSOCs$survey_year>=2000])


x<-as.numeric(so_som_plotSOCs$c_stock)
y<-as.numeric(so_som_plotSOCs$c_stock[so_som_plotSOCs$survey_year<2000])
z<-as.numeric(so_som_plotSOCs$c_stock[so_som_plotSOCs$survey_year>=2000])



x<-na.omit(x)
y<-na.omit(y)
z<-na.omit(z)

#png("c:/R/OUT/histtest.png")
hist(x,nclass=100, prob=TRUE, main="Distribution total SOC stocks across Europe",xlab="SOC stock (t C/ha) in forest floor and mineral soil")
lines(density(x), col="red", lwd=2)
# show outliers
outliers<-so_som_plotSOCs[x>1000,c("c_stock","plot_id")]
text(outliers$c_stock,0.0005,outliers$plot_id, cex=0.9)
outliers
#dev.off()

# define function
theta <- function(x){mean(x)} 
med <- function(x){median(x)} 

# bootstrap B=1000
results <- bootstrap(x,5000,theta)
outcome<-results$thetastar
hist(outcome, nclass=100, prob=TRUE, col="peachpuff", 
     border="black",main="Distribution")
abline(v=mean(outcome), col="blue", lwd=2)


## x var
length(x)
##### Non parametric BCa CI
BCa<-bcanon(x, 5000, theta, alpha=c(0.025,0.5,0.975))
## estimated BCA conf limits
BCa$confpoints
CIpoints<-BCa$confpoints[,2]
abline(v=CIpoints, col=c("red","red","red"))
## estimated acceleration constant
BCa$acc

## y var
length(y)
##### Non parametric BCa CI
BCa<-bcanon(y, 5000, theta, alpha=c(0.025,0.5,0.975))
## estimated BCA conf limits
BCa$confpoints
CIpoints<-BCa$confpoints[,2]
abline(v=CIpoints, col=c("red","red","red"))
## estimated acceleration constant
BCa$acc


## z var
length(z)
##### Non parametric BCa CI
BCa<-bcanon(z, 5000, theta, alpha=c(0.025,0.5,0.975))
## estimated BCA conf limits
BCa$confpoints
CIpoints<-BCa$confpoints[,2]
abline(v=CIpoints, col=c("red","red","red"))
## estimated acceleration constant
BCa$acc




### violin plot
library(rempsyc)
library(ggplot2)

names(so_som_plotSOCs)
so_som_plotSOCs$log_c_stock<-log10(as.numeric(so_som_plotSOCs$c_stock))

## make period pre2K, post2K (2K = year 2000)
so_som_plotSOCs$period<-ifelse(so_som_plotSOCs$survey_year<2000,"pre2K","post2K")

unique(so_som_plotSOCs$period)

plotSOC<-nice_violin(
  data = so_som_plotSOCs,
  group = "period",
  #group="survey_year",
  response = "log_c_stock",
  ytitle = "Log10 of Total SOC stock (t C/ha)",
  comp1 = "pre2K",
  comp2 = "post2K",
  has.d = FALSE,      # add Cohen's d effect size
  #obs=TRUE,
  xlabels = c("After 2000", "Before 2000"),
  #boot = TRUE,
  #bootstraps = 2000,
  
)

plotSOC



















##--------------------------

