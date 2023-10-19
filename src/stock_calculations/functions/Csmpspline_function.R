### Function calculating 10 cm stocks with SOILSPLINE function for each profile
###
### FUNCTIONS BASED ON SELECTED PROFILE ####
### Bruno De Vos - based on earlier function


Csmpspline<-function(PROF) {
  
  PLOTNAME<-unique(PROF$profile_ID)
  LTOP<-PROF$TOP
  LBOT<-PROF$BOT
  LCd<-PROF$CD
  nlay<-length(PROF$CD)
  maxBOT<-100
  proflayCs<-array(0,120)
  
  
  # fit a spline if at least 2 horizons:
  if  (length(LCd)>=2) {
    proflayCs<-SOILSPLINE(PLOTNAME,LTOP,LBOT,LCd,msd=100,graph=TRUE)
      }
  
  #### Bereken cummulatieve Cs per cm (i) tot maxBOT
  #### proflayCs[i] geeft Cs voor diepte i cm
  
  PCsmps10<-ifelse(10<=maxBOT,round(sum(proflayCs[1:10]),2),as.numeric(NA))
  PCsmps20<-ifelse(20<=maxBOT,round(sum(proflayCs[1:20]),2),as.numeric(NA))
  PCsmps30<-ifelse(30<=maxBOT,round(sum(proflayCs[1:30]),2),as.numeric(NA))
  PCsmps40<-ifelse(40<=maxBOT,round(sum(proflayCs[1:40]),2),as.numeric(NA))
  PCsmps50<-ifelse(50<=maxBOT,round(sum(proflayCs[1:50]),2),as.numeric(NA))
  PCsmps60<-ifelse(60<=maxBOT,round(sum(proflayCs[1:60]),2),as.numeric(NA))
  PCsmps70<-ifelse(70<=maxBOT,round(sum(proflayCs[1:70]),2),as.numeric(NA))
  PCsmps80<-ifelse(80<=maxBOT,round(sum(proflayCs[1:80]),2),as.numeric(NA))
  PCsmps90<-ifelse(90<=maxBOT,round(sum(proflayCs[1:90]),2),as.numeric(NA))
  PCsmps100<-ifelse(100<=maxBOT,round(sum(proflayCs[1:100]),2),as.numeric(NA))
  SOCs<-ifelse(maxBOT>1,round(sum(proflayCs[1:maxBOT]),2),as.numeric(NA))
  A<-cbind(nlay,PCsmps10,PCsmps20,PCsmps30,PCsmps40,PCsmps50,PCsmps60,PCsmps70,PCsmps80,PCsmps90,PCsmps100,SOCs)
  
  return(A)
}

