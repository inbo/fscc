#### Using Mass preserving spline / Equal area spline function of soil layers/horizons
#### Calculation for carbon stocks at 30 and 100 cm of depth
#### INPUT FILE = so_som
#### Bruno De Vos - adapted 19/07/2023 - based on earlier developed scripts


setwd("C:/R_scripts/_GIT_REPO/fscc")

#### Work folders (on local drive) ####
# scripts in "./scr/stock_calculations/"
# input data in "./data/so_20230718/so"
# output data in "./output/stocks/so"


### CARBON PROFILES GENERATOR via mpsplines
### RUN SOILSPLINE function first !!!!
source("./src/stock_calculations/functions/SOILSPLINE_function_v3.R")
source("./src/stock_calculations/functions/Csmpspline_function.R")

## profile is input based on Carbon density (CD) data 

#====================================#
#### Calculation for so dataset  ####
#### Results in SOILLAYCs.min.csv
#====================================#

## CALC Cs1PROF.LI 
## => no repetitions anymore due to aggregating per plot
## => each layer is unique in profile

###
## Loading datasets from CSV files  ####
###

# read in master dataset
so.som<-read.csv2("./data/so_20230718/so/so_som.csv", header=TRUE); dim(so.som)
names(so.som)

# add plot_ID based on partner_code and code_plot
so.som$plot_ID<-paste0(so.som$partner_code,"_",so.som$code_plot)

# add profile_ID based on concatenation survey_year, plot_ID and repetition
so.som$profile_ID<-paste0(so.som$survey_year,"_",so.som$plot_ID,"_",so.som$repetition)
names(so.som)

# recode helping variables for stock calculation

so.som$TOP<-so.som$layer_limit_superior
so.som$BOT<-so.som$layer_limit_inferior
so.som$AVD<-(so.som$TOP+so.som$BOT)/2    # calculate average depth (cm)
so.som$LAYTHICK<-(so.som$BOT-so.som$TOP) # calculate layer thickness (cm)
names(so.som)


# derive layer based Cs working dataset for mineral soils and forest floors
SOILLAYCs<-so.som[so.som$layer_type=="mineral" ,c(4,96,7,8,77,18,20,19,97,98,99,100,86,23,101) ]; dim(SOILLAYCs)
# for forest floors
SOILLAYCs.FF<-so.som[so.som$layer_type=="forest_floor" ,c(4,96,7,8,77,18,20,19,97,98,99,100,86,23,101) ]; dim(SOILLAYCs.FF)

### calculate CD and LAYSOCs
str(SOILLAYCs)
SOILLAYCs$VPCF<-(SOILLAYCs$coarse_fragment_vol/100)   # calculate volume proportion of coarse fractions 
names(SOILLAYCs)

### calculate CD carbon density and LAYSOCs
SOILLAYCs$CD<- (SOILLAYCs$organic_carbon_total * SOILLAYCs$bulk_density * (1-SOILLAYCs$VPCF))/10000    ## compute mean carbon density per layer  (t C/ha.cm) 
SOILLAYCs$LAYSOCs<-(SOILLAYCs$CD * SOILLAYCs$LAYTHICK)                                                 ## compute SOC stock (ton C/ha) for each layer
                  
### add data availability index
SOILLAYCs$av.THICK<-ifelse(is.na(SOILLAYCs$LAYTHICK),0,1)
SOILLAYCs$av.TOC<-ifelse(is.na(SOILLAYCs$organic_carbon_total),0,1)
SOILLAYCs$av.BD<-ifelse(is.na(SOILLAYCs$bulk_density),0,1)
SOILLAYCs$av.CF<-ifelse(is.na(SOILLAYCs$coarse_fragment_vol),0,1)

View(SOILLAYCs)


### Save the SOILLAYCs dataset (all data, also missing data)

write.csv2(SOILLAYCs,"./output/stocks/so/SOILLAYCs.min.csv", row.names = FALSE)



#### Mineral profile SOC calculation routine  ####
#### ONLY with AVAILABLE CD data <-> imputed data

SOILLAYCs<-SOILLAYCs[!is.na(SOILLAYCs$CD), ]
dim(SOILLAYCs)
View(SOILLAYCs)



#View(SOILLAYCs)

names(SOILLAYCs)
str(SOILLAYCs)


pv<-NULL
CsPROFmps<-NULL
proflist<-unique(SOILLAYCs$profile_ID)
nprof<-length(proflist)
nprof

tail(proflist)


for (i in 1:nprof) {
#for (i in 1:500) {

## omit error generating profiles
 # if (proflist[i]=="1995_7_33_1") next   # omit plot due to CD values only 0-5 cm

  
  pv<-SOILLAYCs[SOILLAYCs$profile_ID==proflist[i], ]

	# ordering/sorting matrix by depth or HSEQ
  pv<-pv[order(pv$layer_number), ] 
  pv
	
  # if CD are all NA => next
  # if (is.na(min(unique(pv$CD)))) next #omit this plot when first Cd value is NA 

  ## input for Csmpspline function
 	  PROF<-pv[,c("plot_ID","profile_ID","code_layer","TOP","BOT","CD")]  	
		
 	## variables for calculation output  
 	  PROFID<-unique(PROF$profile_ID)
		PLOTID<-as.character(unique(PROF$plot_ID))

		# OBSERVATION DEPTH
		OBSDEPTH<-max(PROF$BOT[!is.na(PROF$CD)])
		
		# set DEPTHSTOCK
		SOILDEPTH<-100
		
		
		### PROFSTOCK: apply SOILSPLINE function on PROF
	  out2<-Csmpspline(PROF)
		lineout<-data.frame(PLOTID,PROFID,OBSDEPTH,SOILDEPTH,out2)
		CsPROFmps<-rbind(CsPROFmps,lineout)	
	next

	}

###
### Resulting datafiles ####
###

CsPROFmps;dim(CsPROFmps)
View(CsPROFmps)

#### WRITE output

write.csv(CsPROFmps,file="./output/stocks/so/so.som.CsPROFmps.csv")

## end of routine


## Check profiles with Cs10 stocks (0-10 cm SOC stock) equal0 

Zerolist<-CsPROFmps[CsPROFmps$PCsmps10==0, ]
# list of plots with Cs10==0
unique(Zerolist$PLOTID)    # 40 plots


SOILLAYCs[SOILLAYCs$plot_ID=="1_51", ]    # => just 1 code_layer=> M24
SOILLAYCs[SOILLAYCs$plot_ID=="1_55", ]    # => just 1 code_layer=> M01
SOILLAYCs[SOILLAYCs$plot_ID=="55_15", ]    # => 1 rep until 40 cm, 2 M12 layer only




dim(CsPROFmps)
CsPROFmps2<-CsPROFmps[CsPROFmps$PCsmps10>0 , ]
dim(CsPROFmps2)

length(unique(CsPROFmps2$PLOTID))   #258 plots
length(unique(CsPROFmps2$PROFID))   ##1158 profiles



#=============================================#
#### Forest floor SOC calculation routine  ####
#### Results in SOILLAYCs.FF.csv           ####
#=============================================#

names(SOILLAYCs.FF)
head(SOILLAYCs.FF)

unique(SOILLAYCs.FF$code_layer)

# recode inconsistencies

SOILLAYCs.FF$code_layer<-ifelse(SOILLAYCs.FF$code_layer=="OL74","OL",SOILLAYCs.FF$code_layer)   # in plot 54_208
SOILLAYCs.FF$code_layer<-ifelse(SOILLAYCs.FF$code_layer=="OL31","OL",SOILLAYCs.FF$code_layer) 


### calculate CD carbon density and LAYSOCs
SOILLAYCs.FF$LAYSOCs<-(SOILLAYCs.FF$organic_carbon_total* SOILLAYCs.FF$organic_layer_weight)/100          ## compute SOC stock (ton C/ha) for each layer
SOILLAYCs.FF$CD<-(SOILLAYCs.FF$LAYSOCs/SOILLAYCs.FF$LAYTHICK)                                            ## compute mean carbon density per layer  (t C/ha.cm) 
                                               

### add data availability logical
SOILLAYCs.FF$av.THICK<-ifelse(is.na(SOILLAYCs.FF$LAYTHICK),0,1)
SOILLAYCs.FF$av.TOC<-ifelse(is.na(SOILLAYCs.FF$organic_carbon_total),0,1)
SOILLAYCs.FF$av.BD<-ifelse(is.na(SOILLAYCs.FF$bulk_density),0,1)
SOILLAYCs.FF$av.OLM<-ifelse(is.na(SOILLAYCs.FF$organic_layer_weight),0,1)

View(SOILLAYCs.FF)


### Save the SOILLAYCs dataset (all data, also missing data)

write.csv2(SOILLAYCs.FF,"./output/stocks/so/SOILLAYCs.FF.csv", row.names = FALSE)



#### Forest floor SOC calculation routine  ####
#### Combined Stocks in OL, OFH and OLFH per profile  
dim(SOILLAYCs.FF)
### only with non-imputed data
SOILLAYCs.FF<-SOILLAYCs.FF[!is.na(SOILLAYCs.FF$LAYSOCs), ]
dim(SOILLAYCs.FF)

length(unique(SOILLAYCs.FF$plot_ID))      #590 plots
length(unique(SOILLAYCs.FF$profile_ID))   #1993 profiles


names(SOILLAYCs.FF)
str(SOILLAYCs.FF)


FF<-NULL
CsFF<-NULL
proflist<-unique(SOILLAYCs.FF$profile_ID)
nprof<-length(proflist)
nprof


for (i in 1:nprof) {

  FF<-SOILLAYCs.FF[SOILLAYCs.FF$profile_ID==proflist[i], ]
  
  # ordering/sorting matrix by depth or HSEQ
  FF<-FF[order(FF$layer_number), ] 
  FF    ## single FF profile
  
  ## Carbon stock additions
  OL.Cs<-ifelse("OL"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="OL"],0)
  OLF.Cs<-ifelse("OLF"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="OLF"],0)
  OFH.Cs<-ifelse("OFH"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="OFH"],0)
  LFH.Cs<-ifelse("OL"%in%FF$code_layer&"OF"%in%FF$code_layer&"OH"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="OL"]+FF$LAYSOCs[FF$code_layer=="OF"]+FF$LAYSOCs[FF$code_layer=="OH"],0)
  OFH.Cs<-ifelse("OF"%in%FF$code_layer&"OH"%in%FF$code_layer, FF$LAYSOCs[FF$code_layer=="OF"]+FF$LAYSOCs[FF$code_layer=="OH"],OFH.Cs )
  LFH.Cs<-ifelse("OLF"%in%FF$code_layer&"OH"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="OLF"]+FF$LAYSOCs[FF$code_layer=="OH"],LFH.Cs)
  LFH.Cs<-ifelse("OL"%in%FF$code_layer&"OFH"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="OL"]+FF$LAYSOCs[FF$code_layer=="OFH"],LFH.Cs)
  O.Cs<-sum(FF$LAYSOCs[FF$code_layer=="O1"],FF$LAYSOCs[FF$code_layer=="O2"],FF$LAYSOCs[FF$code_layer=="O3"],na.rm=T)
  O.Cs<-ifelse("O"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="O"],O.Cs)
  OLF.Cs<-ifelse("OL"%in%FF$code_layer&"OF"%in%FF$code_layer,FF$LAYSOCs[FF$code_layer=="OL"]+FF$LAYSOCs[FF$code_layer=="OF"],OLF.Cs)
  LFH.Cs<-ifelse(O.Cs>0,O.Cs,LFH.Cs)
    
  ## variables for calculation output  
  PLOTID<-as.character(unique(FF$plot_ID))
  PROFID<-as.character(unique(FF$profile_ID))
  FFLAYERS<-paste(c(FF$code_layer),collapse = ",")
  FFnLAY<-length(FF$layer_number)
  
  # total FF thickness
  FFTHICK<-sum(FF$LAYTHICK)

  ### PROFSTOCK: apply SOILSPLINE function on PROF
  lineout<-cbind(PLOTID,PROFID,FFLAYERS,FFnLAY,FFTHICK,O.Cs,OL.Cs,OLF.Cs,OFH.Cs,LFH.Cs)
  CsFF<-rbind(CsFF,lineout)	
  print(paste("Processed Profile", i, "out of",nprof,"PROF ID=",PROFID))
  next

}

View(CsFF)




write.csv(CsFF,file="./output/stocks/so/so.som.CsFF.csv", row.names = FALSE)

