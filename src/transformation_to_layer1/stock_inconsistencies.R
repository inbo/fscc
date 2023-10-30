#### so_som inconsistencies
 

 
 #######
 ## Data checks
 
 ## do we have effective soil depths for all plots ? 
 
 unique(CsPROFmps$PLOTID)   # 281 plots with stocks
 
 Plots_Missing_soildepths<- CsPROFmps$PLOTID[CsPROFmps$PLOTID %in% so_soildepth$PLOT_ID==FALSE]
 length(Plots_Missing_soildepths)
 
 View(so_prf_adds)
 
 
 
 ##### correcties aan so.som file 
 
 so.som<-read.csv2("./data/so_20230915/so/so_som.csv", header=TRUE); dim(so.som)
 names(so.som)
 
 
 
 
 

 
VW<-so.som[so.som$code_country=="11", c(5,4,80,7,8,93,20,73)]
View(VW)

str(so.som) 

so.som[so.som$survey_year==1996 & so.som$plot_id=="54_212" & so.som$code_layer=="OL", c(so.som$organic_layer_weight)]
 
so.som$organic_layer_weight[so.som$organic_layer_weight>200]
 
 
 


#### code_line based corrections for inconsistencies


#---- codelayer errors
so.som$code_layer[so.som$code_line=="SOSK2007-540054-000210"]<-"OL"
so.som$code_layer[so.som$code_line=="SOSK2007-540054-000157"]<-"OL"


#---- wrong organic_layer_weight 
so.som$organic_layer_weight[so.som$plot_id=="54_212" & so.som$code_layer=="OL"]<-(so.som$organic_layer_weight[so.som$plot_id=="54_212" & so.som$code_layer=="OL"])/1000
so.som$organic_layer_weight[so.som$plot_id=="54_212" & so.som$code_layer=="OF"]<-(so.som$organic_layer_weight[so.som$plot_id=="54_212" & so.som$code_layer=="OF"])/1000

so.som$organic_layer_weight[so.som$plot_id=="54_211" & so.som$code_layer=="OL"]<-(so.som$organic_layer_weight[so.som$plot_id=="54_211" & so.som$code_layer=="OL"])/1000
so.som$organic_layer_weight[so.som$plot_id=="54_211" & so.som$code_layer=="OF"]<-(so.som$organic_layer_weight[so.som$plot_id=="54_211" & so.som$code_layer=="OF"])/1000

names(so.som)


# visual screening
plot(so.som$layer_thickness,so.som$organic_layer_weight)
text(so.som$layer_thickness,so.som$organic_layer_weight+1,so.som$plot_id, cex=0.6, col="blue")
abline(0,1, col="red")







