### s1_som from Global ENV characteristics

names(s1_som)

## all unique plot_ids
length(unique(s1_som$plot_id))

length(unique(s1_som$plot_id[s1_som$survey_year<2000]))   # n=5203
length(unique(s1_som$plot_id[s1_som$survey_year>=2000]))  # n=5098




## count number of plots before and after year 2000 => repetitions over time


list_plot_ids<-unique(s1_som$plot_id)
n_plot_ids<-length(list_plot_ids)
OUT<-NULL

for (i in 1:n_plot_ids) {
  prof<-s1_som[s1_som$plot_id==list_plot_ids[i], c(71,8,4)]
  surveyed_before_2000<-ifelse(min(unique(prof$survey_year))<2000,1,0)
  surveyed_after_2000<-ifelse(max(unique(prof$survey_year))>=2000,1,0)
  plot_id<-list_plot_ids[i]
  outrow<-cbind(plot_id,surveyed_before_2000,surveyed_after_2000)
  OUT<-rbind(OUT,outrow)
}

### plots surveyed before and after year 2000
s1_plot_resurvey<-data.frame(OUT)
s1_plot_resurvey$resurveyed<-ifelse(s1_plot_resurvey$surveyed_before_2000==1 & s1_plot_resurvey$surveyed_after_2000==1,1,0)
s1_plot_resurvey

length(s1_plot_resurvey$plot_id[s1_plot_resurvey$resurveyed==1])   ### 2210 plots resurveyed

# resurveyed level I plots
resuveyed_s1_plots<-s1_plot_resurvey$plot_id[s1_plot_resurvey$resurveyed==1]



## overview available data 
# number of layers in plot_id - survey_year matrix
tapply(s1_som$code_layer,list(s1_som$plot_id,s1_som$survey_year),length)



s1_som[s1_som$plot_id=="2_12", c(3,4,71,7,8,18:20,23)]

s1_som[s1_som$plot_id=="1_1563", c(3,4,71,7,8,18:20,23)]