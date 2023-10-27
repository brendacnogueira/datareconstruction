source("Functions.R")


descRandom_80_21<-read.csv("DescsRandom_80_21.csv", na.strings = "NULL",sep = ';')
#sum(is.na(descRandom_80_21$metier))

ds<-descRandom_80_21
#### ANALISE INICIAL
m<-ds[is.na(ds$compff),"matricula"]
m<-unique(m)
n<-unique(ds[!is.na(ds$compff),"matricula"])
aux<-ds[(ds$matricula  %in% m) & !(ds$matricula  %in% n),c("iddescarga","dia","mes","ano","metier","matricula","classe_com","compff","compsl","bocasl")]
aux<-aux[order(aux$matricula),]
list_aux<-aux %>% group_by(matricula,metier,compff,compsl,bocasl)%>% summarise( .groups = 'drop')


p<-ds %>% group_by(dia,mes,ano,euros)%>% summarise( .groups = 'drop')
library(dplyr)


c<-ds[is.na(ds$classificacao),"iddescarga"]
b<-c<-ds[!is.na(ds$classificacao),"iddescarga"]
c<-unique(c)
b<-unique(b)
caux<-ds[(ds$iddescarga  %in% c),c("iddescarga","dia","mes","ano", "classificacao","metier","matricula","compff","compsl","bocasl")]
caux<-caux[order(caux$iddescarga),]
list_caux<-caux %>% group_by(iddescarga,metier,classificacao,compff)%>% summarise( .groups = 'drop')

##temos casos sem classificacao e sem compff mas ignoramos

###prepared data (By descarga)

#ds_80_21_final<-data_prep(descRandom_80_21)

#save(ds_80_21_final,file="ds_80_21_final.RData")

## Add features
load("ds_80_21_final.RData")
n_distinct(ds_80_21_final$iddescarga)

#ds_80_21_feature<-feature_add(ds_80_21_final)

#save(ds_80_21_feature,file="ds_80_21_feature.RData")


##### Apply models
load("ds_80_21_feature.RData")
train<-ds_80_21_feature[ds_80_21_feature$metier!="VAZIO",] ###121,391 linhas
validation<-train

results_sliding_80_21<-strategies_window(train,validation,"sliding")
results_growing_80_21<-strategies_window(train,validation,"growing")
results_full_80_21<-strategies_window(train,validation,"full")

save(results_sliding_80_21,file="results_sliding_80_21.RData")
save(results_growing_80_21,file="results_growing_80_21.RData")
save(results_full_80_21,file="results_full_80_21.RData")

######## Evaluation

load("results_sliding_80_21.RData")
load("results_growing_80_21.RData")
load("results_full_80_21.RData")

list_metiers<-levels(ds_80_21_feature$metier)
columns = c("id","name","dob") 
evaluation= data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns
for (i in 1:length(list_metiers)) {   
  plot_lines(results_sliding_80_21,results_growing_80_21,results_full_80_21,list_metiers[i],"D:/OKEANOS/plots/FinalDataSet/")
}


evaluation_sliding_80_21<-evaluation(results_sliding_80_21,list_metiers)
evaluation_growing_80_21<-evaluation(results_growing,list_metiers)
evaluation_full_80_21<-evaluation(results_full_80_21,list_metiers)

