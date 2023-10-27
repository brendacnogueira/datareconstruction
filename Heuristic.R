source("Functions.R")
load("ds_feature.RData")


heuristic<-full_window_heuristic(ds_feature)
save(heuristic,file="heuristic.RData")
heuristic_CM<-table(heuristic$metier,heuristic$predicted)
acc_per_metier<-acc_per_metier(heuristic_CM)

