library(purrr)
library(rpart)
library(xgboost)
load("ds_feature.RData")


###########
source("Boosting_better_perfomance.R")

#create_resampling()
#sort(table(ds_feature$metier))

## 3 maiores classes : LLS-PD  LHP-CEF   LHP-PB


#gs.ENN <- as.data.frame(list(Strategy="rs.ENN",k=c(1,3,5)) %>% cross_df())
gs.RU <- as.data.frame(list(Strategy="rs.RandUnder",und.perc=seq(0.1,0.9,by=0.1)) %>% cross_df())
gs.RO <- as.data.frame(list(Strategy="rs.RandOver",ove.perc=c(1,2,3,4)) %>% cross_df())
gs.IS <- as.data.frame(list(Strategy="rs.ImpSamp",und.perc=seq(0.1,0.9,by=0.1),ove.perc=c(seq(0.25,1,by=0.25),2,3,4)) %>% cross_df()); gs.IS <- gs.IS[-10,]
gs.SM <- as.data.frame(list(Strategy="rs.SMOTE",und.perc=seq(0.1,0.9,by=0.1),ove.perc=c(seq(0.25,1,by=0.25),2,3,4)) %>% cross_df()); gs.SM <- gs.SM[-10,]

##########
gridlist <- list(gs.RU,gs.RO,gs.IS,gs.SM)

for(xi in 1:length(gridlist)) {
  
  x <- gridlist[[xi]]
  
  for(i in 1:nrow(x)){
    Boosting_full_best_parameter(x[i,])
  }

}

##########

source("resampling_random.R")

#create_resampling_random()


## 3 maiores classes : LLS-PD  LHP-CEF   LHP-PB
combination<-function(s,m){
  c<-combn(s,m)
  lst<-list()
  for(i in 1:ncol(c)){
    p<-Permn(c[,i])
    for(j in 1:nrow(p)){
      lst<-append(lst,list(p[j,]))
    }
    
  }
  r<-sample(1:length(lst),10)
  lst_final<-list()
  for(i in r){
    lst_final<-append(lst_final,lst[i])
  }
  lst_final
}

und.list<-combination(seq(0.1,0.9,by=0.1),3)
ove.list<-combination(c(seq(0.25,1,by=0.25),2,3,4),4)


#gs.ENN <- as.data.frame(list(Strategy="rs.ENN",k=c(1,3,5)) %>% cross_df())
gs.RU <- as.data.frame(list(Strategy="rs.RandUnder",und.perc=TRUE,ove.perc=FALSE))
gs.RO <- as.data.frame(list(Strategy="rs.RandOver",und.perc=FALSE,ove.perc=TRUE))
gs.IS <- as.data.frame(list(Strategy="rs.ImpSamp",und.perc=TRUE,ove.perc=TRUE))
gs.SM <- as.data.frame(list(Strategy="rs.SMOTE",und.perc=TRUE,ove.perc=TRUE))

##########
gridlist <- list(gs.SM)

for(xi in 1:length(gridlist)) {
  
  x <- gridlist[[xi]]
  if(x$und.perc & x$ove.perc){
    for(u in und.list[-c(1,2)]){
      
      for(o in ove.list){
        o_plus_one<-o+1
        Boosting_full_best_parameter_random(x$Strategy,u, o_plus_one)
      }
    }
  }else if(x$und.perc){
    for(u in und.list){
     
        Boosting_full_best_parameter_random(x$Strategy,u,NULL)
    }
  }else if(x$ove.perc){
    for(o in ove.list){
      o_plus_one<-o+1
      Boosting_full_best_parameter_random(x$Strategy,NULL,o_plus_one)
    }
  }
  
 
  
  
}






###############
source("resampling_random.R")
library("stringr") 


acc_max<-resampling[resampling$acc==max(resampling[,2]),]$gs
####"rs.RandUnder" "0.9" 
acc_max_resampling<-paste("confusion_best_model",str_c(acc_max, collapse = "_") ,sep="_")

TP_max<-resampling[resampling$TP==max(resampling[,3]),]$gs
#### "rs.ImpSamp" "0.3" "4"  
TP_max_resampling<-paste("confusion_best_model",str_c(TP_max, collapse = "_") ,sep="_")

confusion_resampling_max_acc<-matrix(0, ncol = 13,nrow = 13) 
colnames(confusion_resampling_max_acc)<-levels(ds_feature$metier)
rownames(confusion_resampling_max_acc)<-levels(ds_feature$metier)

confusion_resampling_max_TP<-matrix(0, ncol = 13,nrow = 13) 
colnames(confusion_resampling_max_TP)<-levels(ds_feature$metier)
rownames(confusion_resampling_max_TP)<-levels(ds_feature$metier)


for(xi in 1:length(gridlist)) {
  
  x <- gridlist[[xi]]
  
  for(i in 1:nrow(x)){
    if(str_c(as.character(x[i,]), collapse = " ")==str_c(acc_max, collapse = " ")){
     Boosting_full_best_parameter_confusion_matrix(x[i,],confusion_resampling_max_acc,acc_max_resampling)
     
    }else if(str_c(as.character(x[i,]), collapse = " ")==str_c(TP_max, collapse = " ") ){
      Boosting_full_best_parameter_confusion_matrix(x[i,],confusion_resampling_max_TP,TP_max_resampling)
    }
  }
  
}
#############

source("resampling_random.R")
load("resampling_random.RData")
load("ds_feature.RData")

acc_max_random<-resampling_random[resampling_random$acc==max(resampling_random[,acc]),]
acc_max_random_ove<-as.list(as.numeric(strsplit(acc_max_random$ove,",")[[1]]))
acc_max_random_und<-as.list(as.numeric(strsplit(acc_max_random$und,",")[[1]]))
####rs.RandOver     1.25, 4, 1.5, 1.75 0.9098531 7.657536


TP_max_random<-resampling_random[resampling_random$TP==max(resampling_random[,TP]),]
TP_max_random_ove<-as.list(as.numeric(strsplit(TP_max_random$ove,",")[[1]]))
TP_max_random_und<-as.list(as.numeric(strsplit(TP_max_random$und,",")[[1]]))
#### rs.SMOTE 0.9, 0.5, 0.1 3, 4, 5, 1.25 0.8276603 9.243853 


confusion_resampling_max_acc_random<-Boosting_full_best_parameter_confusion_matrix_random(acc_max_random$gs,acc_max_random_und,acc_max_random_ove)
save(confusion_resampling_max_acc_random,file="confusion_resampling_max_acc_random.RData")

confusion_resampling_max_TP_random<-Boosting_full_best_parameter_confusion_matrix_random(TP_max_random$gs,TP_max_random_und,TP_max_random_ove)

save(confusion_resampling_max_TP_random,file="confusion_resampling_max_TP_random.RData")


##########
#rs.ImpSamp com under percent igual a 0.3 e over percent igual a 4
#rs.RandOver  |   1.25, 4, 1.5, 1.75 | 0.9098531 resampling2 max accuracy


### resampling 1 : "rs.ImpSamp" "0.3" "4" 

########For paper
resampling_random_table<-resampling_random %>% separate(und,c( "LLS-PD", "LHP-CEF","LHP-PB"),sep=",")
resampling_random_table<-resampling_random_table %>% separate(ove,c( "FPO-PB", "LHP-PBC", "LLS-DEEP", "PS-PB"),sep=",")

resampling_random_table<-resampling_random_table %>% select(-acc,-TP,-TPmin)
resampling_random_table$gs<-substring(resampling_random_table$gs,4)
colnames(resampling_random_table)<-c("Functions",colnames(resampling_random_table)[-1])
write.table(resampling_random_table, file = "resampling_random_table.txt", sep = ",", quote = FALSE, row.names = T)
