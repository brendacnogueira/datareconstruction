library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
library(janitor)
library(caret)
library(DescTools)

library(rpart)
library(xgboost)
source("resampling_strategies.R")



data_prep<-function(ds){
  ds[ds$metier=="LHM-CEF" & !is.na(ds$metier),]$metier<-"LHP-CEF"
  ds[ds$metier=="LHM-PB" & !is.na(ds$metier),]$metier<-"LHP-PB"
  #------------------------
  ds <- tibble(ds)
  
  ds<-ds %>% clean_names()
  
  ds <- ds %>% select(-i_idamostra, -euros, -compsl, -pontalsl, -bocasl, -arte) #columns to remove

  ds <- ds %>% na.omit
  
  ds_orig <- ds
  
  # first cleanup
  ds <- ds %>% select(-dia, -mes, -ano, -vulgar, -classificacao, -peso) %>% distinct
  
  # repeated iddescarga because of different port
  ax <- ds %>% distinct; ax.tbl <- sort(table(ax$iddescarga),decreasing = TRUE);
  lkup <- names(ax.tbl[ax.tbl>1])
  #ds %>% filter(iddescarga %in% as.numeric(lkup)) %>% print(n=Inf)
  
  ####################################################################################################
  # PREPARATION ABOVE. FINAL DATA SET CREATION BELOW
  # The objective above is to arrive at the id's that must be removed.
  ####################################################################################################
  # repeat from ds_orig to guarantee that the iddescarga do not include erroneous inputs
  
  ds <- ds_orig %>% filter(!(iddescarga %in% as.numeric(lkup)))
  
  # especie
  ds$vulgar <- str_trim(ds$vulgar)
  ds_aux <- select(ds, iddescarga, vulgar, peso)
  especie <- dcast(ds_aux,formula=iddescarga ~ vulgar, fun.aggregate = sum, value.var="peso")
  
  # classificação
  ds$classificacao <- str_trim(ds$classificacao)
  ds_aux <- select(ds, iddescarga, classificacao, peso)
  classificacao <- dcast(ds_aux,formula=iddescarga ~ classificacao, fun.aggregate = sum, value.var="peso")
  
  # ilha
  ds$ilha <- str_trim(ds$ilha)
  ds_aux <- select(ds, iddescarga, ilha)
  ilha <- dcast(ds_aux, formula=iddescarga ~ ilha, length)
  ilha[,-1] <- apply(ilha[,-1], 2, FUN=function(x) ifelse(x>0, 1, 0))
  for (i in 2:ncol(ilha)) { ilha[, i] <- as.factor(ilha[, i]) }
  
  
  # porto
  ds$porto <- str_trim(ds$porto)
  ds_aux <- select(ds, iddescarga, porto)
  porto <- dcast(ds_aux, formula=iddescarga ~ porto, length)
  porto[,-1] <- apply(porto[,-1], 2, FUN=function(x) ifelse(x>0, 1, 0))
  for (i in 2:ncol(porto)) { porto[, i] <- as.factor(porto[, i]) }
  
  
  # classe_com
  ds$classe_com <- str_trim(ds$classe_com)
  ds_aux <- select(ds, iddescarga, classe_com)
  classe_com <- dcast(ds_aux, formula=iddescarga ~ classe_com, length)
  classe_com[,-1] <- apply(classe_com[,-1], 2, FUN=function(x) ifelse(x>0, 1, 0))
  #classe_com[,-1] <- sapply(classe_com[,-1], FUN=function(x) ifelse(x>0, 1, 0))
  for (i in 2:ncol(classe_com)) {classe_com[, i] <- as.factor(classe_com[, i]) }
  
  # date
  ds <- ds %>% mutate(datadescarga = paste0(dia,"-",mes,"-",ano))
  ds$datadescarga <- as.Date(ds$datadescarga, format="%d-%m-%Y")
  
  # matricula
  mat_aux <- str_split(ds$matricula, pattern="-")
  ds$matricula <- unlist(lapply(mat_aux, FUN=function(x) x[length(x)]))
  ds_aux <- select(ds, iddescarga, matricula)
  matricula <- dcast(ds_aux, formula=iddescarga ~ matricula, length)
  matricula[,-1] <- apply(matricula[,-1], 2, FUN=function(x) ifelse(x>0, 1, 0))
  for (i in 2:ncol(matricula)) {matricula[, i] <- as.factor(matricula[, i]) }
  # first cleanup
  ds <- ds %>% select(-dia, -mes, -ano, -vulgar, -classificacao, -peso, -ilha, -porto, -matricula, -classe_com) %>% distinct
  
  # repeated iddescarga because of different port
  #ax <- ds %>% distinct; ax.tbl <- sort(table(ax$iddescarga),decreasing = TRUE);
  #head(ax.tbl) #to confirm that there is only one entry for each iddescarga
  
  # final data set
  ds_final <- inner_join(ds,especie,by=c("iddescarga"))
  ds_final <- inner_join(ds_final, classificacao, by=c("iddescarga"))
  ds_final <- inner_join(ds_final, ilha, by=c("iddescarga"))
  ds_final <- inner_join(ds_final, porto, by=c("iddescarga"))
  ds_final <- inner_join(ds_final, classe_com, by=c("iddescarga"))
  ds_final <- inner_join(ds_final, matricula, by=c("iddescarga"))
  ds_final["peso_total"] <- rowSums(especie[,-1])
  
  ds_final <- janitor::clean_names(ds_final) # prepare column names
  ds_final$metier <- as.factor(ds_final$metier) # factor the target
  
  
  return(ds_final)
}


predict_boosting<-function(train,test,metiers){
  train_x <- data.matrix(train[, -1])
  train_y <- as.integer(train$metier)-1
  
  test_x <- data.matrix(test[, -1])
  test_y <- as.integer(test$metier)-1
  
  xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
  xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
  
  num_class = length(levels(metiers))
  
  params = list(
    booster="gbtree",
    eta=0.001,
    max_depth=10,
    #gamma=g,
    subsample=0.75,
    colsample_bytree=1,
    objective="multi:softprob",
    eval_metric="mlogloss",
    num_class=num_class
  )
  
  watchlist <- list(train=xgb_train, test=xgb_test)
  
  xgb.fit<-xgb.train(
    params=params,
    data=xgb_train,
    nrounds=50,
    early_stopping_rounds=20,
    watchlist=watchlist,
    verbose=0
  )
  xgb.pred <- predict(xgb.fit,test_x,reshape=T)
  xgb.pred <- as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(metiers)
  
  xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
  xgb.pred$label <- levels(metiers)[test_y+1]
  
  
  return(xgb.pred)
}


heuristic_func<-function(train,test,metiers){
  heuristic<-data.frame()
  und<-0.3
  ove<-4
  form<-formula(paste("metier","~."))
  train_resampling<-as.data.frame(train)
  train_resampling<-rs.ImpSamp(form,train_resampling,und,ove)
  
  xgb.pred_original<-predict_boosting(train,test,metiers)
  xgb.pred_resampling<-predict_boosting(train_resampling,test,metiers)
  
  for(i in 1:(nrow(test))){
    predicted_original<-xgb.pred_original$prediction[i]
    predicted_resampling<-xgb.pred_resampling$prediction[i]
    
    if(predicted_resampling=="FPO-PB" ||predicted_resampling=="LHP-PBC" || predicted_resampling=="LLS-DEEP" || predicted_resampling=="PS-PB"){
      df= data.frame(test[i,]$metier,predicted_resampling)
      names(df)=c("metier","predicted")
      heuristic<-rbind(heuristic, df)
      
    }
    else{
      df= data.frame(test[i,]$metier,predicted_original)
      names(df)=c("metier","predicted")
      heuristic<-rbind(heuristic, df)
      
    }
  }
  return(heuristic)
  
}



strategies_window<-function(train_ori,test_ori, window){
  df <- data.frame(matrix(ncol = 5, nrow = 0))
  x<- c("Label","Original", "Resampling1", "Resampling2","Heuristic")
  colnames(df) <- x
  
  metiers<-train_ori$metier
  
  listOfDataframe = list()
  for(i in levels(metiers)){
    
    df_aux<-data.frame(matrix(ncol = 0, nrow = 5))
    rownames(df_aux)=x
    listOfDataframe[[i]]=data.frame(matrix(ncol = 0, nrow = 5))
    
  }
  
  
  begin_train <- min(train_ori$datadescarga)
  end_train <- ifelse(window=="sliding",AddMonths(begin_train, 24),AddMonths(min(train_ori$datadescarga), 24))
  end_test <- AddMonths(end_train, 3)
  max_date=max(test_ori$datadescarga)
  
  while( end_train < max_date) {
    print(end_test)
    train_sliding<-train_ori[train_ori$datadescarga >= begin_train & train_ori$datadescarga < end_train,]
    train_growing<-train_ori[train_ori$datadescarga < end_train,]
    train_full<-train_ori[train_ori$datadescarga < end_train | train_ori$datadescarga >= end_test,]
    
    train <- if(window=="sliding") train_sliding else {if(window=="growing") train_growing else train_full} 
    test_ <- test_ori[test_ori$datadescarga >= end_train & test_ori$datadescarga < end_test,]
    
    
    train <- train %>% select(-iddescarga, -datadescarga);
    test <- test_ %>% select(-iddescarga, -datadescarga);
    
    
    if(nrow(test)!=0) {

      
      source("resampling_strategies.R")
      und<-0.3
      ove<-4
      form<-formula(paste("metier","~."))
      train_<-as.data.frame(train)
      train_resampling<-rs.ImpSamp(form,train_,und,ove)
      
      
      source("resampling_random.R")
      ove<-list(1.25, 4, 1.5, 1.75)
      train_resampling2<-rs.RandOver(form,train_,ove)
      
      xgb.pred_original<-predict_boosting(train,test,metiers)
      
      
      xgb.pred_resampling<-predict_boosting(train_resampling,test,metiers)
      
      
      xgb.pred_resampling2<-predict_boosting(train_resampling2,test,metiers)
    

      
      aux=data.frame(
                     "Label" = test$metier,                   # Create data frame
                     "Original" =xgb.pred_original[,c("prediction")],
                     "Resampling1"=xgb.pred_resampling[,c("prediction")],
                     "Resampling2"=xgb.pred_resampling2[,c("prediction")],
                     "Heuristic" =heuristic_aggregate(xgb.pred_original,xgb.pred_resampling,test))
      
      for(class in levels(metiers)){
        ds_class<-listOfDataframe[[class]]
        list_st<-c()
        for(strategy in colnames(aux)){
          
          list_st=append(list_st,c((nrow(aux[aux[,strategy]==class,])/nrow(aux))))
        }
        ds_class[as.character(end_test)]<-list_st
        listOfDataframe[[class]]= ds_class
      }

      df<-rbind(df,aux)
    }
      
      
    begin_train <- AddMonths(begin_train, 3)
    end_train <- AddMonths(end_train, 3)
    end_test <- AddMonths(end_test, 3)
    
    
  }
  results<-list(df,listOfDataframe)
  
  return(results)
  
}
#install.packages("ggplot2")

diff_perc<-function(results,class){
  new=as.matrix(results[[2]][[class]])
  difference_percentages<-data.frame(apply(new[2:nrow(new),], 1, function(x)  (((x-new[1,])/new[1,])*100)))
  
  colnames(difference_percentages)=c("Original", "Resampling1", "Resampling2","Heuristic")
  
  difference_percentages[sapply(difference_percentages,is.infinite)]<-NaN
  difference_percentages<-as.data.frame(difference_percentages)
  difference_percentages["end_test_date"]<-rownames(difference_percentages)
  
  
  data_ggp <- data.frame(x = difference_percentages$end_test_date,                            # Reshape data frame
                         y = c(difference_percentages$Original, difference_percentages$Resampling1, difference_percentages$Resampling2,difference_percentages$Heuristic),
                         group = c(rep("Original", nrow(difference_percentages)),
                                   rep("Resampling1", nrow(difference_percentages)),
                                   rep("Resampling2", nrow(difference_percentages)),
                                   rep("Heuristic", nrow(difference_percentages))))
  
  
  return(data_ggp)
}

evaluation<-function(results,metiers){
  colname<-c("mean","sd","median")
  evaluate<-data.frame(matrix(nrow = 4, ncol = 3))
  evaluate[is.na(evaluate)]<-0
  rowname<-c("Heuristic","Original","Resampling1","Resampling2")
  colnames(evaluate)<-colname
  rownames(evaluate)<-rowname
  
  for(cls in metiers){
    # Install & load ggplot2
    difference_percentages<-diff_perc(results,cls)
    difference_percentages<-difference_percentages %>% drop_na()
    
    data_gpp<-aggregate(difference_percentages$y, list(difference_percentages$group), FUN=mean)
    data_gpp["sd"]<-aggregate(difference_percentages$y, list(difference_percentages$group), FUN=sd)$x
    data_gpp["median"]<-aggregate(difference_percentages$y, list(difference_percentages$group), FUN=median)$x
    colnames(data_gpp)=c("method","mean","sd","median")
    
    for(i in colname){
      for(j in rowname){
        evaluate[j,i]=evaluate[j,i]+data_gpp[data_gpp[,"method"]==j,i]
      }
    }
    
  }
  evaluate<-evaluate/length(metiers)
  
}

plot_lines<-function(results_sliding,results_growing,results_full,class_,path){
  
  print(class_)
  
  
  difference_percentages_sliding<-diff_perc(results_sliding,class_)
  difference_percentages_growing<-diff_perc(results_growing,class_)
  difference_percentages_full<-diff_perc(results_full,class_)
  
  
  data_ggp_sliding <-difference_percentages_sliding
  data_ggp_sliding["window"]<-"Sliding"
  
  data_ggp_growing <-difference_percentages_growing
  data_ggp_growing["window"]<-"Growing"
  
  data_ggp_full <-difference_percentages_full
  data_ggp_full["window"]<-"Full"
  
  data_ggp<-rbind(data_ggp_sliding,data_ggp_growing,data_ggp_full)
  data_ggp<-rbind(data_ggp,data_ggp_full)
  
  data_ggp$x<-as.Date(data_ggp$x)

  library("tidyr")
  data_ggp<-data_ggp %>% drop_na()
  
  library("ggplot2")
  
  
  ggp <- ggplot(data_ggp, aes(x, y, group=group,col=group)) +             # Create ggplot2 plot
    geom_line() + ggtitle(class_) +geom_point()
  
   
  
  ggp <- ggp + facet_grid(window ~ .)

 
  png( paste(path,paste(class_,"_windows.png",sep=""),sep=""),width =1800,height=750)
  print(ggp)
  dev.off()
  return(data_gpp_mean_sd)
  
 
}




heuristic_aggregate<-function(xgb.pred_original,xgb.pred_resampling,test){
  heuristic<-data.frame()
  
  for(i in 1:(nrow(test))){
    predicted_original<-xgb.pred_original$prediction[i]
    predicted_resampling<-xgb.pred_resampling$prediction[i]
    
    if(predicted_resampling=="FPO-PB" ||predicted_resampling=="LHP-PBC" || predicted_resampling=="LLS-DEEP" || predicted_resampling=="PS-PB"){
      df= data.frame(predicted_resampling)
      names(df)=c("prediction")
      heuristic<-rbind(heuristic, df)
      
    }
    else{
      df= data.frame(predicted_original)
      names(df)=c("prediction")
      heuristic<-rbind(heuristic, df)
      
    }
    
  }
  return(heuristic$prediction)
}



full_window_heuristic<-function(ds){
  ds<-ds_feature
  begin_test <- min(ds$datadescarga)
  end_test <- AddMonths(begin_test, 3)
  
  while( begin_test < max(ds$datadescarga)) {
    print(end_test)
    metiers<-ds$metier
    train <-ds[ds$datadescarga < begin_test | ds$datadescarga >= end_test, ]
    test <-ds[ds$datadescarga >= begin_test & ds$datadescarga < end_test, ]
    
    
    train <- train %>% select(-iddescarga, -datadescarga);
    test <- test %>% select(-iddescarga, -datadescarga);
    
    
    if(nrow(test)!=0) {
     
      heuristic2<-heuristic_func(train,test,metiers)
      heuristic<-rbind(heuristic,heuristic2)
      
    }
    begin_test  <- AddMonths(begin_test, 3)
    end_test <- AddMonths(end_test, 3)
   
  }
  
  return(heuristic)
  
}

acc_per_metier<-function(tbl){
  acc_class<-data.frame()
  soma<-0
  for (row in rownames(tbl)){
    if(any(colnames(tbl)==row)){
      acc_class["acc",row]<-tbl[c(row),c(row)]/sum(tbl[row,])
      soma=soma+tbl[c(row),c(row)]
    }
    else{
      acc_class["acc",row]<-0
    }
    
  }
  acc_class["acc","acc"]<-soma/sum(tbl)
  
  return(acc_class)
}

load("classificacao.RData")
feature_add<-function(ds){
  ds_feature<-ds
  for(classif in classificacao){
    ds_feature[paste("max_peso_",classif,sep="")]<-0
    ds_feature[paste("min_peso_",classif,sep="")]<-0
    ds_feature[paste("mean_peso_",classif,sep="")]<-0
  }
  
  for(i in 1:nrow(ds_feature)){
    ds_sample<-ds_feature[i,]
    print(i)
    begin_sample<-AddMonths(ds_sample$datadescarga,-6)
    end_sample<-ds_sample$datadescarga
    len<-nrow(ds_feature[ds_feature$datadescarga>=begin_sample & ds_feature$datadescarga<end_sample, ])
    for(classif in classificacao){
      ##by classification
      #print(classif)
      if(len!=0){
        ds_feature[i,paste("max_peso_",classif,sep="")]<-max(ds_feature[ds_feature$datadescarga>=begin_sample & ds_feature$datadescarga<end_sample, ][classif])
        ds_feature[i,paste("min_peso_",classif,sep="")]<-min(ds_feature[ds_feature$datadescarga>=begin_sample & ds_feature$datadescarga<end_sample, ][classif])
        ds_feature[i,paste("mean_peso_",classif,sep="")]<-mean(ds_feature[ds_feature$datadescarga>=begin_sample & ds_feature$datadescarga<end_sample, ][classif])
        #ds_feature[i,]<-ds_sample
      }
      
    }
  }
  
  ds_feature[is.na(ds_feature)]<-0
  
  return(ds_feature)
  

}
