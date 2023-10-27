source("resampling_strategies.R")



create_resampling<-function(){
  columns<- c("gs", "acc","TP")
  
  resampling<- data.frame(matrix(nrow = 0, ncol = length(columns))) 
  
  # assign column names
  colnames(resampling) = columns
  
  #change the class of attributes
  
  
  for(i in 2:3){
    resampling[,i]<-as.numeric(resampling[,i])
  }
   
  
  
  save(resampling,file="resampling.RData")
}




Boosting_full_best_parameter<-function(resamplig_method){
  print(resamplig_method)
  load("resampling.RData")
  
  confusion<-matrix(0, ncol = 13,nrow = 13) 
  colnames(confusion)<-levels(ds_feature$metier)
  rownames(confusion)<-levels(ds_feature$metier)
  begin_test <- min(ds_feature$datadescarga)
  end_test <- AddMonths(begin_test, 3)
  
  while( begin_test < max(ds_feature$datadescarga)) {
    print(end_test)
    metiers<-ds_feature$metier
    train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
    test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
    
    train <- train %>% select(-iddescarga, -datadescarga,-l);
    test <- test %>% select(-iddescarga, -datadescarga,-l);
    if(nrow(test)!=0) {
      
      train<-new_sample(resamplig_method,train)
      
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
      
      acc <- sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
      #print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*acc)))
      #print(paste(max,paste(g,paste(r,early))))
      
      tbl<-table(xgb.pred$label,xgb.pred$prediction)
      
      
      for(i in 1:nrow(test)){
        
        confusion[xgb.pred$label[i],xgb.pred$prediction[i]]<-confusion[xgb.pred$label[i],xgb.pred$prediction[i]]+1
      }
      
    }
    begin_test <- AddMonths(begin_test, 3)
    end_test <- AddMonths(end_test, 3)
  }
  sum<-0
  
  for(i in 1:13){
    sum<-sum+(confusion[i,i]/sum(confusion[i,]))
    
  }
  acc <- sum(diag(confusion))/nrow(ds_feature)
  resampling<-rbindlist(list(resampling,list(as.character(resamplig_method),acc,sum)))
  print(acc)
  save(resampling,file="resampling.RData")
  
  
  
}



Boosting_full_best_parameter_confusion_matrix<-function(resamplig_method,confusion,name_confusion){
  print(resamplig_method)
  
  
  begin_test <- min(ds_feature$datadescarga)
  end_test <- AddMonths(begin_test, 3)
  
  while( begin_test < max(ds_feature$datadescarga)) {
    print(end_test)
    metiers<-ds_feature$metier
    train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
    test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
    
    train <- train %>% select(-iddescarga, -datadescarga,-l);
    test <- test %>% select(-iddescarga, -datadescarga,-l);
    if(nrow(test)!=0) {
      
      train<-new_sample(resamplig_method,train)
      
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
      
      acc <- sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
      #print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*acc)))
      #print(paste(max,paste(g,paste(r,early))))
      
      tbl<-table(xgb.pred$label,xgb.pred$prediction)
      
      
      for(i in 1:nrow(test)){
        
        confusion[xgb.pred$label[i],xgb.pred$prediction[i]]<-confusion[xgb.pred$label[i],xgb.pred$prediction[i]]+1
      }
      
    }
    begin_test <- AddMonths(begin_test, 3)
    end_test <- AddMonths(end_test, 3)
  }
 
  
  
  save(confusion,file=paste(name_confusion,".RData",sep=""))
  
  
  
}