rs.RandUnder <- function(form, train, und.perc, ...) {
  
  nms<-c( "LLS-PD", "LHP-CEF","LHP-PB")
  print(und.perc)
  
  lst<-as.list(und.perc); 
 
  names(lst) <- nms

  
  new.ds <- UBL::RandUnderClassif(form = form, dat = train, C.perc = lst, ...)
  
  new.ds
  
  
}

rs.RandOver <- function(form, train, ove.perc, ...) {
  
 
  nms<-c("FPO-PB","LHP-PBC","LLS-DEEP","PS-PB")

  lst<-ove.perc
 
  names(lst) <- nms
 
 
  new.ds <- UBL::RandOverClassif(form = form, dat = train, C.perc = lst)
  
  new.ds
  
}
rs.ImpSamp <- function(form, train, und.perc, ove.perc, ...) {
  print(und.perc)
  print(ove.perc)
  
  nms<-c("FPO-PB","LHP-PBC","LLS-DEEP","PS-PB")
  
  other<-c( "LLS-PD", "LHP-CEF","LHP-PB")
  
  lst<-append(as.list(und.perc),as.list(ove.perc))
  
  
  names(lst) <- append(other,nms)
  
  new.ds <- UBL::WERCSClassif(form = form, dat = train, C.perc = lst, ...)
  
  new.ds
  
}

rs.SMOTE <- function(form, train, und.perc, ove.perc, ...) {
  print(und.perc)
  print(ove.perc)
  
  nms<-c("FPO-PB","LHP-PBC","LLS-DEEP","PS-PB")
  
  other<-c( "LLS-PD", "LHP-CEF","LHP-PB")
  
  lst<-append(as.list(und.perc),as.list(ove.perc))
  
  names(lst) <- append(other,nms)
  
  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")
  
  new.ds <- UBL::SmoteClassif(form = form, dat = train, C.perc = lst, dist = distance, ...)
  
  new.ds
  
}




new_sample_random<-function(method,und,ove,train){
  
  
  train<-as.data.frame(train)
  
  balanced<-NULL
  form<-formula(paste("metier","~."))
  
  
  if(method=="rs.ENN"){
    #balanced<-rs.ENN(form,train,method$k)
    
  }else if(method=="rs.RandOver"){
    balanced<-rs.RandOver(form,train,ove)
    
  }else if(method=="rs.RandUnder"){
    balanced<-rs.RandUnder(form,train,und)
    
  }else if(method=="rs.ImpSamp"){
    
    balanced<-rs.ImpSamp(form,train,und,ove)
    
  }else if(method=="rs.SMOTE"){
    balanced<-rs.SMOTE(form,train,und,ove)
    
  }
  
  
  
  balanced<-as_tibble(balanced)
  print(nrow(balanced))
  return(balanced)
  
}

Boosting_full_best_parameter_random<-function(resamplig_method,und,ove){
  print(resamplig_method)
  load("resampling_random.RData")
  
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
      
      train<-new_sample_random(resamplig_method,und,ove,train)
      
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
  min<-1
  for(i in 1:13){
    sum<-sum+(confusion[i,i]/sum(confusion[i,]))
    min<-ifelse((confusion[i,i]/sum(confusion[i,]))<min,(confusion[i,i]/sum(confusion[i,])),min)
    
  }
  acc <- sum(diag(confusion))/nrow(ds_feature)
  resampling_random<-rbindlist(list(resampling_random,list(resamplig_method,toString(und),toString(ove),acc,sum,min)))
  print(acc)
  save(resampling_random,file="resampling_random.RData")
  
  
  
}

Boosting_full_best_parameter_confusion_matrix_random<-function(resamplig_method,und,ove){
  print(resamplig_method)
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
      
      train<-new_sample_random(resamplig_method,und,ove,train)
      
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
  return(confusion)
  
  
  
}




create_resampling_random<-function(){
  columns<- c("gs","und","ove", "acc","TP","TPmin")
  
  resampling_random<- data.frame(matrix(nrow = 0, ncol = length(columns))) 
  
  # assign column names
  colnames(resampling_random) = columns
  
  #change the class of attributes
  
  
  for(i in 4:6){
    resampling_random[,i]<-as.numeric(resampling_random[,i])
  }
  
  
  
  save(resampling_random,file="resampling_random.RData")
}



