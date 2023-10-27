
load("ds_feature.RData")


columns<- c("end_test_date","window","method","parameter","acc")

for(lev in levels(ds_feature$metier)){
  columns<-append(columns,lev)
}


results_feature_new_window <- data.frame(matrix(nrow = 0, ncol = length(columns))) 

# assign column names
colnames(results_feature_new_window) = columns

#change the class of attributes
results_feature_new_window$end_test_date<-as.Date(results_feature_new_window$end_test_date)
for(i in 2:4){
  results_feature_new_window[,i]<-as.character(results_feature_new_window[,i])
}
for(i in 5:18){
  results_feature_new_window[,i]<-as.numeric(results_feature_new_window[,i])
}


save(results_feature_new_window,file="results_feature_new_window.RData")

load("ds_feature.RData")
load("results_feature_new_window.RData")

##########3

### TESTING  Random Forest (full window)

acc.vec <- c()

for(max in c(250, 500, 750)) {
  begin_test <- min(ds_feature$datadescarga)
  end_test <- AddMonths(begin_test, 1)
  
  while(end_test < max(ds_feature$datadescarga)) {
    
    print(end_test)
    
    train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
    test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
    
    train <- train %>% select(-iddescarga, -datadescarga);
    test <- test %>% select(-iddescarga, -datadescarga);
    
    if(nrow(test)!=0) {
      
      library(ranger)
      m <- ranger(formula=metier ~ ., data=train,num.trees = max, importance="impurity")
      p <- predict(m, test)
      # p$predictions
      
      tbl <- table(test$metier,p$predictions)
      acc <- sum(diag(tbl))/nrow(test) # accuracy
      
      results_feature_new_window<-rbindlist(list(results_feature_new_window,list(end_test,"new","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
      for (row in rownames(tbl)){
        if(any(colnames(tbl)==row)){
          acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
          results_feature_new_window[nrow(results_feature_new_window),c(row)]<-acc_class
        }
        else{
          results_feature_new_window[nrow(results_feature_new_window),c(row)]<-0
        }
        
      }
      
      acc.vec <- c(acc.vec, acc)
      
      
    }
    
    
    begin_test <- AddMonths(begin_test, 1)
    end_test <- AddMonths(end_test, 1)
    
    
  }
}


plot(acc.vec, type="l", ylim=c(0.75,1))

save(results_feature_new_window,file="results_feature_new_window.RData")


### TESTING  Decision Tree (full window)

acc.vec <- c()

begin_test <- min(ds_feature$datadescarga)
end_test <- AddMonths(begin_test, 1)

while(end_test < max(ds_feature$datadescarga)) {
  
  print(end_test)
  
  
  train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
  test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m<-rpart(formula = metier ~ . , data = train, control = rpart.control(maxdepth = 10))
    p <- predict(m, test, type="class")
    # p$predictions
    
    tbl <- table(test$metier,p)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    
    results_feature_new_window<-rbindlist(list(results_feature_new_window,list(end_test,"new","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results_feature_new_window[nrow(results_feature_new_window),c(row)]<-acc_class
      }
      else{
        results_feature_new_window[nrow(results_feature_new_window),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
    
    
    
  }
  
  begin_test <- AddMonths(begin_test, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}
lines(acc.vec, col="green")
save(results_feature_new_window,file="results_feature_new_window.RData")



### TESTING Boosting (full window)

library(rpart)
library(xgboost)
acc.vec <- c()

load("ds_features.RData")
for (max in c(2,10,20)){
  print(paste("max:",max))
  for (r in c(50,75,100)){
    print(paste("r:",r))
    for (early in c(20)){
      begin_test <- min(ds_feature$datadescarga)
      end_test <- AddMonths(begin_test, 1)
      while( begin_test < max(ds_feature$datadescarga)) {
        print(end_test)
        metiers<-ds_feature$metier
        train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
        test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
        
        train <- train %>% select(-iddescarga, -datadescarga);
        test <- test %>% select(-iddescarga, -datadescarga);
        if(nrow(test)!=0) {
          train_x <- data.matrix(train[, -1])
          train_y <- as.integer(train$metier)-1
          
          test_x <- data.matrix(test[, -1])
          test_y <- as.integer(test$metier)-1
          
          xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
          xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
          
          num_class = length(levels(metiers))
          load("results_feature_new_window.RData")
          params = list(
            booster="gbtree",
            eta=0.001,
            max_depth=max,
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
            nrounds=r,
            early_stopping_rounds=early,
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
          acc.vec <- c(acc.vec, acc)
          results_feature_new_window<-rbindlist(list(results_feature_new_window,list(end_test,"new","Boosting",
                                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results_feature_new_window[nrow(results_feature_new_window),c(row)]<-acc_class
            }
            else{
              results_feature_new_window[nrow(results_feature_new_window),c(row)]<-0
            }
            
          }
          save(results_feature_new_window,file="results_feature_new_window.RData")
        }
        begin_test <- AddMonths(begin_test, 1)
        end_test <- AddMonths(end_test, 1)
      }
      
    }
  }
}
lines(acc.vec, col="red")

save(results_feature_new_window,file="results_feature_new_window.RData")



### TESTING  Random Forest (sliding window)

acc.vec <- c()

for(max in c(250, 500, 750)) {
  
  end_train <- min(ds_feature$datadescarga)
  begin_train <- AddMonths(end_train, -24)
  begin_train_2<-AddMonths(end_train,1)
  end_train_2<-AddMonths(begin_train_2, 24)
  
  while(end_train < max(ds_feature$datadescarga)) {
    
    print(begin_train_2)
    
    train <- ds_feature[(ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train) | (ds_feature$datadescarga >= begin_train_2 & ds_feature$datadescarga < end_train_2),]
    test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < begin_train_2,]
    
    train <- train %>% select(-iddescarga, -datadescarga);
    test <- test %>% select(-iddescarga, -datadescarga);
    
    if(nrow(test)!=0) {
      
      library(ranger)
      m<- ranger(formula=metier ~ ., data=train, num.trees = max,importance="impurity")
      p <- predict(m, test)
      # p$predictions
      
      tbl <- table(test$metier,p$predictions)
      acc <- sum(diag(tbl))/nrow(test) # accuracy
      
      results_feature_new_window<-rbindlist(list(results_feature_new_window,list(begin_train_2,"new 2","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
      for (row in rownames(tbl)){
        if(any(colnames(tbl)==row)){
          acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
          results_feature_new_window[nrow(results_feature_new_window),c(row)]<-acc_class
        }
        else{
          results_feature_new_window[nrow(results_feature_new_window),c(row)]<-0
        }
        
      }
      
      acc.vec <- c(acc.vec, acc)
      
      
    }
    
    end_train <- AddMonths(end_train,1)
    begin_train <- AddMonths(begin_train, 1)
    begin_train_2<-AddMonths(begin_train_2,1)
    end_train_2<-AddMonths(end_train_2, 1)
    
  }
}
plot(acc.vec, type="l", ylim=c(0.75,1))
save(results_feature_new_window,file="results_feature_new_window.RData")


### TESTING Decision Tree (sliding window)

acc.vec <- c()

end_train <- min(ds_feature$datadescarga)
begin_train <- AddMonths(end_train, -24)
begin_train_2<-AddMonths(end_train,1)
end_train_2<-AddMonths(begin_train_2, 24)

while(end_train < max(ds_feature$datadescarga)) {
  
  print(begin_train_2)
  
  train <- ds_feature[(ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train) | (ds_feature$datadescarga >= begin_train_2 & ds_feature$datadescarga < end_train_2),]
  test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < begin_train_2,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(rpart)
    m <-rpart(formula = metier ~ . , data = train, control = rpart.control(maxdepth = 10))
    p <- predict(m, test, type="class")
    # p$predictions
    
    tbl <- table(test$metier,p)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    
    results_feature_new_window<-rbindlist(list(results_feature_new_window,list(begin_train_2,"new 2","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results_feature_new_window[nrow(results_feature_new_window),c(row)]<-acc_class
      }
      else{
        results_feature_new_window[nrow(results_feature_new_window),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
    
    
    
  }
  
  
  end_train <- AddMonths(end_train,1)
  begin_train <- AddMonths(begin_train, 1)
  begin_train_2<-AddMonths(begin_train_2,1)
  end_train_2<-AddMonths(end_train_2, 1)
  
  
}

lines(acc.vec, col="blue")

save(results_feature_new_window,file="results_feature_new_window.RData")

### TESTING Boosting (sliding window)

library(rpart)
library(xgboost)
acc.vec <- c()

load("ds_feature.RData")
for (max in c(2,10,20)){
  print(paste("max:",max))
  for (r in c(50,75,100)){
    print(paste("r:",r))
    for (early in c(20)){
      
      end_train <- min(ds_feature$datadescarga)
      begin_train <- AddMonths(end_train, -24)
      begin_train_2<-AddMonths(end_train,1)
      end_train_2<-AddMonths(begin_train_2, 24)
      
      while(end_train  < max(ds_feature$datadescarga)) {
        print(begin_train_2)
        metiers<-ds_feature[ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train_2,]$metier
        train <- ds_feature[(ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train) | (ds_feature$datadescarga >= begin_train_2 & ds_feature$datadescarga < end_train_2),]
        test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < begin_train_2,]
        
        train <- train %>% select(-iddescarga, -datadescarga);
        test <- test %>% select(-iddescarga, -datadescarga);
        if(nrow(test)!=0) {
          train_x <- data.matrix(train[, -1])
          train_y <- as.integer(train$metier)-1
          
          test_x <- data.matrix(test[, -1])
          test_y <- as.integer(test$metier)-1
          
          xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
          xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
          
          num_class = length(levels(metiers))
          load("results_feature_new_window.RData")
          params = list(
            booster="gbtree",
            eta=0.001,
            max_depth=max,
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
            nrounds=r,
            early_stopping_rounds=early,
            watchlist=watchlist,
            verbose=0
          )
          xgb.pred <- predict(xgb.fit,test_x,reshape=T)
          xgb.pred <- as.data.frame(xgb.pred)
          colnames(xgb.pred) = levels(metiers)
          
          xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
          xgb.pred$label <- levels(metiers)[test_y+1]
          
          acc <- sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
          print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*acc)))
          #print(paste(max,paste(g,paste(r,early))))
          
          tbl<-table(xgb.pred$label,xgb.pred$prediction)
          acc.vec <- c(acc.vec, acc)
          results_feature_new_window<-rbindlist(list(results_feature_new_window,list(begin_train_2,"new 2","Boosting",
                                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results_feature_new_window[nrow(results_feature_new_window),c(row)]<-acc_class
            }
            else{
              results_feature_new_window[nrow(results_feature_new_window),c(row)]<-0
            }
            
          }
          save(results_feature_new_window,file="results_feature_new_window.RData")
        }
        
        end_train <- AddMonths(end_train,1)
        begin_train <- AddMonths(begin_train, 1)
        begin_train_2<-AddMonths(begin_train_2,1)
        end_train_2<-AddMonths(end_train_2, 1)
      }
      
    }
  }
}
lines(acc.vec, col="red")

save(results_feature_new_window,file="results_feature_new_window.RData")
