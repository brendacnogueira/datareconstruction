library(caret)
library(DescTools)
library(caret)
library(dplyr)
library(data.table)



#ds_feature<-ds_final%>% mutate(max_peso = 0,min_peso=0,mean_peso=0)

load("ds_final.RData")
ds_feature<-ds_final
for(classif in colnames((ds_final[,103:116]))){
  ds_feature[paste("max_peso_",classif,sep="")]<-0
  ds_feature[paste("min_peso_",classif,sep="")]<-0
  ds_feature[paste("mean_peso_",classif,sep="")]<-0
}



for(i in 1:nrow(ds_feature)){
  ds_sample<-ds_feature[i,]
  print(i)
  begin_sample<-AddMonths(ds_sample$datadescarga,-6)
  end_sample<-ds_sample$datadescarga
  for(classif in colnames((ds_final[,103:116]))){
    ##by classification
    #print(classif)
    if(i!=1){
      ds_feature[i,paste("max_peso_",classif,sep="")]<-max(ds_feature[ds_feature$datadescarga>=begin_sample & ds_feature$datadescarga<end_sample, ][classif])
      ds_feature[i,paste("min_peso_",classif,sep="")]<-min(ds_feature[ds_feature$datadescarga>=begin_sample & ds_feature$datadescarga<end_sample, ][classif])
      ds_feature[i,paste("mean_peso_",classif,sep="")]<-mean(ds_feature[ds_feature$datadescarga>=begin_sample & ds_feature$datadescarga<end_sample, ][classif])
      #ds_feature[i,]<-ds_sample
    }
    
  }
  
  
}

ds_feature[is.na(ds_feature)]<-0



save(ds_feature,file="ds_feature.RData")

####Test

load("ds_feature.RData")


columns<- c("end_test_date","window","method","parameter","acc")

for(lev in levels(ds_feature$metier)){
  columns<-append(columns,lev)
}


results_feature <- data.frame(matrix(nrow = 0, ncol = length(columns))) 

# assign column names
colnames(results_feature) = columns

#change the class of attributes
results_feature$end_test_date<-as.Date(results_feature$end_test_date)
for(i in 2:4){
  results_feature[,i]<-as.character(results_feature[,i])
}
for(i in 5:18){
  results_feature[,i]<-as.numeric(results_feature[,i])
}


save(results_feature,file="results_feature.RData")

load("ds_feature.RData")
load("results_feature.RData")

### TESTING  Random Forest (sliding window)

acc.vec <- c()

for(max in c(250, 500, 750)) {

begin_train <- min(ds_feature$datadescarga)
end_train <- AddMonths(begin_train, 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_feature$datadescarga)) {
  
  print(end_test)
  
  train <- ds_feature[ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train,]
  test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m<- ranger(formula=metier ~ ., data=train, num.trees = max,importance="impurity")
    p <- predict(m, test)
    # p$predictions
    
    tbl <- table(test$metier,p$predictions)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    
    results_feature<-rbindlist(list(results_feature,list(end_test,"sliding","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results_feature[nrow(results_feature),c(row)]<-acc_class
      }
      else{
        results_feature[nrow(results_feature),c(row)]<-0
      }
      
    }
    
    acc.vec <- c(acc.vec, acc)
    
    
  }
  
  begin_train <- AddMonths(begin_train, 1)
  end_train <- AddMonths(end_train, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}
}
plot(acc.vec, type="l", ylim=c(0.75,1))
save(results_feature,file="results_feature.RData")

### TESTING  Random Forest (growing window)

acc.vec <- c()

for(max in c(250, 500, 750)) {
# begin_train <- min(ds_feature$datadescarga)
end_train <- AddMonths(min(ds_feature$datadescarga), 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_feature$datadescarga)) {
  
  print(end_test)
  
  # train <- ds_feature[ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train,]
  train <- ds_feature[ds_feature$datadescarga < end_train,]
  test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m <- ranger(formula=metier ~ ., data=train,num.trees = max, importance="impurity")
    p <- predict(m, test)
    # p$predictions
    
    tbl <- table(test$metier,p$predictions)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    
    results_feature<-rbindlist(list(results_feature,list(end_test,"growing","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results_feature[nrow(results_feature),c(row)]<-acc_class
      }
      else{
        results_feature[nrow(results_feature),c(row)]<-0
      }
      
    }
    
    acc.vec <- c(acc.vec, acc)
    
    
  }
  
  # begin_train <- AddMonths(begin_train, 3)
  end_train <- AddMonths(end_train, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}
}

lines(acc.vec, col="red")

save(results_feature,file="results_feature.RData")

### TESTING  Random Forest (full window)

acc.vec <- c()

for(max in c(250, 500, 750)) {
  begin_test <- min(ds_feature$datadescarga)
  end_test <- AddMonths(begin_test, 3)
  
  while(end_test < max(ds_feature$datadescarga)) {
    
    print(end_test)
    
    train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
    test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
    
    train <- train %>% select(-iddescarga, -datadescarga,-l);
    test <- test %>% select(-iddescarga, -datadescarga,-l);
    
    if(nrow(test)!=0) {
      
      library(ranger)
      m <- ranger(formula=metier ~ ., data=train,num.trees = max, importance="impurity")
      p <- predict(m, test)
      # p$predictions
      
      tbl <- table(test$metier,p$predictions)
      acc <- sum(diag(tbl))/nrow(test) # accuracy
      
      results_feature<-rbindlist(list(results_feature,list(end_test,"full","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
      for (row in rownames(tbl)){
        if(any(colnames(tbl)==row)){
          acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
          results_feature[nrow(results_feature),c(row)]<-acc_class
        }
        else{
          results_feature[nrow(results_feature),c(row)]<-0
        }
        
      }
      
      acc.vec <- c(acc.vec, acc)
      
      
    }
    
    
    begin_test <- AddMonths(begin_test, 3)
    end_test <- AddMonths(end_test, 3)
    
    
  }
}

#lines(acc.vec, col="red")

save(results_feature,file="results_feature.RData")

### TESTING Decision Tree (sliding window)

acc.vec <- c()

begin_train <- min(ds_feature$datadescarga)
end_train <- AddMonths(begin_train, 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_feature$datadescarga)) {
  
  print(end_test)
  
  train <- ds_feature[ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train,]
  test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(rpart)
    m <-rpart(formula = metier ~ . , data = train, control = rpart.control(maxdepth = 10))
    p <- predict(m, test, type="class")
    # p$predictions
    
    tbl <- table(test$metier,p)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    
    results_feature<-rbindlist(list(results_feature,list(end_test,"sliding","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results_feature[nrow(results_feature),c(row)]<-acc_class
      }
      else{
        results_feature[nrow(results_feature),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
    
    
    
  }
  
  begin_train <- AddMonths(begin_train, 1)
  end_train <- AddMonths(end_train, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}

lines(acc.vec, col="blue")

save(results_feature,file="results_feature.RData")

### TESTING  Decision Tree (growing window)

acc.vec <- c()

# begin_train <- min(ds_feature$datadescarga)
end_train <- AddMonths(min(ds_feature$datadescarga), 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_feature$datadescarga)) {
  
  print(end_test)
  
  # train <- ds_feature[ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train,]
  train <- ds_feature[ds_feature$datadescarga < end_train,]
  test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m<-rpart(formula = metier ~ . , data = train, control = rpart.control(maxdepth = 10))
    p <- predict(m, test, type="class")
    # p$predictions
    
    tbl <- table(test$metier,p)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    
    results_feature<-rbindlist(list(results_feature,list(end_test,"growing","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results_feature[nrow(results_feature),c(row)]<-acc_class
      }
      else{
        results_feature[nrow(results_feature),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
    
    
    
  }
  
  # begin_train <- AddMonths(begin_train, 3)
  end_train <- AddMonths(end_train, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}
lines(acc.vec, col="green")
save(results_feature,file="results_feature.RData")


load("results_feature.RData")

### TESTING  Decision Tree (full window)

acc.vec <- c()

begin_test <- min(ds_feature$datadescarga)
end_test <- AddMonths(begin_test, 3)

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
    
    results_feature<-rbindlist(list(results_feature,list(end_test,"full","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results_feature[nrow(results_feature),c(row)]<-acc_class
      }
      else{
        results_feature[nrow(results_feature),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
    
    
    
  }
  
  begin_test <- AddMonths(begin_test, 3)
  end_test <- AddMonths(end_test, 3)
  
  
}
lines(acc.vec, col="green")
save(results_feature,file="results_feature.RData")


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
      begin_train <- min(ds_feature$datadescarga)
      end_train <- AddMonths(begin_train, 24)
      end_test <- AddMonths(end_train, 3)
      while(end_test  < max(ds_feature$datadescarga)) {
        print(end_test)
        metiers<-ds_feature[ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_test,]$metier
        train <- ds_feature[ds_feature$datadescarga >= begin_train & ds_feature$datadescarga < end_train,]
        test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < end_test,]
        
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
          load("results_feature.RData")
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
          results_feature<-rbindlist(list(results_feature,list(end_test,"sliding","Boosting",
                                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results_feature[nrow(results_feature),c(row)]<-acc_class
            }
            else{
              results_feature[nrow(results_feature),c(row)]<-0
            }
            
          }
          save(results_feature,file="results_feature.RData")
        }
        begin_train <- AddMonths(begin_train, 1)
        end_train <- AddMonths(end_train, 1)
        end_test <- AddMonths(end_test, 1)
      }
      
    }
  }
}
#plot(acc.vec, type="l", ylim=c(0.75,1))

save(results_feature,file="results_feature.RData")

### TESTING Boosting (growing window)

library(rpart)
library(xgboost)
acc.vec <- c()

load("ds_features.RData")
for (max in c(2,10,20)){
  print(paste("max:",max))
  for (r in c(50,75,100)){
    print(paste("r:",r))
    for (early in c(20)){
      #begin_train <- min(ds_feature$datadescarga)
      end_train <- AddMonths(min(ds_feature$datadescarga), 24)
      end_test <- AddMonths(end_train, 3)
      while(end_test < max(ds_feature$datadescarga)) {
        print(end_test)
        metiers<-ds_feature[ds_feature$datadescarga < end_test,]$metier
        train <- ds_feature[ds_feature$datadescarga < end_train,]
        test <- ds_feature[ds_feature$datadescarga >= end_train & ds_feature$datadescarga < end_test,]
        
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
          load("results_feature.RData")
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
          results_feature<-rbindlist(list(results_feature,list(end_test,"growing","Boosting",
                                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results_feature[nrow(results_feature),c(row)]<-acc_class
            }
            else{
              results_feature[nrow(results_feature),c(row)]<-0
            }
            
          }
          save(results_feature,file="results_feature.RData")
        }
       # begin_train <- AddMonths(begin_train, 1)
        end_train <- AddMonths(end_train, 1)
        end_test <- AddMonths(end_test, 1)
      }
      
    }
  }
}
plot(acc.vec, type="l", ylim=c(0.75,1))

save(results_feature,file="results_feature.RData")

### TESTING Boosting (full window)

library(rpart)
library(xgboost)
acc.vec <- c()

load("ds_feature.RData")
for (max in c(2,10,20)){
  print(paste("max:",max))
  for (r in c(50,75,100)){
    print(paste("r:",r))
    for (early in c(20)){
      begin_test <- min(ds_feature$datadescarga)
      end_test <- AddMonths(begin_test, 3)
      while( end_test < max(ds_feature$datadescarga)) {
        print(end_test)
        metiers<-ds_feature$metier
        train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
        test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
        
        train <- train %>% select(-iddescarga, -datadescarga,-l);
        test <- test %>% select(-iddescarga, -datadescarga,-l);
        if(nrow(test)!=0) {
          train_x <- data.matrix(train[, -1])
          train_y <- as.integer(train$metier)-1
          
          test_x <- data.matrix(test[, -1])
          test_y <- as.integer(test$metier)-1
          
          xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
          xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
          
          num_class = length(levels(metiers))
          load("results_feature.RData")
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
          #acc.vec <- c(acc.vec, acc)
          results_feature<-rbindlist(list(results_feature,list(end_test,"full","Boosting",
                                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results_feature[nrow(results_feature),c(row)]<-acc_class
            }
            else{
              results_feature[nrow(results_feature),c(row)]<-0
            }
            
          }
          save(results_feature,file="results_feature.RData")
        }
        begin_test <- AddMonths(begin_test, 3)
        end_test <- AddMonths(end_test, 3)
      }
      
    }
  }
}
#plot(acc.vec, type="l", ylim=c(0.75,1))

save(results_feature,file="results_feature.RData")






