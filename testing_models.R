library(caret)
library(DescTools)
library(caret)
library(data.table)
library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
library(janitor)

load("ds_final.RData")
results<-data.frame(end_test_date=as.Date('2017-01-01', format="%d-%m-%Y"),
                    window="sliding",
                    method="rager",
                    parameter="",
                    acc=0.80)
for(col in levels(ds_final$metier)){
  results[[col]]<-0.80
}
results<-results[-c(1),]

save(results,file="results.RData")



### TESTING  Random Forest (sliding window)

load("results.RData")
acc.vec <- c()

for(max in c(250, 500, 750)) {

begin_train <- min(ds_final$datadescarga)
end_train <- AddMonths(begin_train, 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_final$datadescarga)) {
  
  print(end_test)
  
  train <- ds_final[ds_final$datadescarga >= begin_train & ds_final$datadescarga < end_train,]
  test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m<- ranger(formula=metier ~ ., data=train, num.trees = max, importance="impurity")
    p <- predict(m, test)
    # p$predictions
    
    tbl <- table(test$metier,p$predictions)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    print(acc)
    
    results<-rbindlist(list(results,list(end_test,"sliding","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results[nrow(results),c(row)]<-acc_class
      }
      else{
        results[nrow(results),c(row)]<-0
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


save(results,file="results.RData")

### TESTING  Random Forest (growing window)
load("results.RData")
acc.vec <- c()

for(max in c(250, 500, 750)) {
# begin_train <- min(ds_final$datadescarga)
end_train <- AddMonths(min(ds_final$datadescarga), 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_final$datadescarga)) {
  
  print(end_test)
  
  # train <- ds_final[ds_final$datadescarga >= begin_train & ds_final$datadescarga < end_train,]
  train <- ds_final[ds_final$datadescarga < end_train,]
  test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m <- ranger(formula=metier ~ ., data=train,num.trees = max, importance="impurity")
    p <- predict(m, test)
    # p$predictions
    
    tbl <- table(test$metier,p$predictions)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    print(acc)
    
    results<-rbindlist(list(results,list(end_test,"growing","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results[nrow(results),c(row)]<-acc_class
      }
      else{
        results[nrow(results),c(row)]<-0
      }
      
    }
    
    acc.vec <- c(acc.vec, acc)
   
    
  }
  
  # begin_train <- AddMonths(begin_train, 3)
  end_train <- AddMonths(end_train, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}
}

lines(acc.vec, col=34)

save(results,file="results.RData")

### TESTING  Random Forest (full window)

load("results.RData")
load("ds_final.RData")
acc.vec <- c()


for(max in c(250, 500, 750)) {
#begin_train <- min(ds_final$datadescarga)
end_train <- min(ds_final$datadescarga)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_final$datadescarga)) {
  
  print(end_test)
  
  train <- ds_final[ds_final$datadescarga < end_train | ds_final$datadescarga >= end_test,]
  test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m<- ranger(formula=metier ~ ., data=train, num.trees = max,importance="impurity")
    p <- predict(m, test)
    # p$predictions
    
    tbl <- table(test$metier,p$predictions)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    print(acc)
    
    results<-rbindlist(list(results,list(end_test,"full","Random Forest",paste("max=",max),acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results[nrow(results),c(row)]<-acc_class
      }
      else{
        results[nrow(results),c(row)]<-0
      }
      
    }
    
    acc.vec <- c(acc.vec, acc)
    
    
  }
  
  #begin_train <- AddMonths(begin_train, 1)
  end_train <- AddMonths(end_train, 3)
  end_test <- AddMonths(end_test, 3)
  
  
}
}


plot(acc.vec, type="l", ylim=c(0.75,1))
save(results,file="results.RData")

### TESTING Decision Tree (sliding window)

acc.vec <- c()

begin_train <- min(ds_final$datadescarga)
end_train <- AddMonths(begin_train, 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_final$datadescarga)) {
  
  print(end_test)
  
  train <- ds_final[ds_final$datadescarga >= begin_train & ds_final$datadescarga < end_train,]
  test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(rpart)
    m <-rpart(formula = metier ~ . , data = train, control = rpart.control(maxdepth = 10))
    p <- predict(m, test, type="class")
    # p$predictions
    
    tbl <- table(test$metier,p)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    print(acc)
    
    results<-rbindlist(list(results,list(end_test,"sliding","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results[nrow(results),c(row)]<-acc_class
      }
      else{
        results[nrow(results),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
   
    
    
  }
  
  begin_train <- AddMonths(begin_train, 1)
  end_train <- AddMonths(end_train, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}

plot(acc.vec, type="l", ylim=c(0.75,1))

save(results,file="results.RData")

### TESTING  Decision Tree (growing window)

acc.vec <- c()

# begin_train <- min(ds_final$datadescarga)
end_train <- AddMonths(min(ds_final$datadescarga), 24)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_final$datadescarga)) {
  
  print(end_test)
  
  # train <- ds_final[ds_final$datadescarga >= begin_train & ds_final$datadescarga < end_train,]
  train <- ds_final[ds_final$datadescarga < end_train,]
  test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(ranger)
    m<-rpart(formula = metier ~ . , data = train, control = rpart.control(maxdepth = 10))
    p <- predict(m, test, type="class")
    # p$predictions
    
    tbl <- table(test$metier,p)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    print(acc)
    
    results<-rbindlist(list(results,list(end_test,"growing","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results[nrow(results),c(row)]<-acc_class
      }
      else{
        results[nrow(results),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
  
    
    
  }
  
  # begin_train <- AddMonths(begin_train, 3)
  end_train <- AddMonths(end_train, 1)
  end_test <- AddMonths(end_test, 1)
  
  
}

save(results,file="results.RData")

### TESTING Decision Tree (full window)
load("results.RData")
acc.vec <- c()

#begin_train <- min(ds_final$datadescarga)
end_train <- min(ds_final$datadescarga)
end_test <- AddMonths(end_train, 3)

while(end_test < max(ds_final$datadescarga)) {
  
  print(end_test)
  
  train <- ds_final[ds_final$datadescarga < end_train | ds_final$datadescarga >= end_test,]
  test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
  
  train <- train %>% select(-iddescarga, -datadescarga);
  test <- test %>% select(-iddescarga, -datadescarga);
  
  if(nrow(test)!=0) {
    
    library(rpart)
    m <-rpart(formula = metier ~ . , data = train, control = rpart.control(maxdepth = 10))
    p <- predict(m, test, type="class")
    # p$predictions
    
    tbl <- table(test$metier,p)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
    print(acc)
    
    results<-rbindlist(list(results,list(end_test,"full","Decision Tree","",acc,"","","","","","","","","","","","","")))
    for (row in rownames(tbl)){
      if(any(colnames(tbl)==row)){
        acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
        results[nrow(results),c(row)]<-acc_class
      }
      else{
        results[nrow(results),c(row)]<-0
      }
      
    }
    acc.vec <- c(acc.vec, acc)
    
    
    
  }
  
  #begin_train <- AddMonths(begin_train, 1)
  end_train <- AddMonths(end_train, 3)
  end_test <- AddMonths(end_test, 3)
  
  
}

lines(acc.vec, col=34)

save(results,file="results.RData")


### TESTING Boosting (sliding window)

library(rpart)
library(xgboost)
acc.vec <- c()
load("ds_final.RData")

for (max in c(2,10,20)){
  print(paste("max:",max))
  for (r in c(50,75,100)){
    print(paste("r:",r))
    for (early in c(20)){
      begin_train <- min(ds_final$datadescarga)
      end_train <- AddMonths(begin_train, 24)
      end_test <- AddMonths(end_train, 3)
      while(end_test < max(ds_final$datadescarga)) {
        print(end_test)
        metiers<-ds_final[ds_final$datadescarga >= begin_train & ds_final$datadescarga < end_test,]$metier
        train <- ds_final[ds_final$datadescarga >= begin_train & ds_final$datadescarga < end_train,]
        test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
        
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
          load("results.RData")
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
          results<-rbindlist(list(results,list(end_test,"sliding","Boosting",
                                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results[nrow(results),c(row)]<-acc_class
            }
            else{
              results[nrow(results),c(row)]<-0
            }
            
          }
          save(results,file="results.RData")
        }
        begin_train <- AddMonths(begin_train, 1)
        end_train <- AddMonths(end_train, 1)
        end_test <- AddMonths(end_test, 1)
      }
      
    }
  }
}

plot(acc.vec, type="l", ylim=c(0.75,1))

save(results,file="results.RData")


### TESTING Boosting (growing window)

library(rpart)
library(xgboost)
acc.vec <- c()
load("ds_final.RData")

for (max in c(2,10,20)){
  print(paste("max:",max))
  for (r in c(50,75,100)){
    print(paste("r:",r))
    for (early in c(20)){
      #begin_train <- min(ds_final$datadescarga)
      end_train <- AddMonths(min(ds_final$datadescarga), 24)
      end_test <- AddMonths(end_train, 3)
      while(end_test < max(ds_final$datadescarga)) {
        print(end_test)
        metiers<-ds_final[ds_final$datadescarga < end_test,]$metier
        train <- ds_final[ds_final$datadescarga < end_train,]
        test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
        
        train <- train %>% select(-iddescarga, -datadescarga);
        test <- test %>% select(-iddescarga, -datadescarga);
        if(nrow(test)!=0){
          train_x <- data.matrix(train[, -1])
          train_y <- as.integer(train$metier)-1
          
          test_x <- data.matrix(test[, -1])
          test_y <- as.integer(test$metier)-1
          
          xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
          xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
          
          num_class = length(levels(metiers))
          load("results.RData")
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
          results<-rbindlist(list(results,list(end_test,"growing","Boosting",
                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results[nrow(results),c(row)]<-acc_class
            }
            else{
              results[nrow(results),c(row)]<-0
            }
            
          }
          save(results,file="results.RData")
        }
        #begin_train <- AddMonths(begin_train, 1)
        end_train <- AddMonths(end_train, 1)
        end_test <- AddMonths(end_test, 1)
      }
      
    }
  }
}
plot(acc.vec, type="l", ylim=c(0.75,1))
 
save(results,file="results.RData")

################

### TESTING Boosting (full window)

library(rpart)
library(xgboost)
acc.vec <- c()
load("ds_final.RData")

for (max in c(2,10,20)){
  print(paste("max:",max))
  for (r in c(50,75,100)){
    print(paste("r:",r))
    for (early in c(20)){
      #begin_train <- min(ds_final$datadescarga)
      end_train <- min(ds_final$datadescarga)
      end_test <- AddMonths(end_train, 3)
      while(end_test < max(ds_final$datadescarga)) {
        print(end_test)
        metiers<-ds_final$metier
        train <- ds_final[ds_final$datadescarga < end_train | ds_final$datadescarga >=end_test,]
        test <- ds_final[ds_final$datadescarga >= end_train & ds_final$datadescarga < end_test,]
        
        train <- train %>% select(-iddescarga, -datadescarga);
        test <- test %>% select(-iddescarga, -datadescarga);
        if(nrow(test)!=0){
          train_x <- data.matrix(train[, -1])
          train_y <- as.integer(train$metier)-1
          
          test_x <- data.matrix(test[, -1])
          test_y <- as.integer(test$metier)-1
          
          xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
          xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
          
          num_class = length(levels(metiers))
          load("results.RData")
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
          results<-rbindlist(list(results,list(end_test,"full","Boosting",
                                               paste("max_depth=",paste(max,paste(",nrounds=",r))),
                                               acc,"","","","","","","","","","","","","")))
          
          for (row in rownames(tbl)){
            if(any(colnames(tbl)==row)){
              acc_class<-tbl[c(row),c(row)]/nrow(test[test$metier==row,])
              results[nrow(results),c(row)]<-acc_class
            }
            else{
              results[nrow(results),c(row)]<-0
            }
            
          }
          save(results,file="results.RData")
        }
        #begin_train <- AddMonths(begin_train, 1)
        end_train <- AddMonths(end_train, 3)
        end_test <- AddMonths(end_test, 3)
      }
      
    }
  }
}
plot(acc.vec, type="l", ylim=c(0.75,1))

save(results,file="results.RData")


