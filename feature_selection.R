######### end test date 2011-01-04 
#### 15% de 180 variaveis =


# ######Boosting feature importance max=10 e r=50
# importance_matrix = xgb.importance(colnames(xgb_train), model = xgb.fit)
# 
# importance_matrix$Feature
# save(importance_matrix,file="importance_matrix.RData")
# 
# ######## Random Forest importance max_depth=250
# 
# importance_matrix_random_forest<-importance(m)
# save(importance_matrix_random_forest,file="importance_matrix_random_forest.RData")

library(Boruta)

features<-ds_feature %>% select(-iddescarga, -datadescarga,-l);

feature_importance_boruta<-Boruta(metier ~ .,data=features)

save(feature_importance_boruta,file="feature_importance_boruta.RData")

#boruta_correct<-TentativeRoughFix(feature_importance_boruta)

plot(feature_importance_boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(feature_importance_boruta$ImpHistory),function(i)
  feature_importance_boruta$ImpHistory[is.finite(feature_importance_boruta$ImpHistory[,i]),i])
names(lz) <- colnames(feature_importance_boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(feature_importance_boruta$ImpHistory), cex.axis = 0.7)

getSelectedAttributes(feature_importance_boruta, withTentative = F)

###########


columns<- c("method","n","acc","TP")



results_feature_feature_selection<- data.frame(matrix(nrow = 0, ncol = length(columns))) 

# assign column names
colnames(results_feature_feature_selection) = columns

#change the class of attributes

for(i in 2:4){
  results_feature_feature_selection[,i]<-as.numeric(results_feature_feature_selection[,i])
}


save(results_feature_feature_selection,file="results_feature_feature_selection.RData")



#############

acc.vec <- c()
acc2.vec <- c()
begin_test <- min(ds_feature$datadescarga)
end_test <- AddMonths(begin_test, 3)

confusion_matrix_Boosting_full_feature_selection<-matrix(0, ncol = 13,nrow = 13) 
colnames(confusion_matrix_Boosting_full_feature_selection)<-levels(ds_feature$metier)
rownames(confusion_matrix_Boosting_full_feature_selection)<-levels(ds_feature$metier)

confusion_matrix_Random_Forest_full_feature_selection<-matrix(0, ncol = 13,nrow = 13) 
colnames(confusion_matrix_Random_Forest_full_feature_selection)<-levels(ds_feature$metier)
rownames(confusion_matrix_Random_Forest_full_feature_selection)<-levels(ds_feature$metier)


###################


for(p in 10:90){
  print(p)
  n<-(p/100)*ncol(ds_feature)
  
  confusion_matrix_Boosting_full_feature_selection<-matrix(0, ncol = 13,nrow = 13) 
  colnames(confusion_matrix_Boosting_full_feature_selection)<-levels(ds_feature$metier)
  rownames(confusion_matrix_Boosting_full_feature_selection)<-levels(ds_feature$metier)
  
  confusion_matrix_Random_Forest_full_feature_selection<-matrix(0, ncol = 13,nrow = 13) 
  colnames(confusion_matrix_Random_Forest_full_feature_selection)<-levels(ds_feature$metier)
  rownames(confusion_matrix_Random_Forest_full_feature_selection)<-levels(ds_feature$metier)
  
  begin_test <- min(ds_feature$datadescarga)
  end_test <- AddMonths(begin_test, 3)

while(end_test < max(ds_feature$datadescarga)) {
  
  print(end_test)
  metiers<-ds_feature$metier
  train <-ds_feature[ds_feature$datadescarga < begin_test | ds_feature$datadescarga >= end_test, ]
  test <-ds_feature[ds_feature$datadescarga >= begin_test & ds_feature$datadescarga < end_test, ]
  
  train <- train %>% select(-iddescarga, -datadescarga,-l);
  test <- test %>% select(-iddescarga, -datadescarga,-l);
  
  if(nrow(test)!=0) {
    
   
    #train1 <- train %>% select(metier, rownames(as.matrix(sort(importance_matrix_random_forest,decreasing = TRUE)[1:n])))
    #test1 <- test %>% select(metier, rownames(as.matrix(sort(importance_matrix_random_forest,decreasing = TRUE)[1:n])))
    train<- train%>% select(metier, rownames( as.matrix(feature_importance_boruta$finalDecision[feature_importance_boruta$finalDecision=="Confirmed"])))
    test<- test %>% select(metier, rownames( as.matrix(feature_importance_boruta$finalDecision[feature_importance_boruta$finalDecision=="Confirmed"])))
    
    
    library(ranger)
    m <- ranger(formula=metier ~ ., data=train,num.trees = 500, importance="impurity")
    p <- predict(m, test)
    # p$predictions
    
    tbl <- table(test$metier,p$predictions)
    acc <- sum(diag(tbl))/nrow(test) # accuracy
   # print(acc)
    
    
    for(i in 1:nrow(test)){
      
      confusion_matrix_Random_Forest_full_feature_selection[test$metier[i],p$predictions[i]]<-confusion_matrix_Random_Forest_full_feature_selection[test$metier[i],p$predictions[i]]+1
    }
    
    acc.vec <- c(acc.vec, acc)
    
    #train <- train %>% select(metier, importance_matrix$Feature[1:n] )
    #test <- test %>% select(metier, importance_matrix$Feature[1:n] )
    
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
   # print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*acc)))
    #print(paste(max,paste(g,paste(r,early))))
    
    tbl<-table(xgb.pred$label,xgb.pred$prediction)
    acc2.vec <- c(acc2.vec, acc)
    
    
    
    for(i in 1:nrow(test)){
      
      confusion_matrix_Boosting_full_feature_selection[xgb.pred$label[i],xgb.pred$prediction[i]]<-confusion_matrix_Boosting_full_feature_selection[xgb.pred$label[i],xgb.pred$prediction[i]]+1
    }
    
    
  }
  
  
  begin_test <- AddMonths(begin_test, 3)
  end_test <- AddMonths(end_test, 3)
  
  
}
  ###########
  sum<-0
  
  for(i in 1:13){
    sum<-sum+(confusion_matrix_Random_Forest_full_feature_selection[i,i]/sum(confusion_matrix_Random_Forest_full_feature_selection[i,]))
    
  }
  acc <- sum(diag(confusion_matrix_Random_Forest_full_feature_selection))/nrow(ds_feature)
  results_feature_feature_selection<-rbindlist(list(results_feature_feature_selection,list("Random Forest",n,acc,sum)))
  print(acc)
                                                    
  sum<-0
  
  for(i in 1:13){
    sum<-sum+(confusion_matrix_Boosting_full_feature_selection[i,i]/sum(confusion_matrix_Boosting_full_feature_selection[i,]))
    
  }
  acc <- sum(diag(confusion_matrix_Boosting_full_feature_selection))/nrow(ds_feature)
  results_feature_feature_selection<-rbindlist(list(results_feature_feature_selection,list("Boosting",n,acc,sum)))
  print(acc)                                               
 
  #########
  
 # save(confusion_matrix_Boosting_full_feature_selection,file=paste("Feature_selection/confusion_matrix_Boosting_full_feature_selection",p,sep = "_"))
 # save(confusion_matrix_Random_Forest_full_feature_selection,file=paste("Feature_selection/confusion_matrix_Random_Forest_full_feature_selection",p,sep = "_"))
}
#plot(acc.vec, type="l", ylim=c(0.75,1))
#lines(acc2.vec,col="red")


save(results_feature_feature_selection,file="results_feature_feature_selection.RData")

save(confusion_matrix_Boosting_full_feature_selection,file="confusion_matrix_Boosting_full_feature_selection.RData")
save(confusion_matrix_Random_Forest_full_feature_selection,file="confusion_matrix_Random_Forest_full_feature_selection.RData")

