#' Resampling Strategies for Imbalanced Classification
#' 
#' Provides an interface to resampling strategies for imbalanced classification tasks from other R packages.
#' 
#' @author Nuno Moniz
#'

########################################################

#' Edited Nearest Neighbour
#'
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.ENN(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.ENN(form, PimaIndiansDiabetes, k=1)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.ENN <- function(form, train, k=3, ...) {
  
  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")
  
  new.ds <- UBL::ENNClassif(form = form, dat = train, k = k, dist = distance, ...)[[1]]
  
  new.ds
  
}




########################################################

#' Random Undersampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL RandUnderClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.RandUnder(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.RandUnder(form, PimaIndiansDiabetes, und.perc=0.95)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.RandUnder <- function(form, train, und.perc=0.5, ...) {
  
  nms<-c( "LLS-PD", "LHP-CEF","LHP-PB")
  
  
  
  lst<-as.list(rep((und.perc),length(nms))); names(lst) <- nms
  
  new.ds <- UBL::RandUnderClassif(form = form, dat = train, C.perc = lst, ...)
  
  new.ds
  
}

########################################################

#' Random Oversampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param ove.perc Oversampling percentage for the minority class (e.g. 20% oversampling corresponds to 0.2)
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL RandOverClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.RandOver(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.RandOver(form, PimaIndiansDiabetes, ove.perc=0.95)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' 
rs.RandOver <- function(form, train, ove.perc=0.5, ...) {
 
  
  nms<-c("FPO-PB","LHP-PBC","LLS-DEEP","PS-PB")
  
  lst<-as.list(rep((1+ove.perc),length(nms))); names(lst) <- nms
  
  new.ds <- UBL::RandOverClassif(form = form, dat = train, C.perc = lst, ...)
  
  new.ds
  
}

########################################################


#' Importance Sampling
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class (default is 1)
#' @param ove.perc Oversampling percentage for the minority class (default is 1)
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL ImpSampClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.ImpSamp(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.ImpSamp(form, PimaIndiansDiabetes, und.perc = 0.5)
#' new.ds3 <- rs.ImpSamp(form, PimaIndiansDiabetes, ove.perc = 0.5)
#' new.ds4 <- rs.ImpSamp(form, PimaIndiansDiabetes, und.perc = 0.5, ove.perc = 0.5)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' table(new.ds3$diabetes)
#' table(new.ds4$diabetes)
#' 
rs.ImpSamp <- function(form, train, und.perc = 1, ove.perc = 0, ...) {
  
  nms<-c("FPO-PB","LHP-PBC","LLS-DEEP","PS-PB")
  
  other<-c( "LLS-PD", "LHP-CEF","LHP-PB")
  
  lst<-append(as.list(rep(und.perc,length(other))),as.list(rep((1+ove.perc),length(nms))))

  names(lst) <- append(other,nms)
  
  new.ds <- UBL::WERCSClassif(form = form, dat = train, C.perc = lst)
  
  new.ds
  
}

########################################################

#' Synthetic Minority Over-sampling Technique (SMOTE)
#'
#' @param form A model formula
#' @param train A data.frame object with the training data
#' @param und.perc Undersampling percentage for the majority class (default is 1)
#' @param ove.perc Oversampling percentage for the minority class (default is 1)
#' @param ... 
#'
#' @return
#' @export
#' 
#' @importFrom UBL SmoteClassif
#'
#' @examples
#' library(mlbench)
#' 
#' data(PimaIndiansDiabetes)
#' 
#' form <- diabetes ~ .
#' 
#' new.ds1 <- rs.SMOTE(form, PimaIndiansDiabetes)
#' new.ds2 <- rs.SMOTE(form, PimaIndiansDiabetes, und.perc = 0.5)
#' new.ds3 <- rs.SMOTE(form, PimaIndiansDiabetes, ove.perc = 0.5)
#' new.ds4 <- rs.SMOTE(form, PimaIndiansDiabetes, und.perc = 0.5, ove.perc = 0.5)
#' 
#' table(PimaIndiansDiabetes$diabetes)
#' table(new.ds1$diabetes)
#' table(new.ds2$diabetes)
#' table(new.ds3$diabetes)
#' table(new.ds4$diabetes)
#' 
rs.SMOTE <- function(form, train, und.perc = 1, ove.perc = 0, ...) {
  
  ove<-list_sort_under(ove.per,train)
  und<-list_sort_under(und.perc,train)
  
  lst<-append(ove,und);
  
  distance <- ifelse(any(sapply(train,is.numeric)==FALSE),"HEOM","Euclidean")
  
  new.ds <- UBL::SmoteClassif(form = form, dat = train, C.perc = lst, dist = distance, ...)
  
  new.ds
  
}




new_sample<-function(method,train){
  
  
    train<-as.data.frame(train)
    
    balanced<-NULL
    form<-formula(paste("metier","~."))
    
      
    if(method$Strategy=="rs.ENN"){
      balanced<-rs.ENN(form,train,method$k)
        
    }else if(method$Strategy=="rs.RandOver"){
      balanced<-rs.RandOver(form,train,method$ove.perc)
        
    }else if(method$Strategy=="rs.RandUnder"){
      balanced<-rs.RandUnder(form,train,method$und.perc)
        
    }else if(method$Strategy=="rs.ImpSamp"){
        
      balanced<-rs.ImpSamp(form,train,method$und.perc,method$ove.perc)
        
    }else if(method$Strategy=="rs.SMOTE"){
      balanced<-rs.SMOTE(form,train,method$und.perc,method$ove.perc)
        
    }
      
    
  
  balanced<-as_tibble(balanced)
  print(nrow(balanced))
  return(balanced)
  
}


list_sort_under<- function(under,train) {
  labels<-sort(table(train$metier))[1:4]
  s<- sum(labels)
  lst<-(labels*(1+under))/s
  lst<-as.list(lst)
  
  lst
}

list_sort_over<- function(over,train) {
  nms<-c("FPO-PB","LHP-PBC","LLS-DEEP","PS-PB")
  t<-table(train$metier)
  labels<-t[nms]
  s<- sum(labels)
  lst<-(s/labels)/(1+over)
  lst<-as.list(lst)
  
  lst
}

