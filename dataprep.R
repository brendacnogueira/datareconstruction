library(dplyr)
library(data.table)
library(reshape2)
library(stringr)
library(janitor)

ds <- read.csv("artesmetiers.xlsx - guiml.csv", na.strings = "NULL")

ds[ds$metier=="LHM-CEF",]$metier<-"LHP-CEF"
ds[ds$metier=="LHM-PB",]$metier<-"LHP-PB"

ds <- tibble(ds)

ds <- ds %>% select(-idamostra, -euros, -compff.1, -compsl, -pontalsl, -bocasl) #columns to remove
ds[is.na(ds$classificacao),]$classificacao <- "VAZIO" # preencher valor da classificacao de especies que não têm classificação
ds <- ds %>% na.omit

ds_orig <- ds

# first cleanup
ds <- ds %>% select(-dia, -mes, -ano, -vulgar, -classificacao, -arte, -peso) %>% distinct

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

# matricula
ds$matricula <- str_trim(ds$matricula)
ds_aux <- select(ds, iddescarga, matricula)
matricula <- dcast(ds_aux, formula=iddescarga ~ matricula, length)
matricula[,-1] <- apply(matricula[,-1], 2, FUN=function(x) ifelse(x>0, 1, 0))


# classe_com
ds$classe_com <- str_trim(ds$classe_com)
ds_aux <- select(ds, iddescarga, classe_com)
classe_com <- dcast(ds_aux, formula=iddescarga ~ classe_com, length)
classe_com[,-1] <- apply(classe_com[,-1], 2, FUN=function(x) ifelse(x>0, 1, 0))
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
ds <- ds %>% select(-dia, -mes, -ano, -vulgar, -classificacao, -arte, -peso, -ilha, -porto, -matricula, -classe_com) %>% distinct

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


save(ds_final,file="ds_final.RData")
### TESTING

train <- ds_final[ds_final$datadescarga < "2013-01-01",]
test <- ds_final[ds_final$datadescarga >= "2013-01-01" & ds_final$datadescarga < "2014-01-01",]

train <- train %>% select(-iddescarga, -datadescarga);
test <- test %>% select(-iddescarga, -datadescarga);


### MODELS

library(ranger)
m <- ranger(formula=metier ~ ., data=train)
p <- predict(m, test)
p$predictions

tbl <- table(test$metier,p$predictions)
acc <- sum(diag(tbl))/nrow(test) # accuracy
#80%accuracy for random forest

nb <- naiveBayes(formula=metier ~ ., data = train)
nb_p <- predict(nb, test)
nb_p

tbl_nb <- table(test$metier,nb_p)
acc_nb <- sum(diag(tbl_nb))/nrow(test) 
#accuracy naive bayes 13%


####
# Variable importance
library(vip)
m <- ranger(formula=metier ~ ., data=train, importance="permutation")
p <- predict(m, test)
vip(m,num_features = 20)
?ranger
library(lime)
explainer <- lime(train, m)
explanation <- explain( test[1:4,], explainer, n_labels = 1, n_features = 6)
plot_features(explanation)




###############
