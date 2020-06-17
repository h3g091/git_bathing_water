{library(dplyr)
library(glmnet)
library(purrr)
library(tidyverse)
#?glmnet
library(coefplot) #for extracing non 0 coef
#install.packages("tidyverse")
library(tidyverse)
library(pROC)
library(relaxo)
# splits dataset in train and test 
#gets rid of all e_coli == NA as well as all cols that have NA in it 
#(not rows, because of otherwise we do not have enough datapoints left)
#returns df: train and test & dgcMatrix: train_sparse, test_sparse, and list that has all rows that are in test 
#need to rebuild date
build_test_train <-function(original_data, bacteria = T){
  
  #data_processed <- data_ilz%>%
  #  select(-X1) 
  data_processed <- original_data%>%
    select(-X1) 
  
  data_processed <- as.tibble(data_processed)
  data_processed[-1] <- apply(data_processed[-1],2,as.numeric)
  #log10 ecoli, drop entero, drop all missing e.coli
  e_col <- data_processed%>%
    rename(log10_bacteria = e.coli)%>%
    select(-c(entero))%>%
    drop_na(log10_bacteria)
  
  #drop all empty cols
  e_col<-  e_col[, colSums(is.na(e_col)) != nrow(e_col)]
  
  entero <- data_processed%>%
    rename(log10_bacteria=entero)%>%
    select(-c(e.coli))%>%
    drop_na(log10_bacteria)
  
  entero<-  entero[, colSums(is.na(entero)) != nrow(entero)]
  #glimpse(e_col)
  
  #change here to train either entero or e_col bacteria ==F --> entero
  
  some_df <- data.matrix(e_col)
  
  if(bacteria == T) {
    some_df <- data.matrix(e_col)
    cols_with_na<-!colSums(is.na(some_df))
    
    some_df<-as.data.frame(some_df)
    some_df<-(some_df[cols_with_na])
  } else {
    some_df <- data.matrix(entero)
    cols_with_na<-!colSums(is.na(some_df))
    
    some_df<-as.data.frame(some_df)
    some_df<-(some_df[cols_with_na])
  }
 
  
  set.seed(2)
  split <- sample(nrow(some_df), floor(0.7*nrow(some_df)))
  split
  
  train <- some_df[split,]
  
  test <- some_df[-split,]
  
  #train <- e_col[split,]
  
  #test <- e_col[-split,]
  #train <-as.data.frame( some_df[split,] )
  #test <- as.data.frame(some_df[-split,] )
  
  
  #be careul, global option
  # na.action="na.pass"
  #train_sparse <- sparse.model.matrix(~., train[,3:ncol(train)])
  #test_sparrse <- sparse.model.matrix(~., test[,3:ncol(test)] )
  
  
  train_sparse <- sparse.model.matrix(train$log10_bacteria~., train[,3:ncol(train)]) #data must be dataframe
  
  #l<-is.na(train)
  #isTRUE(l) recheck if false
  
  
  #train_sparse
  test_sparrse <- sparse.model.matrix(test$log10_bacteria~., test[,3:ncol(test)] ) #data must be dataframe
  some_df$datum <- as.Date(some_df$datum, origin="1970-01-01")
  glimpse(some_df)
  return(list(train, train_sparse, test, test_sparrse, split, some_df))
}

get_coef_1se_cv <- function(df){
  tmp_coeffs <- coef(df, s = "lambda.1se")
  a <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
  return(a)
}
get_coef_min_cv <- function(df){
  tmp_coeffs <- coef(df, s = "lambda.min")
  a <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
  return(a)
}

get_coef_fixed_lambda <- function(df,lambda){
  tmp_coeffs <- coef(df, s = lambda)
  a <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
  return(a)
}

#get numbers temp system boundary

}
#not standardized yet
data_ilz <- read_csv("/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/Masterarbeit/Dataframes_processed_heiko/ilz.csv")
data_isar<- read_csv("/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/Masterarbeit/Dataframes_processed_heiko/isar.csv")

#already standardized  in feature building
data_ilz_stand <- read_csv("/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/Masterarbeit/Dataframes_processed_heiko/ilz_stand.csv")
data_isar_stand<- read_csv("/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/Masterarbeit/Dataframes_processed_heiko/isar_stand.csv")


data_ilz_stand_new <- read_csv("/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/Masterarbeit/Dataframes_processed_heiko/ilz_stand_new.csv")
data_isar_stand_new <- read_csv("/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/Masterarbeit/Dataframes_processed_heiko/isar_stand_new.csv")
#change data to 
train_test_data <- build_test_train(data_ilz)
train_test_data <- build_test_train(data_isar)

train_test_data <- build_test_train(data_ilz_stand)
train_test_data <- build_test_train(data_isar_stand)
train_test_data <- build_test_train(data_ilz_stand_new)

train <- (train_test_data[[1]])
train_sparse <- (train_test_data[[2]])
test <-(train_test_data[[3]])
test_sparse <- (train_test_data[[4]])
split <- train_test_data[[5]]
processed_data <-train_test_data[[6]]





#lasso alpha =1 : cross validation, standardize,  --> 4 possibles
#test_shit
{
  #fit_test <- glmnet(train_sparse, train$log10_bacteria, na.rm =T, standardize = F, lambda = fit_lasso_base_cross$lambda.min)#gets min lambda
  #fit_test <- glmnet(train_sparse, train$log10_bacteria, na.rm =T, standardize = F, lambda = fit_lasso_base_cross$lambda.1se)
  
  #a<-coef(fit_test) #gets min lambda
  #l<-a != 0
  #a[l]
}


#fit_relaxed_lasso_base <- relaxo(processed_data[-1], processed_data$log10_bacteria)

#ohne standartisieren
fit_lasso_base <- glmnet(train_sparse, train$log10_bacteria, na.rm =T, standardize = F, alpha = 1)
fit_lasso_base_cross <- cv.glmnet(train_sparse, train$log10_bacteria,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3,standardize = F)#--> alpha =1:  lasso regressio




fit_lasso_base_stand <- glmnet(train_sparse, train$log10_bacteria, na.rm =T, standardize = T, alpha =1)
fit_lasso_base_cross_stand <- cv.glmnet(train_sparse, train$log10_bacteria,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3,standardize = T)#--> alpha =1:  lasso regressio



fit_elnet_base <- glmnet(train_sparse, train$log10_bacteria, na.rm =T, standardize = F, alpha = 0.5)
fit_elnet_base_cross <- cv.glmnet(train_sparse, train$log10_bacteria,type.measure="mse", alpha=0.5, family="gaussian",  nfolds = 3,standardize = F)#--> alpha =1:  elnet regressio



fit_elnet_base_stand <- glmnet(train_sparse, train$log10_bacteria, na.rm =T, standardize = T, alpha = 0.5)
fit_elnet_base_cross_stand <- cv.glmnet(train_sparse, train$log10_bacteria,type.measure="mse", alpha=0.5, family="gaussian",  nfolds = 3,standardize = T)#--> alpha =1:  elnet regressio




par(mfrow=c(2,2))

plot(fit_lasso_base, xvar="lambda", label = T, main = "lasso_base")
plot(fit_lasso_base_cross,main="LASSO")

plot(fit_lasso_base_stand, xvar="lambda", label = T, main = "lasso_base_stand")
plot(fit_lasso_base_cross_stand,main="LASSO")

plot(fit_elnet_base, xvar="lambda", label = T, main = "elnet_base")
plot(fit_elnet_base_cross,main="elnet")

plot(fit_elnet_base_stand, xvar="lambda", label = T, main = "elnet_base_stand")
plot(fit_elnet_base_cross_stand,main="elnet")

#standardize errorcheck


#max(abs(fit_lasso_base$beta - fit_lasso_base_stand$beta))
#max(abs(fit_lasso_base_cross$beta - fit_lasso_base_cross_stand$beta))
#get number of features that are not -0 by lambda nzero

fit_elnet_base_cross_stand$nzero
fit_lasso_base_cross_stand$nzero
l<-fit_lasso_base_cross_stand$nzero==5
k<-fit_elnet_base_cross_stand$nzero==5
fit_lasso_base_cross_stand$nzero[l]
fit_elnet_base_cross_stand$nzero[k]
#immer number +1

fit_lasso_base_cross_stand$nzero[23]
fit_elnet_base_cross_stand$nzero[21]

extract.coef(fit_lasso_base_cross_stand,lambda = fit_lasso_base_cross_stand$lambda[32])
extract.coef(fit_elnet_base_cross_stand,lambda = fit_elnet_base_cross_stand$lambda[20])




#file_spec <- "_ilz_not_pre_standardized"

get_coef_1se_cv(fit_lasso_base_cross)
get_coef_1se_cv(fit_lasso_base_cross_stand)

get_coef_1se_cv(fit_elnet_base_cross)
get_coef_1se_cv(fit_elnet_base_cross_stand)


get_coef_min_cv(fit_lasso_base_cross)
get_coef_min_cv(fit_lasso_base_cross_stand)

get_coef_min_cv(fit_elnet_base_cross)
get_coef_min_cv(fit_elnet_base_cross_stand)




#printing
{
get_coef_1se_cv(fit_lasso_base_cross)
{
a<-get_coef_1se_cv(fit_lasso_base_cross)
filename<- paste("lasso_base_1se_cv", file_spec, ".txt", sep = "")

merged <-a

#merged <-merge(m, df2, by = "row.names", all = TRUE)
names(merged)[1]<-filename



#write.table(a,filename, sep = "\t",
#            row.names = TRUE, col.names = NA)
}


get_coef_min_cv(fit_lasso_base_cross)

a<-get_coef_min_cv(fit_lasso_base_cross)
filename<- paste("lasso_base_min_cv", file_spec, ".txt", sep = "")

merged <-merge(merged, a, by = "row.names", all = TRUE)
merged = subset(merged, select=-(Row.names))

names(merged)[3]<-filename

#write.table(a,filename, sep = "\t",
  #          row.names = TRUE, col.names = NA)



a<-get_coef_1se_cv(fit_lasso_base_cross_stand)
filename<- paste("lasso_base_1se_cv_stand", file_spec, ".txt", sep = "")

merged <-merge(merged, a, by = "row.names", all = TRUE)
merged = subset(merged, select=-(Row.names))
names(merged)[5]<-filename

#write.table(a,filename, sep = "\t",
 #           row.names = TRUE, col.names = NA)

a<-get_coef_1se_cv(fit_lasso_base_cross_stand)
filename<- paste("lasso_base_min_cv_stand", file_spec, ".txt", sep = "")
merged <-merge(merged, a, by = "row.names", all = TRUE)
merged = subset(merged, select=-(Row.names))
names(merged)[7]<-filename

#write.table(a,filename, sep = "\t",
 #           row.names = TRUE, col.names = NA)



#get_coef_fixed_lambda(fit_lasso_base_cross_stand,"lambda.")
#elnet
get_coef_1se_cv(fit_elnet_base_cross)

a<-get_coef_1se_cv(fit_elnet_base_cross)
filename<- paste("elnet_base_1se_cv", file_spec, ".txt", sep = "")

merged <-merge(merged, a, by = "row.names", all = TRUE)
merged = subset(merged, select=-(Row.names))
names(merged)[9]<-filename

#write.table(a,filename, sep = "\t",
#            row.names = TRUE, col.names = NA)

  
get_coef_min_cv(fit_elnet_base_cross)
  
  a<-get_coef_min_cv(fit_elnet_base_cross)
  filename<- paste("elnet_base_min_cv", file_spec, ".txt", sep = "")
  merged <-merge(merged, a, by = "row.names", all = TRUE)
  merged = subset(merged, select=-(Row.names))
  names(merged)[11]<-filename
  
  #write.table(a,filename, sep = "\t",
    #          row.names = TRUE, col.names = NA)
  
get_coef_1se_cv(fit_lasso_base_cross_stand)
  a<-get_coef_1se_cv(fit_elnet_base_cross_stand)
  filename<- paste("elnet_base_1se_cv_stand", file_spec, ".txt", sep = "")
  merged <-merge(merged, a, by = "row.names", all = TRUE)
  merged = subset(merged, select=-(Row.names))
  names(merged)[13]<-filename
  
  #write.table(a,filename, sep = "\t",
   #           row.names = TRUE, col.names = NA)
get_coef_min_cv(fit_elnet_base_cross_stand)  
  a<-get_coef_min_cv(fit_elnet_base_cross_stand)
  filename<- paste("elnet_base_min_cv_stand", file_spec, ".txt", sep = "")
  merged <-merge(merged, a, by = "row.names", all = TRUE)
  merged = subset(merged, select=-(Row.names))
  names(merged)[15]<-filename
  
  #write.table(a,filename, sep = "\t",
   #           row.names = TRUE, col.names = NA)
  
}  
#get_coef_1se_cv(fit_elnet_base_cross)
#get_coef_min_cv(fit_elnet_base_cross)

#get_coef_1se_cv(fit_lasso_base_cross_stand)
#get_coef_min_cv(fit_lasso_base_cross_stand)




#old  
fit.lasso_stand <- glmnet(train_sparse, train$log10_bacteria, family="gaussian", alpha=1, standardize = T)#--> alpha =1:  lasso regressio
fit.lasso <- glmnet(train_sparse, train$log10_bacteria, family="gaussian", alpha=1, standardize = F)#--> alpha =1:  lasso regressio
  #plot(fit.lasso_stand)
  #plot(fit.lasso)
  
  
  fit.lassocross <- cv.glmnet(train_sparse, train$log10_bacteria,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3,standardize = F)#--> alpha =1:  lasso regressio
  fit.lassocross_stand <- cv.glmnet(train_sparse, train$log10_bacteria,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3, standardize = T)#--> alpha =1:  lasso regressio
  par(mfrow=c(2,1))
  plot(fit.lassocross)
  plot(fit.lassocross_stand)
  
  extract.coef(fit.lassocross)
  extract.coef(fit.lassocross_stand)
  
  fit.ridge <- glmnet(train_sparse, train$log10_bacteria, family="gaussian", alpha=0) #--> alpha =0:  ridge regressio
  fit.elnet <- glmnet(train_sparse, train$log10_bacteria, family="gaussian", alpha=.5) #elastic net
  
  for (i in 0:10) {
    assign(paste("fit", i, sep=""), cv.glmnet(train_sparse, train$log10_bacteria, type.measure="mse", 
                                              alpha=i/10,family="gaussian", nfolds = 7))
  }
  
  #cv1 <- cv.glmnet(train_sparse, train_ecol[,2], nfolds = 7)
  
  
  par( mfrow=c(3,2))
  #par(mfrow=c(2,1))
  
  plot(fit.lasso, xvar="lambda", label = T,  main='ss')
  #plot(fit.lasso,xvar="dev", main="LASSO")
  plot(fit10, main="LASSO")
  
  plot(fit.ridge, xvar="lambda",label = T)
  plot(fit0, main="Ridge")
  
  plot(fit.elnet, xvar="lambda",label = T)
  plot(fit5, main="Elastic Net")
  
  
  
  
  yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=test_sparrse) #ridge #lambda.min
  yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=test_sparrse)
  yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=test_sparrse)
  yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=test_sparrse)
  yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=test_sparrse)
  yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=test_sparrse)
  yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=test_sparrse)
  yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=test_sparrse)
  yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=test_sparrse)
  yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=test_sparrse)
  yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=test_sparrse) #lasso
  
  
  
  mse_error0 <- sum(sqrt(((yhat0- test$log10_bacteria)**2)))
  
  
  
  mse_error1 <- sum(sqrt(((yhat1- test$log10_bacteria)**2)))
  mse_error2 <- sum(sqrt(((yhat2- test$log10_bacteria)**2)))
  mse_error3 <- sum(sqrt(((yhat3- test$log10_bacteria)**2)))
  mse_error4 <- sum(sqrt(((yhat4- test$log10_bacteria)**2)))
  mse_error5 <- sum(sqrt(((yhat5- test$log10_bacteria)**2)))
  mse_error6 <- sum(sqrt(((yhat6- test$log10_bacteria)**2)))
  mse_error7 <- sum(sqrt(((yhat7- test$log10_bacteria)**2)))
  mse_error8 <- sum(sqrt(((yhat8- test$log10_bacteria)**2)))
  mse_error9 <- sum(sqrt(((yhat9- test$log10_bacteria)**2)))
  mse_error10 <- sum(sqrt(((yhat10- test$log10_bacteria)**2)))
  #error6 <- sum(sqrt(((pred6- test[2])**2)))
  
  error_list <-c(mse_error0, mse_error1, mse_error2,mse_error3,mse_error4,mse_error5,mse_error6,mse_error7,mse_error8,mse_error9,mse_error10)
  error_list
  minimum<-min(error_list)
  m<-"mse_error0, mse_error1, mse_error2,mse_error3,mse_error4,mse_error5,mse_error6,mse_error7,mse_error8,mse_error9,mse_error10"
  n<-(strsplit(m,split = ","))
  l <- n[[1]]
  error_df <- data.frame(l, error_list)
  error_df
  minimum



}


