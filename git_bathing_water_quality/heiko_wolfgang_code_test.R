#for new river instanziere this
river_stat_model_save <- data.frame()
river_fmla_save <-list()
river_fb_save <-data.frame()
error_df_algorithms_save<- data.frame(5)
rf_var_df_save <- data.frame(5)
step_var_df_save<- data.frame(5)

lasso_var_lambda_1se_save <- data.frame(5)
elnet_var_lambda_1se_save <- data.frame(5)
lasso_var_lambda_min_save <- data.frame(5)
elnet_var_lambda_min_save<- data.frame(5)
{    #preload packages
{
library(caret)
library(magrittr)
library(randomForest)
library(dplyr)
library(glmnet)
library(purrr)
library(tidyverse)
#?glmnet
library(coefplot) #for extracing non 0 coef
#install.packages("tidyverse")
library(tidyverse)
library(pROC)
library(fhpredict)
library(tidyverse)
library(Boruta)

library(kwb.flusshygiene)
}
  {
    
    first_time_model_train <- 1
    
  rivers <- c("havel")
  river <- "havel"
  #river_paths <- kwb.flusshygiene::get_paths()[paste0(rivers, "data")]
  
  #river_paths <- list(havel = "Y:/SUW_Department/Projects/FLUSSHYGIENE/Data-Work packages/Daten/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv")
  
  river_paths <- list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv" )
  #river_paths <- list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Isar/DATA_preprocessed_csv")

}
  river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
  
  #river <- "Isar"
  
  names(river_data) <- rivers

#if (FALSE)
#turn to False if not wanted   
  #set.seed(5)
  
}
#run here

{
  {
  #set.seed(5)
for (iterations in 1:2) {
  random_number <- set.seed(iterations)
    
  do_only_lin_regression <-T
  do_mcmc <-T
  new_train_test_split <-T
  do_lasso <- F
  do_elnet <- F #takes way to long (2h+)
  do_step <- T
  do_random_forest <- F 
  {
    do_mean_importance_selection<-F
    do_n_features_half <- F
    do_cumdecission <- F
    do_5_selection<-T
  }
  selection <- list()
  fmla <- list()
  fb<-list()
  
  
  
  {  

  
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
  
  prediction_and_mse<- function(model, test, test_bacteria){
    prediction<-predict(model, test)
    return(mean(sqrt((test_bacteria - prediction)^2)))
  }
  
  get_coef_fixed_lambda <- function(df,lambda){
    tmp_coeffs <- coef(df, s = lambda)
    a <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
    return(a)
  }
  
  

  get_formula_variable_names <- function(formula_a,df){ 
  mf <- model.frame(formula_a, data=df)
  mt <- attr(mf, "terms")
  predvarnames <- attr(mt, "term.labels")
  predvarnames
  }
  limit_max_features <- function(fmla_list, data = data){
    for (formla in 1:length(fmla_list)) {
      #formla = 3
      #fmla_list<- list_lasso
      if((length(all.vars(fmla_list[[formla]]))-1)> floor(nrow(data)/2)){ 
        fmla_list <- fmla_list[-formla]
        print("fmla remove because of two many features compared to datapoint")
      }
      return(fmla_list)
    }
  }
  get_feature_selection_coeficient_names_as_formular_1se <- function(algorithm_list){
    #fit_lasso_base_cross
    #algorithm_list<-fit_lasso_base_cross
    coef_1se<- get_coef_1se_cv(algorithm_list)
    if(dim(coef_1se)[1]==1){
      print("only intercept. nothing to model")
    }else{
      coef_name_lambda_1se<-coef_1se$name[-1]
      #a<-str("")
      coefficients<-paste(coef_name_lambda_1se, collapse = " + " )
      
      formel<-paste("log_e.coli ~ ", coefficients)
      formel
      formula_from_selector<-formula(formel)
    }
    return(formula_from_selector)
  }
  get_feature_selection_coeficient_names_as_formular_lambda_min <- function(algorithm_list){
    #algorithm_list<-fit_lasso_base_cross
    coef_lambda_min<- get_coef_min_cv(algorithm_list)
    coef_name_lambda_min<-coef_lambda_min$name[-1]
    #a<-str("")
    coefficients<-paste(coef_name_lambda_min, collapse = " + " )
    formel<-paste("log_e.coli ~ ", coefficients)
    formula_from_selector<-formula(formel)
    return(formula_from_selector)
  } 
#lasso
#build/integrate here into folds to train with same cross validation
#fold1<-train_rows[[1]]

#training_heiko<-data[fold1,]

  calc_t <- function (datalist=river_data$havel, onlysummer) {
    #heiko
    #datalist<- river_data1$havel
    phy_data <- datalist[-1] # Entfernung der Hygienedaten
    
    if(onlysummer==T){
      hyg_df <- subset(datalist[[1]],
                subset = lubridate::month(datum) %in% 5:9) # Filtern nach Sommer, warum hier 5:9 und beim anderen 4:9?
      
      data_summer <- lapply(phy_data, function(df){
        
        df <- subset(df, subset = lubridate::month(datum) %in% 4:9) 
        }
      )
    }  
    
    
    # z_standardize <- function (x) {
    
    #   y = (x - mean(x, na.rm=T))/sd(x, na.rm=T)
    
    # }
    
    log_transorm_rain <- function(df) { #log transforming rain data
      
      for (site in names(df)[-1]) { # every col gets treatment
        
        df2 <- subset(df, select = c("datum", site))
        
        if (grepl("^r_.*",site)) { # rain gets log-transformed and 1/sigma2
          
          df2[[site]] <- log(df2[[site]]+1)
          
          # df2[[site]] <- df2[[site]]/sd(df2[[site]], na.rm=T)
          
        } #else {
        
        #   df[[site]] <- z_standardize(df2[[site]]) # standardize
        
        # }
        
        df[[site]] <- df2[[site]]
        
      }
      
      return(df)
      
    }
    
    data_t <- lapply(data_summer, log_transorm_rain)
    
    result <- append(list(hyg_df), data_t)
    
    names(result) <- names(datalist)
    
    return(result)
    
  }
  ### Anwenden von calc_t auf Inputliste
  
  river_data_ts <- lapply(river_data, function(river_list){
    
    river_ts <- calc_t(river_list, onlysummer = T) # use function
    
    add_meancol <- function (df) { # for rain and i #edit: + ka #2ndedit: + q
      
      prefix <- unique(sub("([a-z])_.*","\\1",names(df)[-1]))
      
      for (pre in prefix) {
        
        df2 <- dplyr::select(df, dplyr::starts_with(pre))
        
        df[,paste0(pre,"_mean")] <- rowMeans(df2, na.rm=T)
        
      }
      
      
      
      return(df)
      
    }
    
    add_sumcol <- function (df) { # originally for ka, but not used
      
      prefix <- unique(sub("([a-z])_.*","\\1",names(df)[-1]))
      
      if (length(df) > 2)
        
        df[,paste0(prefix,"_sum")] <- rowSums(df[,-1], na.rm=T)
      
      return(df)
      
    }
    
    
    
    q_pos <- grep("^q", names(river_ts)[-1])+1
    
    
    if (length(q_pos) == 1)
      
      river_ts[[q_pos]] <- add_meancol(river_ts[[q_pos]])
    
    ka_pos <- grep("^ka", names(river_ts)[-1])+1
    
    if (length(ka_pos) == 1)
      
      river_ts[[ka_pos]] <- add_meancol(river_ts[[ka_pos]])
    
    i_pos <- grep("^i", names(river_ts)[-1])+1
    
    if (length(i_pos) == 1)
      
      river_ts[[i_pos]] <- add_meancol(river_ts[[i_pos]])
    
    r_pos <- grep("^r", names(river_ts)[-1])+1
    
    river_ts[[r_pos]] <- add_meancol(river_ts[[r_pos]])
    
    return(river_ts)
    
  })
  
  
  
  #rm(river_data,calc_t)
  
  #river = "havel"
  pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)"
  riverdata <- river_data_ts[[river]]
  
  # prepare variables out of all cominations (given by pattern)
  
  # variables for interaction get replaced by q_new (remove q_old)
  
  vars1 <- (riverdata[-1] %>% unroll_physical_data() %>%
              
              lapply(names) %>% unlist() %>% unique())[-1]
  
  vars2 <- vars1[stringr::str_detect(vars1, pattern)]
  
  
  
  # prepare formulas
  
  data <- process_model_riverdata(riverdata, c("log_e.coli", vars2)) %>%
    
    dplyr::select(-datum) 
    
  data <- na.omit(data)
  
  data <-data.frame(scale(data))
  
  #data <- data %>% filter(log_e.coli > log10(15)) #why-heiko? genauigkeit test?
  
  
  #Definition of models
  
  # Definition of null and full models
  #stepwise models
  null <- lm(log_e.coli ~ 1, data = data) #model with only 1 variable
  
  full <- lm(log_e.coli ~ .^2, data = data)
  }
  #heiko models
  {
    #heiko
    {
      
      
     
    bacteria<-names(data)[1]
    form<-formula(paste(bacteria," ~ .^2"))
    
    #get_formula_variable_names(form,data)
    #training_heiko_features <- (training_heiko%>% select(-log_e.coli))
    #sparse.model.matrix(form, training_heiko)
    
    #form <- log_e.coli ~ (.)ˆ2
    #training_heiko_features_matrix <- (data.frame.2.sparseMatrix(training_heiko_features))
  
  test_rows <- list()
  datapoints<-seq(1, nrow(data))
   # for (fold in 1:5) {
    #  smp_size <- floor(0.75 * nrow(data))
    #  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    #  train_rows[fold] <- list(train_ind)
    #  foldid[-train_ind]<-fold
    #  }  
  #foldid
  if(new_train_test_split ==T){
  train_rows <- list()
  foldid=sample(rep(seq(5),length=nrow(data)))  #fixes the train/test for cv.glmnet
  train_rows<- list()
  for (fold in 1:5) {
    train_rows[[fold]]<-datapoints[foldid!=fold]  
  }
  }
  
  #table(foldid) 
  
  
  #data$train.id =foldid
  #data
  train <- model.matrix(form, data)
  
  #train <- model.matrix(log_e.coli~.^2, data = data)
  train_sparse <- sparse.model.matrix(form, data) #data must be dataframe
    
    #train_sparse <- sparse.model.matrix(training_heiko$log_e.coli~(.)ˆ2, training_heiko[,3:ncol(training_heiko)]) #data must be dataframe
    #form <- Y ~ (x + y + z)^2
    #testing_heiko<-data[-fold1,]
    
    # test_sparse <- sparse.model.matrix(testing_heiko$log_e.coli~., testing_heiko[,3:ncol(testing_heiko)]) #data must be dataframe
    
    
    
    
  
} 

    #stepwise function
    
  stepwise <- function (river, pattern, data1, null1, full1 ){
 # Definition maximum number of steps
    
    nsteps <- 5 #ifelse(round(nrow(data)/10) < 10, round(nrow(data)/10), 5 )
    
    selection <- list()
    
    fmla <- list()
    
    
    
    # Creating list of candidate models with 1 ...n predictors 
    #split up this piece in stpe and new algorithms/formulars
    
    for(i in 1: nsteps){
      
      
      
      selection[[i]] <- step(null1, data = data1,
                             
                             direction = "forward",
                             
                             list(lower=null1, upper=full1), steps = i)   
      
      
      fmla[[i]] <- as.list(selection[[i]]$call)$formula
      
      
      
    }
    
    #heiko_add_formular to fmla list function function
    #selection[[6]] <- heiko_lm
    #fmla[[6]] <- as.list(selection[[6]]$call)$formula
    step_returns <- list(fmla, selection)
    return(step_returns)
    
  }
  
  if(do_only_lin_regression==T){
   
    paste_together_step_coefficients<-function(var_step_model_function ){
      #var_step_model_function <- var_step_model_3
      #var_step_model_function[1:3]
      #var_step_model_function <- var_step_model_function[[2:length(var_step_model_function)]]
      formel<- var_step_model_function[[1]]
      
      if(length(var_step_model_function)> 1){
        for (entry in 2:length(var_step_model_function)) {
          
          formel<-paste(formel, var_step_model_function[entry], sep = " + ")
          
        }
      }
      return(formel)
    } 
    #instanzieren variables
    elnet_var_lambda_1se<-lasso_var_lambda_1se<-elnet_var_lambda_min<-lasso_var_lambda_min<-data.frame()
    rf_var_df<-step_var_df <- data.frame(5)
    mse_list_rf_1<-mse_list_rf_2<-mse_list_rf_3<-mse_list_rf_4<-mse_list_rf_5<-mse_list_step_1<-    mse_list_step_2<-    mse_list_step_3<-    mse_list_step_4<-    mse_list_step_5 <-mse_list_rf <- list()
    
    for(j in 1:5){
     
       #j=1
      train_bacteria <- data[train_rows[[j]],]$log_e.coli
      train <- data[train_rows[[j]],-1] #without e.coli
      train_rf <- model.matrix(form, data[train_rows[[j]],])
      
      test_bacteria <- data[-c(train_rows[[j]]),]$log_e.coli
      test <- data[-c(train_rows[[j]]),-1]
      test_rf <- model.matrix(form, data[-c(train_rows[[j]]),])
      #train_glmnet <- model.matrix(form, train)
      
      #train <- model.matrix(log_e.coli~.^2, data = data)
      #train_sparse_glmnet <- sparse.model.matrix(form, train) #data must be dataframe
      
      #set name for iteration
      iteration_name<-paste("iterations", iterations,"fold",j,sep = "_")
      
      
    
      
      
      #rfModel<-randomForest(x=train,  y=train_bacteria, xtest = test, ytest =  test_bacteria, importance = T, keep.forest = T )
      rfModel<-randomForest(train_rf, y = train_bacteria, na.rm =T, keep.forest = T) 
      
      imp_rf <- as.data.frame(varImp(rfModel, scale=T))
      
      imp_rf <- data.frame(overall = imp_rf$Overall,
                        names   = rownames(imp_rf))
      imp_rf<-imp_rf[order(imp_rf$overall,decreasing = T),]
      
      build_formula <- function( feature_list,bacteria = "log_e.coli~ " ){
        #feature_list<-imp_rf[1,2]
        formel <- feature_list[1]
        #formel1 <- feature_list[1,]
        if(length(feature_list)>1){
          for (idx in 2: length(feature_list)) {
            #element<-feature_list[2,]
            formel <- paste(formel, feature_list[[idx]], sep=" + ")
          }
        }  
          formel_with_bacteria<- as.formula(paste(bacteria, formel, sep=""))
          formellsit<-c(formel_with_bacteria,formel)
        
        return(formellsit)
      }
      
      
      rf_formula_11<-build_formula(imp_rf[1,2])
      rf_formula_21<-build_formula(imp_rf[1:2,2])
      rf_formula_31<-build_formula(imp_rf[1:3,2])
      rf_formula_41<-build_formula(imp_rf[1:4,2])
      rf_formula_51<-build_formula(imp_rf[1:5,2])
      
      var_rf_model_1 <-  rf_formula_11[[2]]
      var_rf_model_2 <- rf_formula_21[[2]]
      var_rf_model_3 <- rf_formula_31[[2]]
      var_rf_model_4 <- rf_formula_41[[2]]
      var_rf_model_5 <- rf_formula_51[[2]]
      
      

      
      
      #save random forest variables here
      #rf_var_df <- data.frame(var_rf_model_1,var_rf_model_2,var_rf_model_3,var_rf_model_4,var_rf_model_5)
      rf_var_df_fold <- setNames(data.frame(rbind(var_rf_model_1,var_rf_model_2,var_rf_model_3,var_rf_model_4,var_rf_model_5)), iteration_name)
      rf_var_df <- cbind(rf_var_df,rf_var_df_fold)
      
      
      rf_formula_1<- rf_formula_11[[1]]
      rf_formula_2<- rf_formula_21[[1]]
      rf_formula_3<- rf_formula_31[[1]]
      rf_formula_4<- rf_formula_41[[1]]
      rf_formula_5<- rf_formula_51[[1]]
      #build train test for rf_formulas
      train_rf_1 <- model.matrix(rf_formula_1,data[train_rows[[j]],])
      test_rf_1 <- model.matrix(rf_formula_1,data[-c(train_rows[[j]]),])
      
      train_rf_2 <- model.matrix(rf_formula_2,data[train_rows[[j]],])
      test_rf_2 <- model.matrix(rf_formula_2,data[-c(train_rows[[j]]),])
      
      train_rf_3 <- model.matrix(rf_formula_3,data[train_rows[[j]],])
      test_rf_3 <- model.matrix(rf_formula_3,data[-c(train_rows[[j]]),])
      
      train_rf_4 <- model.matrix(rf_formula_4,data[train_rows[[j]],])
      test_rf_4 <- model.matrix(rf_formula_4,data[-c(train_rows[[j]]),])
      
      train_rf_5 <- model.matrix(rf_formula_5,data[train_rows[[j]],])
      test_rf_5 <- model.matrix(rf_formula_5,data[-c(train_rows[[j]]),])
      
      rf_model_1<-randomForest(train_rf_1,y = train_bacteria, na.rm =T, keep.forest = T)
      rf_model_2<-randomForest(train_rf_2,y = train_bacteria, na.rm =T, keep.forest = T)
      rf_model_3<-randomForest(train_rf_3,y = train_bacteria, na.rm =T, keep.forest = T)
      rf_model_4<-randomForest(train_rf_4,y = train_bacteria, na.rm =T, keep.forest = T)
      rf_model_5<-randomForest(train_rf_5,y = train_bacteria, na.rm =T, keep.forest = T)
      
      mse_rf_model_1_fold <- prediction_and_mse(rf_model_1, test = test_rf_1, test_bacteria = test_bacteria)
      mse_rf_model_2_fold <- prediction_and_mse(rf_model_2, test = test_rf_2, test_bacteria = test_bacteria)
      mse_rf_model_3_fold <- prediction_and_mse(rf_model_3, test = test_rf_3, test_bacteria = test_bacteria)
      mse_rf_model_4_fold <- prediction_and_mse(rf_model_4, test = test_rf_4, test_bacteria = test_bacteria)
      mse_rf_model_5_fold <- prediction_and_mse(rf_model_5, test = test_rf_5, test_bacteria = test_bacteria)
      
      mse_list_rf_1<- append(mse_list_rf_1,mse_rf_model_1_fold )
      mse_list_rf_2<- append(mse_list_rf_2,mse_rf_model_2_fold )
      mse_list_rf_3<- append(mse_list_rf_3,mse_rf_model_3_fold )
      mse_list_rf_4<- append(mse_list_rf_4,mse_rf_model_4_fold )
      mse_list_rf_5<- append(mse_list_rf_5,mse_rf_model_5_fold )
      
      null <- lm(log_e.coli ~ 1, data = data[train_rows[[j]],]) #model with only 1 variable
      
      full <- lm(log_e.coli ~ .^2, data = data[train_rows[[j]],])
      
      step_returns <- stepwise(river = river, pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)", data = data[train_rows[[j]],],null, full)
      fmla <- step_returns[[1]]
      selection <- step_returns[[2]]
      
      step_model_1<-step_returns[[2]][[1]]
      step_model_2<-step_returns[[2]][[2]]
      step_model_3<-step_returns[[2]][[3]]
      step_model_4<-step_returns[[2]][[4]]
      step_model_5<-step_returns[[2]][[5]]
     
      
      var_step_model_1<- names(selection[[1]]$coefficients)[-1]
      var_step_model_2<-names(selection[[2]]$coefficients)[-1]
      var_step_model_3<-names(selection[[3]]$coefficients)[-1]
      var_step_model_4<-names(selection[[4]]$coefficients)[-1]
      var_step_model_5<-names(selection[[5]]$coefficients)[-1]
      
      
      
      var_step_model_1<- paste_together_step_coefficients(var_step_model_1)
      var_step_model_2<-paste_together_step_coefficients(var_step_model_2)
      var_step_model_3<-paste_together_step_coefficients(var_step_model_3)
      var_step_model_4<-paste_together_step_coefficients(var_step_model_4)
      var_step_model_5<-paste_together_step_coefficients(var_step_model_5)
      
      step_var_df_fold <- setNames(data.frame(rbind(var_step_model_1,var_step_model_2,var_step_model_3,var_step_model_4,var_step_model_5)), iteration_name)
      step_var_df <- cbind(step_var_df,step_var_df_fold)
      
      mse_step_1_fold <- prediction_and_mse(step_model_1, test = test, test_bacteria = test_bacteria)
      mse_step_2_fold <- prediction_and_mse(step_model_2, test = test, test_bacteria = test_bacteria)
      mse_step_3_fold <- prediction_and_mse(step_model_3, test = test, test_bacteria = test_bacteria)
      mse_step_4_fold <- prediction_and_mse(step_model_4, test = test, test_bacteria = test_bacteria)
      mse_step_5_fold <- prediction_and_mse(step_model_5, test = test, test_bacteria = test_bacteria)
      
      mse_list_step_1<- append(mse_list_step_1,mse_step_1_fold)
      mse_list_step_2<- append(mse_list_step_2,mse_step_2_fold)
      mse_list_step_3<- append(mse_list_step_3,mse_step_3_fold)
      mse_list_step_4<- append(mse_list_step_4,mse_step_4_fold)
      mse_list_step_5<- append(mse_list_step_5,mse_step_5_fold)
      
      
      
      
      
      
      
      
      
     
     
    }
    step_var_df<- step_var_df[,-1]
    rf_var_df <- rf_var_df[,-1]
    #muss nicht 5x laufen
    # fit_lasso_base <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = F, alpha = 1,relax = F, foldid=foldid)
    #fit_lasso_base_cross <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 5,standardize = F,relax = F, foldid = foldid)#--> alpha =1:  lasso regression
    fit_lasso_base_stand <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = T, alpha = 1,relax = F, foldid=foldid)
    fit_lasso_base_cross_stand <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 5,standardize = T,relax = F,foldid=foldid)#--> alpha =1:  lasso regressio
    
    # fit_elnet_base <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = F, alpha = 0.5,relax = F, foldid=foldid)
    #  fit_elnet_base_cross <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=0.5, family="gaussian",  nfolds = 3,standardize = F,relax = F, foldid=T)#--> alpha =1:  lasso regressio
    fit_elnet_base_stand <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = T, alpha = 0.5,relax = F, foldid=foldid)
    fit_elnet_base_cross_stand <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=0.5, family="gaussian",  nfolds = 5,standardize = T,relax = F,foldid=foldid)#--> alpha =1:  lasso regressio
    
    
    lasso_lambda_min <- which(fit_lasso_base_cross_stand$lambda == fit_lasso_base_cross_stand$lambda.min)
    lasso_lambda_1se <- which(fit_lasso_base_cross_stand$lambda == fit_lasso_base_cross_stand$lambda.1se)
    #get_variable names of lasso base
    var_lasso_base_cross_stand_lambda_1se<-get_coef_1se_cv(fit_lasso_base_cross_stand)[[1]]
    var_lasso_base_cross_stand_lambda_min<-get_coef_min_cv(fit_lasso_base_cross_stand)[[1]]
    
    var_lasso_base_cross_stand_lambda_1se<-paste_together_step_coefficients(var_lasso_base_cross_stand_lambda_1se[-1])
    var_lasso_base_cross_stand_lambda_min<- paste_together_step_coefficients(var_lasso_base_cross_stand_lambda_min[-1])
    
    mse_lambda_min_lasso <- fit_lasso_base_cross_stand$cvm[lasso_lambda_min]
    mse_lambda_1se_lasso <- fit_lasso_base_cross_stand$cvm[lasso_lambda_1se]
    
    elnet_lambda_min <- which(fit_elnet_base_cross_stand$lambda == fit_elnet_base_cross_stand$lambda.min)
    elnet_lambda_1se <- which(fit_elnet_base_cross_stand$lambda == fit_elnet_base_cross_stand$lambda.1se)
    
    var_elnet_base_cross_stand_lambda_1se<-get_coef_1se_cv(fit_elnet_base_cross_stand)[[1]]
    var_elnet_base_cross_stand_lambda_min<-get_coef_min_cv(fit_elnet_base_cross_stand)[[1]]
    
    var_elnet_base_cross_stand_lambda_1se <- paste_together_step_coefficients(var_elnet_base_cross_stand_lambda_1se[-1])
    var_elnet_base_cross_stand_lambda_min <- paste_together_step_coefficients(var_elnet_base_cross_stand_lambda_min[-1])
    
    mse_lambda_min_elnet <- fit_elnet_base_cross_stand$cvm[elnet_lambda_min]
    mse_lambda_1se_elnet <- fit_elnet_base_cross_stand$cvm[elnet_lambda_1se]
    
   mse_step_1<-mean(unlist(mse_list_step_1, recursive=FALSE))
   mse_step_2<-mean(unlist(mse_list_step_2, recursive=FALSE))
   mse_step_3<-mean(unlist(mse_list_step_3, recursive=FALSE))
   mse_step_4<-mean(unlist(mse_list_step_4, recursive=FALSE))
   mse_step_5<-mean(unlist(mse_list_step_5, recursive=FALSE))
   
   
   mse_rf_1<- mean(unlist(mse_list_rf_1, recursive=FALSE))
   mse_rf_2<- mean(unlist(mse_list_rf_2, recursive=FALSE))
   mse_rf_3<- mean(unlist(mse_list_rf_3, recursive=FALSE))
   mse_rf_4<- mean(unlist(mse_list_rf_4, recursive=FALSE))
   mse_rf_5<- mean(unlist(mse_list_rf_5, recursive=FALSE))
   
   error_df_algorithms <- data.frame(mse_step_1,mse_step_2,mse_step_3,mse_step_4,mse_step_5,mse_rf_1,mse_rf_2,mse_rf_3,mse_rf_4,mse_rf_5, mse_lambda_min_lasso, mse_lambda_1se_lasso, mse_lambda_min_elnet, mse_lambda_1se_elnet)  %>%
     t()
   step_var_df_save <- cbind(step_var_df_save,step_var_df)
   rf_var_df_save <- cbind(rf_var_df_save,rf_var_df)
   lasso_var_lambda_1se_save<- cbind(lasso_var_lambda_1se_save,var_lasso_base_cross_stand_lambda_1se)
   lasso_var_lambda_min_save<-cbind(lasso_var_lambda_min_save,var_lasso_base_cross_stand_lambda_min)
   elnet_var_lambda_min_save<-cbind(elnet_var_lambda_min_save,var_elnet_base_cross_stand_lambda_min)
   elnet_var_lambda_1se_save<-cbind(elnet_var_lambda_1se_save,var_elnet_base_cross_stand_lambda_1se)
   error_df_algorithms_save <- cbind(error_df_algorithms_save,error_df_algorithms)  
   }    
  }   
}
    
    count_coefficients<-function(entry){ 
      numb_coefficients<-str_count(entry, "\\+") +1
      return(numb_coefficients)
    }
    
    
    step_var_df_save_analysis<-step_var_df_save[,-1]
    lasso_var_lambda_min_save_analysis<-lasso_var_lambda_min_save_analysis%>% mutate(across(contains('var'), 
                       .fns = count_coefficients,
                       .names = paste("coefficients","{col}", sep = "_"))) 
    
    
    lasso_var_lambda_1se_save_analysis[1,2] == lasso_var_lambda_1se_save_analysis[1,2]
    
    (strsplit(lasso_var_lambda_1se_save_analysis[1,2]," \\+ "))[[1]][1]
   
    }
    count_coefficients(b)
    
    c <- "fg+b"
    str_count(string = c, pattern = "\\+")
    
    d<-error_df_algorithms_save[,-c(1:2)]%>%t()
    
    d1<-d %>% colMeans()
    d1<- as.data.frame(d1)
    d3<-d1%>%t()  
  
    # train_rows
  # can also do rf with split https://rstudio-pubs-static.s3.amazonaws.com/71575_4068e2e6dc3d46a785ad7886426c37db.html
  if (do_random_forest ==T) {
    n_features <- 5 #number of features rf should select
    fmla<- list()
    selection <- list()
    
    for(i in 1: length(train_rows)){
      #i= 1
      #train_rf <- data[train_rows[[i]],]
      #test_rf <- data[-c(train_rows[[i]]),]
      train_rf_bacteria <- data[train_rows[[i]],]$log_e.coli
      train_rf <- data[train_rows[[i]],-1]
      test_rf_bacteria <- data[-c(train_rows[[i]]),]$log_e.coli
      test_rf <- data[-c(train_rows[[i]]),-1]
      
      rfModel<-randomForest(x=train_rf,  y=train_rf_bacteria, xtest = test_rf, ytest =  test_rf_bacteria, importance = T )
      assign(paste("rfModel", i, sep = "_"), rfModel)
      #assign(paste(i,"_test_error_df_model",sep = ""),test_error_df_model)
      #imp1 <- as.data.frame(importance(rfModel))
      imp <- as.data.frame(varImp(rfModel, scale=T))
      
      imp <- data.frame(overall = imp$Overall,
                        names   = rownames(imp))
      imp<-imp[order(imp$overall,decreasing = T),]
      
      #take all features that are bigger than  mean importance
      if (do_mean_importance_selection==T) {
        indize <- (imp$overall > mean(imp$overall))
        a<-imp[indize,]
        n_features<-length(a$overall)
        #par(mfrow = c(1,1))
        
        #varImpPlot(rfModel, type = 1,  n.var = nrow(rfModel$importance) )
        selected_rf_features<-imp[order(imp$overall,decreasing = T),]
        #selected_rf_features<-selected_rf_features[1:n_features,"names"]
        selected_rf_features<-selected_rf_features[1:n_features,"names"]
        
      }
      else if(do_n_features_half == T){#or max n-features = n_obs/2
        selected_rf_features<-imp[order(imp$overall,decreasing = T),]
        n_features <- floor(nrow(train)/2)
        selected_rf_features<-selected_rf_features[1:n_features,"names"]
        
      }
      else if(do_cumdecission==T){
        selected_rf_features<-imp[order(imp$overall,decreasing = T),]
        imp1<-imp %>%
          mutate(cum_importance = cumsum(overall))
        
        z<-max(imp1$cum_importance)
        x<-imp1$cum_importance < 1/3 * z
        n_features<-nrow(imp1[x,])
        selected_rf_features<-selected_rf_features[1:n_features,"names"]
      }
      else if(do_5_selection){
        selected_rf_features<-imp[order(imp$overall,decreasing = T),]
        n_features <- 5
        selected_rf_features<-selected_rf_features[1:n_features,"names"]
      }
      
      str_fmla <- selected_rf_features[1]
      for( i in 2:n_features){
        str_fmla<- paste(str_fmla, selected_rf_features[i], sep = " + ")
      }
      
      str_fmla <- paste("log_e.coli ~ ",str_fmla, sep="")
      
      fmla_new <- as.formula(str_fmla)
      #heiko_lm<- lm(fmla_new, data = data)
      #selection<-append(selection, heiko_lm)
      fmla<- append(fmla, fmla_new)
      
      #selection<-append(selection, list_heiko_lm)
      #assign(paste("features_rf",i,sep = "_"),selected_rf_features)
      
      
    }
    heiko_lm <- list()
  for( i in 1:length(fmla)){
   #formel  = fmla[[1]]
    heiko_lm<- lm(fmla[[i]], data = data)
    selection[[i]] <-heiko_lm
    

  }
  fb <- selection
  }
  
    # order of pattern, q_old and q_new is important!
  
  #fb <- stepwise(river = river, pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)", data,null, full)#,
  if(do_step==T){
  step_returns <- stepwise(river = river, pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)", data,null, full)
  fmla <- step_returns[[1]]
  selection <- step_returns[[2]]
  fb<- selection 
  
  }
  
    #lasso methods
  if(do_lasso ==T){        
    fit_lasso_base <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = F, alpha = 1,relax = F, foldid = foldid)
    
    fit_lasso_base_cross <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 5,standardize = F,relax = F, foldid = foldid)#--> alpha =1:  lasso regressio
    
    
    fit_lasso_base_stand <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = T, alpha = 1,relax = F , foldid = foldid)
    fit_lasso_base_cross_stand <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 5,standardize = T,relax = F, foldid = foldid)#--> alpha =1:  lasso regressio
    
    
  
  
  
  
    par(mfrow=c(2,2))
    plot(fit_lasso_base, xvar="lambda", label = T, main = "lasso_base")
    plot(fit_lasso_base_cross,main="LASSO")
    
    plot(fit_lasso_base_stand, xvar="lambda", label = T, main = "lasso_base_stand")
    plot(fit_lasso_base_cross_stand,main="LASSO")
    
    #plot(fit_elnet_base, xvar="lambda", label = T, main = "elnet_base")
    #plot(fit_elnet_base_cross,main="elnet")
    
    #plot(fit_elnet_base_stand, xvar="lambda", label = T, main = "elnet_base_stand")
    #plot(fit_elnet_base_cross_stand,main="elnet")
    
      
    
      #coef_1se_fit_lasso_base_cross<-get_coef_1se_cv (fit_lasso_base_cross)
      #coef_1se_fit_lasso_base_cross_stand<-get_coef_1se_cv (fit_lasso_base_cross_stand)
      #coef_lambda_min_fit_lasso_base_cross<-get_coef_min_cv (fit_lasso_base_cross)
      #coef_lambda_min_fit_lasso_base_cross_stand<-get_coef_min_cv (fit_lasso_base_cross_stand)
      
      # add_new_formulas_to_list_if_exists <- function(coef_list){
      
      #    if(exists("coef_1se_fit_lasso_base_cross")== TRUE){
      #     idx <- length(list_lasso)
      #    idx <- idx+1
      #    list_lasso[[idx]] <-coef_1se_fit_lasso_base_cross
      #  }
      #}
    {
      list_lasso <- list()
      
    try ( coef_1se_fit_lasso_base_cross               <-get_feature_selection_coeficient_names_as_formular_1se(fit_lasso_base_cross))
      
      if(exists("coef_1se_fit_lasso_base_cross")== TRUE){
        idx <- length(list_lasso)
        idx <- idx+1
        list_lasso[[idx]] <-coef_1se_fit_lasso_base_cross
      }
     try( coef_1se_fit_lasso_base_cross_stand         <-get_feature_selection_coeficient_names_as_formular_1se(fit_lasso_base_cross_stand))
      if(exists("coef_1se_fit_lasso_base_cross_stand")== TRUE){
        idx <- length(list_lasso)
        idx <- idx+1
        list_lasso[[idx]] <-coef_1se_fit_lasso_base_cross_stand
      }
      
      try(coef_lambda_min_fit_lasso_base_cross        <-get_feature_selection_coeficient_names_as_formular_lambda_min(fit_lasso_base_cross))
      if(exists("coef_lambda_min_fit_lasso_base_cross")== TRUE){
        idx <- length(list_lasso)
        idx <- idx+1
        list_lasso[[idx]] <-coef_lambda_min_fit_lasso_base_cross
      }
      
      try(coef_lambda_min_fit_lasso_base_cross_stand  <-get_feature_selection_coeficient_names_as_formular_lambda_min(fit_lasso_base_cross_stand))
      if(exists("coef_lambda_min_fit_lasso_base_cross_stand")== TRUE){
        idx <- length(list_lasso)
        idx <- idx+1
        list_lasso[[idx]] <-coef_lambda_min_fit_lasso_base_cross_stand
      }
    }
    
    
      #check if all 4 coefficients exist and remove intercepts
      idx <-0
      for(element in list_lasso){
        idx<-idx+1
        if(typeof(element)!="language"){
          list_lasso <- list_lasso[-idx]
          print("f")
        }
      }
      
      print(paste(length(list_lasso)," new models added")) 
      model_lsit<-list()
      list_lasso<-limit_max_features(list_lasso, data)
      
      
      #builded linear model
      heiko_lm_1<-lm(list_lasso[[1]], data = data)
      heiko_lm_2<-lm(list_lasso[[2]],data = data)
      heiko_lm_3<-lm(list_lasso[[3]],data = data)
      heiko_lm_4<-lm(list_lasso[[4]],data = data)
      
      list_heiko_lm <- list()
      list_heiko_lm[[1]]<- heiko_lm_1
      list_heiko_lm[[2]]<- heiko_lm_2
      list_heiko_lm[[3]]<- heiko_lm_3
      list_heiko_lm[[4]]<- heiko_lm_4
      #for(form in list_lasso){
      
      # heiko_lm <- lm(form, data = data)
      #  heiko_lm<-list(heiko_lm)
      # append(heiko_lm,model_lsit)
      #  }
      #heiko_lm<- lm(formula_heiko_1, data = data)   
      
      #nicht mehr benötigt  
      #### Anwenden der Hauptfunktion ###################
      
  
  
  

  
  
#adding new linear models, featureselection with lasso/elnet
#selection<-append(selection, list(heiko_lm_1,heiko_lm_2,heiko_lm_3,heiko_lm_4))
  selection<-append(selection, list_heiko_lm)
  fb<- selection  
#fb[6] <- list(heiko_lm)
  
  #selection[6] <- list(heiko_lm)
  selection
  fb
  
  fmla_heiko_1 <-eval(heiko_lm_1$call$formula)
  fmla_heiko_2 <-eval(heiko_lm_2$call$formula)
  fmla_heiko_3 <-eval(heiko_lm_3$call$formula)
  fmla_heiko_4 <-eval(heiko_lm_4$call$formula)
  
  fmla_lasso <- list()
  fmla_lasso[[1]]<- fmla_heiko_1
  fmla_lasso[[2]]<- fmla_heiko_2
  fmla_lasso[[3]]<- fmla_heiko_3
  fmla_lasso[[4]]<- fmla_heiko_4
 
  
  # as.list(selection[[6]]$call)$formula
  
  fmla<-append(fmla, fmla_lasso)
  
  if(class(fmla[[length(fmla)]]) !="formula"){
    print("new element is no formula!!")
  }
  
  
  #add my models here
  
      
  #q_old = "q_cochem",
  
  #q_new = "q_cochem_abs_1")
  


  
  
  }
  
  if(do_elnet ==T){        
   # fit_elnet_base <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = F, alpha = 0.5,relax = F)
    
    #fit_elnet_base_cross <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=0.5, family="gaussian",  nfolds = 3,standardize = F,relax = F)#--> alpha =1:  lasso regressio
    
    
    fit_elnet_base_stand <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = T, alpha = 0.5,relax = F, foldid = foldid)
    fit_elnet_base_cross_stand <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=0.5, family="gaussian",  nfolds = 5,standardize = T,relax = F, foldid = foldid)#--> alpha =1:  lasso regressio
    
    
    
    
    
    
    par(mfrow=c(2,2))
    #plot(fit_elnet_base, xvar="lambda", label = T, main = "elnet_base")
    #plot(fit_elnet_base_cross,main="Elnet")
    
    plot(fit_elnet_base_stand, xvar="lambda", label = T, main = "elnet_base_stand")
    plot(fit_elnet_base_cross_stand,main="Elnet")
    
    #plot(fit_elnet_base, xvar="lambda", label = T, main = "elnet_base")
    #plot(fit_elnet_base_cross,main="elnet")
    
    #plot(fit_elnet_base_stand, xvar="lambda", label = T, main = "elnet_base_stand")
    #plot(fit_elnet_base_cross_stand,main="elnet")
    
    
    
    #coef_1se_fit_elnet_base_cross<-get_coef_1se_cv (fit_elnet_base_cross)
    coef_1se_fit_elnet_base_cross_stand<-get_coef_1se_cv (fit_elnet_base_cross_stand)
    #coef_lambda_min_fit_elnet_base_cross<-get_coef_min_cv (fit_elnet_base_cross)
    coef_lambda_min_fit_elnet_base_cross_stand<-get_coef_min_cv (fit_elnet_base_cross_stand)
    
    # add_new_formulas_to_list_if_exists <- function(coef_list){
    
    #    if(exists("coef_1se_fit_lasso_base_cross")== TRUE){
    #     idx <- length(list_lasso)
    #    idx <- idx+1
    #    list_lasso[[idx]] <-coef_1se_fit_lasso_base_cross
    #  }
    #}
    
    list_elnet <- list()
    
   # coef_1se_fit_elnet_base_cross               <-get_feature_selection_coeficient_names_as_formular_1se(fit_elnet_base_cross)
    
    #if(exists("coef_1se_fit_elnet_base_cross")== TRUE){
    #  idx <- length(list_elnet)
    #  idx <- idx+1
    #  list_elnet[[idx]] <-coef_1se_fit_elnet_base_cross
    #}
    coef_1se_fit_elnet_base_cross_stand         <-get_feature_selection_coeficient_names_as_formular_1se(fit_elnet_base_cross_stand)
    if(exists("coef_1se_fit_elnet_base_cross_stand")== TRUE){
      idx <- length(list_elnet)
      idx <- idx+1
      list_elnet[[idx]] <-coef_1se_fit_elnet_base_cross_stand
    }
    
    #coef_lambda_min_fit_elnet_base_cross        <-get_feature_selection_coeficient_names_as_formular_lambda_min(fit_elnet_base_cross)
    #if(exists("coef_lambda_min_fit_elnet_base_cross")== TRUE){
    #  idx <- length(list_elnet)
    #  idx <- idx+1
    #  list_elnet[[idx]] <-coef_lambda_min_fit_elnet_base_cross
    #}
    
    coef_lambda_min_fit_elnet_base_cross_stand  <-get_feature_selection_coeficient_names_as_formular_lambda_min(fit_elnet_base_cross_stand)
    if(exists("coef_lambda_min_fit_elnet_base_cross_stand")== TRUE){
      idx <- length(list_elnet)
      idx <- idx+1
      list_elnet[[idx]] <-coef_lambda_min_fit_elnet_base_cross_stand
    }
    
    #check if all 4 coefficients exist and remove intercepts
    idx <-0
    for(element in list_elnet){
      idx<-idx+1
      if(typeof(element)!="language"){
        list_elnet <- list_elnet[-idx]
        print("f")
      }
    }
    list_elnet
    list_elnet<-limit_max_features(list_elnet, data = data)
    print(paste(length(list_elnet)," new models added")) 
    model_lsit<-list()
    
    #builded linear model
    heiko_lm_1<-lm(list_elnet[[1]], data = data)
    heiko_lm_2<-lm(list_elnet[[2]],data=data)
   # heiko_lm_3<-lm(list_elnet[[3]],data=data)
   # heiko_lm_4<-lm(list_elnet[[4]],data=data)
    
    list_heiko_lm <- list()
    list_heiko_lm[[1]]<- heiko_lm_1
    list_heiko_lm[[2]]<- heiko_lm_2
    #list_heiko_lm[[3]]<- heiko_lm_3
    #list_heiko_lm[[4]]<- heiko_lm_4
    #for(form in list_lasso){
    
    # heiko_lm <- lm(form, data = data)
    #  heiko_lm<-list(heiko_lm)
    # append(heiko_lm,model_lsit)
    #  }
    #heiko_lm<- lm(formula_heiko_1, data = data)   
    
    #nicht mehr benötigt  
    
    
    
    
    
    
    
    
    
    #adding new linear models, featureselection with lasso/elnet
    #selection<-append(selection, list(heiko_lm_1,heiko_lm_2,heiko_lm_3,heiko_lm_4))
    selection<-append(selection, list_heiko_lm)
    fb<- selection  
    #selection <- selection[-c(1:4)]
    #fb[6] <- list(heiko_lm)
    
    #selection[6] <- list(heiko_lm)
   # selection[[13]]
    #fb[[13]]
    
    fmla_heiko_1 <-eval(heiko_lm_1$call$formula)
    fmla_heiko_2 <-eval(heiko_lm_2$call$formula)
    #fmla_heiko_3 <-eval(heiko_lm_3$call$formula)
    #fmla_heiko_4 <-eval(heiko_lm_4$call$formula)
    
    fmla_elnet <- list()
    fmla_elnet[[1]]<- fmla_heiko_1
    fmla_elnet[[2]]<- fmla_heiko_2
    #fmla_elnet[[3]]<- fmla_heiko_3
    #fmla_elnet[[4]]<- fmla_heiko_4
    # as.list(selection[[6]]$call)$formula
    
    fmla<-append(fmla, fmla_elnet)
    
    if(class(fmla[[length(fmla)]]) !="formula"){
      print("new element is no formula!!")
    }
    
    
    #add my models here
    
    
    #q_old = "q_cochem",
    
    #q_new = "q_cochem_abs_1")
    
    
    
    
  
  }
  
  }
    {
  if (do_mcmc==T) {
  #do renaming here
  names(fb) <- sprintf(paste0(river,"model_%02d"), seq_along(1:length(fb)))
  
  
  
  ################ Validation ########################
  
  
  
  # calculate statistical tests for residuals: Normality and s2 = const
  
  # shapiro-wilk test and breusch-pagan test
  
  get_stat_tests <- function(model) {
    c(N = shapiro.test(model$residuals)$p.value, lmtest::bptest(model)$p.value,
    R2 = summary(model)[["adj.r.squared"]], n_obs = length(model$residuals))
    
  }
  
  
  
  # Eliminieren von modelled die doppelt vorkommen, da forward selection früher
  
  #fertig als n steps
  
  #heiko add fb beforehand to this
  #fb
  unique_index <- length(unique(fb))
  fb <- fb[1:unique_index]
  
  
  
  # testing for classical statistical model assumtions, normality of residuals and
  
  # heteroskelasdicity   #t() transpose
  river_stat_tests <- sapply(fb, get_stat_tests)%>%
    t() %>%
    dplyr::as_tibble(rownames = "model")  %>%
    dplyr::bind_rows(.id = "river") %>%
    dplyr::mutate(stat_correct = N > .05 & BP > .05)
  
  
  
  # creating list of independent training rows
  #-test/train split
  
  #weirde zeile, setze alle stat tests auf 0
  river_stat_tests$in95 <- river_stat_tests$below95 <-river_stat_tests$below90 <- river_stat_tests$in50 <- river_stat_tests$MSE <- 0
  
  if(first_time_model_train == 1){ #keep the same train_rows, if models were trained one after another. object should be true only when it is trained 1. time on dataset
  #crate cross validation folds
 #   train_rows <- caret::createFolds(1:nrow(fb[[paste0(river, "model_01")]]$model),
                                     
  #                                   k = 5, list = T, returnTrain = T)
    
    
  #}  
 #sum(foldid ==1)
 #sum(foldid ==2)
 #sum(foldid ==3) 
 #sum(foldid ==4)
 #sum(foldid ==5)
    
  #train_rows <- caret::createFolds(1:nrow(fb[[paste0(river, "model_01")]]$model),
                                 
   #                                k = 5, list = T, returnTrain = T)
  #train_row_id <- seq(1:nrow(data))
  #train_rows <- list()
  #for(fold in 1:5){
  #train_rows <- train_row_id[foldid!= fold]
  #train_rows[[fold]] <- train_row_id[foldid!= fold] 
  #}
  }  
  
  if(class(fmla[[length(fmla)]]) !="formula"){
    print("new element is no formula!!")
  }
  
  test_beta <- function(true, false, percentile){
    if( pbeta(q = percentile, shape1 = true + 1, shape2 = false + 1) > 0.05){
      TRUE}
    else{FALSE}
    
  }
  
 #naming models 
  names(fmla) <- sprintf(paste0(river,"model_%02d"), seq_along(1:length(fb)))
  

  
  
  counter<-0
  
  #fb<-fb[-6]


erro_df <- data.frame()
     
#cross validation   needs fb, and fmla


  

 { 
   for(i in names(fb)){
  counter<- counter+1
       #i="havelmodel_01"
       test_error_df_model <- data.frame()
        assign(paste(i,"_test_error_df_model",sep = ""),test_error_df_model)
    
    for(j in 1:5){
      counter <- counter+1
         # j=3
    
      
        training <- as.data.frame(fb[[i]]$model)[c(train_rows[[j]]),]
        #training <- as.data.frame(fb[[6]]$model)[c(train_rows[[1]]),]
        test <- as.data.frame(fb[[i]]$model)[-c(train_rows[[j]]),]
        #test <- as.data.frame(fb[[6]]$model)[-c(train_rows[[1]]),]
        #formel<-formula(formula_heiko_1)
        
        
        #fmla[6]<- list(formel)
        
        
        
        fit <- rstanarm::stan_glm(fmla[[i]], data = training ) #fitting
        #fit <- rstanarm::stan_glm(fmla[[1]], data = training) #fitting
       
        
        df <- apply(rstanarm::posterior_predict(fit, newdata = test), 2, quantile, #predicting
                    
                    probs = c(0.025, 0.25, 0.5 , 0.75, 0.9, 0.95, 0.975)) %>% t() %>% as.data.frame() %>%
          
          dplyr::mutate(log_e.coli = test$log_e.coli, #evaluating ther model has to be classified correctly with every single test train split
                        #--> here 5 different splits, if all validations correct than everywhere ==5
                        
                        below95 = log_e.coli < `95%`,
                        
                        below90 = log_e.coli < `90%`,
                        
                        within95 = log_e.coli < `97.5%`& log_e.coli > `2.5%`,
                        
                        within50 = log_e.coli < `75%`& log_e.coli > `25%`,
                        
          ) 
        test_error_df_temp<- df%>%select(log_e.coli,`50%`)%>% mutate(squared_error = ((log_e.coli - `50%`)^2))
        test_error_df_model<- test_error_df_model %>% rbind(test_error_df_temp)
        
        assign(paste(i,"_test_error_df_model", sep = ""),test_error_df_model)
        #validation step if all percentile categories are set to 1
        
        river_stat_tests$in95[river_stat_tests$model == i] <-
          
          river_stat_tests$in95[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$within95), false = sum(!df$within95), percentile = .95 ) #is 1 if true
        
        river_stat_tests$below95[river_stat_tests$model == i] <-
          
          river_stat_tests$below95[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$below95), false = sum(!df$below95), percentile = .95 )
        
        river_stat_tests$below90[river_stat_tests$model == i] <-
          
          river_stat_tests$below90[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$below90), false = sum(!df$below90), percentile = .90 )
        
        river_stat_tests$in50[river_stat_tests$model == i] <-
          
          river_stat_tests$in50[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$within50), false = sum(!df$within50), .5)
        #add MSE to stat_tests
        river_stat_tests$MSE[river_stat_tests$model == i] <- mean(test_error_df_model$squared_error)
        
        
        #add selected features and coeeficients to statistical test
        
        river_stat_tests$selected_features[river_stat_tests$model == i] <- list(all.vars(fmla[[i]]))
        river_stat_tests$coef_selected[river_stat_tests$model == i]<- list(coef(fb[[i]]))
        
        
        
      } 
      
  } 
 }
#fmla
  
  #needed to be sure to not change train_rows while analyzing the same dataset with different algorithms
  first_time_model_train <- first_time_model_train +1
  
  if(do_lasso ==T ){
   #
    river_stat_tests$model = paste(river_stat_tests$model,"_lasso",sep = "")
    names(fmla) = paste(names(fmla),"_lasso",sep = "")
    names(fb)  = paste(names(fb),"_lasso",sep = "")
  
    for (entry in 1:nrow(river_stat_tests)) {
      if (entry==1) {
        new_add<- "_1se"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
        
      }else if (entry==2) {
        new_add<- "_1se_stand"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
      }else if (entry==3) {
        new_add<- "_lambda_min"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
      }else if (entry==4) {
        new_add<- "_lambda_min_stand"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
      }
      
      }
    }
  
  if(do_elnet ==T ){
    river_stat_tests$model = paste("elnet_",river_stat_tests$model)
    names(fmla) = paste("elnet_",names(fmla))
    names(fb)  = paste("elnet_",names(fb))
    for (entry in 1:nrow(river_stat_tests)) {
      if (entry==1) {
        new_add<- "_1se"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
        
      }else if (entry==2) {
        new_add<- "_1se_stand"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
      }else if (entry==3) {
        new_add<- "_lambda_min"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
      }else if (entry==4) {
        new_add<- "_lambda_min_stand"
        river_stat_tests$model[entry] = paste(river_stat_tests$model[entry], new_add, sep = "")
        names(fmla)[entry] <- paste(names(fmla)[entry], new_add, sep = "")
        names(fb)[entry]  = paste(names(fb)[entry],new_add,sep = "")
      }
      
    }
  }
  if(do_step ==T ){
    for (entry in nrow(river_stat_tests)) {
      river_stat_tests$model = paste("step_",river_stat_tests$model)
      names(fmla) = paste("step_",names(fmla))
      names(fb)  = paste("step_",names(fb))
    }
  }
    
  if(do_random_forest ==T ){
    for (entry in nrow(river_stat_tests)) {
      river_stat_tests$model = paste("random_forest_",river_stat_tests$model)
      names(fmla) = paste("random_forest_",names(fmla))
      names(fb)  = paste("random_forest_",names(fb))
    }
  } 
  
  } 
  
}
   # class(fmla[[1]])
    #class(fb[[1]])
    #coef(fb[[1]])
    #all.vars(fmla[[1]])
    river_stat_model_save <- rbind(river_stat_model_save,river_stat_tests)
    
    river_fmla_save <-append(river_fmla_save,fmla)
    
    river_fb_save <-append(river_fb_save,fb)
    
    
    
    
    
    sorted_modellist <- river_stat_model_save %>%
      
      filter( below95 == 5 & below90 == 5& in95) %>%
      
      dplyr::arrange(desc(in50), desc(R2))    
}


#collection_fmla <- list()
collection_stat_models[[1]]<- river_stat_model_save
names(collection_stat_models)<-"havel"
collection_stat_models

rivers
river_stat_model_save<-data.frame(river_stat_model_save)
write.csv(river_stat_model_save, paste(river[[1]], "river_stat_model.csv", sep = "_"))
write.csv(river_fmla_save, paste(river, "river_fmla.csv"))
write(river_fb_save, paste(river, "river_fb.csv", sep = "_"))
 #save formulas and fb and stat model in dataframe and list
  #halloolo <- river_stat_tests
  #halloolo
  #river_stat_tests  
 # river_stat_model_save <- rbind(river_stat_model_save,river_stat_tests)
#  river_fmla_save <-append(river_fmla_save,fmla)
  
#  river_fb_save <-append(river_fb_save,fb)
  
  
  
  
  
 # sorted_modellist <- river_stat_model_save %>%
    
  #  filter( below95 == 5 & below90 == 5& in95) %>%
    
   # dplyr::arrange(desc(in50), desc(R2))
  
  #river_stat_tests
  #sorted_modellist
  
  best_valid_model_stats <- sorted_modellist[1,]
  
  best_valid_model <- river_fb_save[[best_valid_model_stats$model]]
  
  sorted_modellist
  
  #a<-mean(sqrt(havelmodel_01_test_error_df_model$error^2 ))
  #b<-mean(sqrt(havelmodel_02_test_error_df_model$error^2 ))
  #c<-mean(sqrt(havelmodel_03_test_error_df_model$error^2 ))
  #d<-mean(sqrt(havelmodel_04_test_error_df_model$error^2 ))
  
  #a
  #b
  #c
  #d
  #(a+b+c+d)/4
  #fit_lasso_base_cross_stand$lambda.min
  
  #coef(best_valid_model)
  
  
  
  #refit best model
 
  
  
  
  
  #best_valid_model   <- lm(best_valid_model,data ) 
first_valid_model_stats <- sorted_modellist[1,]
second_valid_model_stats <- sorted_modellist[2,]

first_valid_model <- river_fb_save[[first_valid_model_stats$model]]
second_valid_model <- river_fb_save[[second_valid_model_stats$model]]
first <-predict(first_valid_model)
second <-predict(second_valid_model)

par(mfrow = c(1,1))
  plot(first,data$log_e.coli,
       xlab="predicted",ylab="actual", xlim=c(1,4), ylim=c(1,4))
  points(second, data$log_e.coli, col = "red")
  abline(a=0,b=1)
  par(mfrow = c(2,2))
  plot((best_valid_model))
  
  #river_fmla_save$`lasso_ havelmodel_04`
  
  #training <- as.data.frame(river_fb_save$`lasso_ havelmodel_04`$model)[c(train_rows[[j]]),]
  #training <- as.data.frame(fb[[6]]$model)[c(train_rows[[1]]),]
  #test <- as.data.frame(river_fb_save$`lasso_ havelmodel_04`$model)[-c(train_rows[[j]]),]
  
  
  stanfit <- rstanarm::stan_glm(river_fmla_save[[best_valid_model_stats$model]],
                                
                                data = best_valid_model$model)
  
  
  rstanarm::posterior_predict(stanfit, newdata = test) #predicting
              
  
  coef(best_valid_model)
  #here for giving the model to prediction 
  stanfit <- rstanarm::stan_glm(fmla[[best_valid_model_stats$model]],
                                
                                data = best_valid_model$model)
  
  brmsfit <- brms::brm(fmla[[best_valid_model_stats$model]],
                       
                       data = best_valid_model$model, iter = 10000) 
 
  
  #return(list(sorted_modellist = sorted_modellist,
              
   #           best_model = best_valid_model,
              
    #          stanfit = stanfit,
              
  
     #         brmsfit = brmsfit))
  
  



  