
#preload packages
{
  library(compare)
  library(broom)
  library(caret)
  library(magrittr)
  library(randomForest)
  library(dplyr)
  library(glmnet)
  library(purrr)
  library(tidyverse)
  #?glmnet
  library(phonTools)
  library(coefplot) #for extracing non 0 coef
  #install.packages("tidyverse")
  library(tidyverse)
  library(pROC)
  library(fhpredict)
  library(tidyverse)
  library(Boruta)
  library(reshape2)
  library(kwb.flusshygiene)
}
#data_paths
{
  
  #first_time_model_train <- 1
  
  rivers <- c("havel")
  river <- "havel"
  #river_paths <- kwb.flusshygiene::get_paths()[paste0(rivers, "data")]
  
  #river_paths <- list(havel = "Y:/SUW_Department/Projects/FLUSSHYGIENE/Data-Work packages/Daten/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv")
  
  river_paths1 <- list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv" )
  river_paths2 <- list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Isar/DATA_preprocessed_csv")
  river_paths3 <- list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Ilz/DATA_preprocessed_csv" )
  
  river_paths4<-list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Rhein_Mosel_Lahn/Mosel/DATA_preprocessed_csv")
  river_paths5<-list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Rhein_Mosel_Lahn/Rhein/DATA_preprocessed_csv")
  
  river_paths6<-list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Ruhr/Ruhr/DATA_preprocessed_csv")
  list_river_pathes<- list(river_paths1,river_paths2,river_paths3,river_paths4,river_paths5,river_paths6)
}
#initializing list_for saving
{
iteration_river_list_df_min_mse_algo_oos<- list()
iteration_river_list_lasso_Model <- list()

iteration_river_list_rf_Model<-list()
iteration_river_list_mse_mean_rf<- list()

iteration_river_list_selection_aic<-list()
iteration_river_list_selection_bic <-list()
iteration_river_list_mse_mean_step_aic<- list()
iteration_river_list_mse_mean_step_bic<- list()

iteration_river_list_selection_5_aic<-list()
iteration_river_list_selection_5_bic<-list()
iteration_river_list_mse_mean_step_5_aic<- list()
iteration_river_list_mse_mean_step_5_bic<- list()

iteration_river_list_lasso_Model <- list()
iteration_river_list_mse_mean_lasso_lambda_min<- list()
iteration_river_list_mse_mean_lasso_lambda_1se<- list()

iteration_river_list_data <- list()
iteration_river_list_data_train <- list()
iteration_river_list_data_test <- list()

iteration_river_list_foldids <- list()
}
start_time<-Sys.time()

for(iteration in 1:1){
  #iteration<- 1
  river_list_data<-list()
  river_list_foldids<-list()
  #preloading data
  {
    list_train_rows <- list()
    list_test_rows <- list()
    for (river_path_river in list_river_pathes) {
      
      river_paths<-river_path_river
      
      #river_paths<- list_river_pathes[[1]]
      {
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
      }
      
      river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
      names(river_data) <- rivers
      {
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
      }
      #run here
      iterations <-iteration
      new_train_test_split <-T
      
      set.seed(iterations)
      pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)"
      riverdata <- river_data_ts[[river]]
      # prepare variables out of all cominations (given by pattern)
      # variables for interaction get replaced by q_new (remove q_old)
      vars1 <- (riverdata[-1] %>% unroll_physical_data() %>%
                  lapply(names) %>% unlist() %>% unique())[-1]
      
      vars2 <- vars1[stringr::str_detect(vars1, pattern)]
      # prepare formulas
      
      data <- process_model_riverdata(riverdata, c("log_e.coli", vars2)) %>%  dplyr::select(-datum) 
      data <- na.omit(data)
      
      river_list_data <- append(river_list_data,list(data))
      
      datapoints<-seq(1, nrow(data))
      
      
      if(new_train_test_split ==T){
        foldid=sample(rep(seq(5),length=nrow(data)))  #fixes the train/test for cv.glmnet
        #length(foldid)
        river_list_foldids<- append(river_list_foldids, list(foldid))
        train_rows<- list()
        test_rows<- list()
        for (fold in 1:5) {
          train_rows[[fold]]<-datapoints[foldid!=fold] 
          test_rows[[fold]]<-datapoints[foldid==fold] 
        }
        list_train_rows <- append(list_train_rows,list(train_rows))
        list_test_rows <- append(list_test_rows,list(test_rows))
      }
      
    }  
  }
  
  
}
end_time<-Sys.time()
end_time- start_time



river_list_data[[1]]

for(iteration in 1:1){  
  #new data_train data_test
  
  
  river_list_data_train <- list()
  river_list_data_test <- list()
  river_list_data_train_bacteria <- list()
  river_list_data_test_bacteria <- list()
  #rive_data initalization
  for (indx_river in 1:length(river_list_data)) {
    #indx_river<-1
    list_data_train<- list()
    list_data_test<- list()
    list_data_train_bacteria<- list()
    list_data_test_bacteria<- list()
    
    data <- river_list_data[[indx_river]]
    train_rows<- list_train_rows[[indx_river]]
    test_rows<- list_test_rows[[indx_river]]
    for(indx_fold in 1:length(train_rows)){
      #indx_fold<-5
      data_train <- data[train_rows[[indx_fold]],]
      data_train <-data.frame(scale(data_train))
      list_data_train<- append(list_data_train, list(data_train))
      
      #test_data fold
      data_test <- data[-train_rows[[indx_fold]],]
      data_test <-data.frame(scale(data_test))
      
      
      list_data_test<- append(list_data_test, list(data_test))
      
      train_bacteria <- data_train$log_e.coli
      list_data_train_bacteria<- append(list_data_train_bacteria, list(train_bacteria))
      
      test_bacteria <- data_test$log_e.coli
      list_data_test_bacteria<- append(list_data_test_bacteria, list(test_bacteria))
    }
    
    river_list_data_train <- append(river_list_data_train, list(list_data_train))
    river_list_data_test <- append(river_list_data_test, list(list_data_test))
    river_list_data_train_bacteria <- append(river_list_data_train_bacteria, list(list_data_train_bacteria))
    river_list_data_test_bacteria <- append(river_list_data_test_bacteria, list(list_data_test_bacteria))
  }
  
  iteration_river_list_data<- append(iteration_river_list_data, list(river_list_data))
  
  iteration_river_list_foldids <- append(iteration_river_list_foldids, list(river_list_foldids))
  
  iteration_river_list_data_train <- append(iteration_river_list_data_train, list(river_list_data_train))
  iteration_river_list_data_test <- append(iteration_river_list_data_test, list(river_list_data_test))
  
  
  
  
  
  
  
  do_lasso<-T
  #initializing lasso formulas
  if (do_lasso==T) {
    #river_list_mse_mean_lasso <- list() # mses on test-set outer loop for every river
    river_list_mse_mean_lasso_lambda_min <- list()
    river_list_mse_mean_lasso_lambda_1se <- list()
    
    river_list_lasso_Model<- list() 
    #river_list_selection_lasso<- list()
    river_list_selection_lasso_min<- list()
    river_list_selection_lasso_1se<- list()
    
    
    #error on full dataset
    #river_list_mse_lasso_evaluation<-list()
    
  }
  #lasso-selection
  if(do_lasso==T){
    for(indx_river in 1:length(river_list_data_train)){
      #entry <- list_data_train_river[[1]]
      #indx_river<-1
      list_mse_lasso_lambda_min <- list()
      list_mse_lasso_lambda_1se <- list()
      list_lasso_Model <- list()
      
      data_train_folds<-river_list_data_train[[indx_river]]
      data_test_folds <- river_list_data_test[[indx_river]]
      for(fold_indx in  1: length(data_train_folds)){
        #fold_indx <- 1
        data_train_fold <- data_train_folds[[fold_indx]]
        
        data_test_fold <- data_test_folds[[fold_indx]]
        
        full_with_interaction <- formula(log_e.coli ~ (.)^2, data = data_train_fold)
        
        train_scaled <- sparse.model.matrix(full_with_interaction, data_train_fold)
        test_scaled <- sparse.model.matrix(full_with_interaction, data_test_fold)
        
        lassoModel_prescale<-cv.glmnet(train_scaled, data_train_fold$log_e.coli,type.measure="mse", alpha=1,  nfolds = 5,standardize = F,relax = F)#--> alpha =1:  lasso regressio
        
        
        list_lasso_Model<- append(list_lasso_Model, list(lassoModel_prescale))
        
        
        #prediction_lasso<- predict(lassoModel, newx = test_scaled, s=c(lassoModel$lambda.min,lassoModel$lambda.1se))
        prediction_lasso_lambda_min<- predict(lassoModel_prescale, newx = test_scaled, s=lassoModel_prescale$lambda.min)
        prediction_lasso_lambda_1se<- predict(lassoModel_prescale, newx = test_scaled, s=lassoModel_prescale$lambda.1se)
        #mean error all folds for 1 river
        mse_lasso_lambda_min <-mean(sqrt((data_test_fold$log_e.coli - prediction_lasso_lambda_min)^2))
        mse_lasso_lambda_1se <-mean(sqrt((data_test_fold$log_e.coli - prediction_lasso_lambda_1se)^2))
        
        list_mse_lasso_lambda_min<- append(list_mse_lasso_lambda_min, list(mse_lasso_lambda_min))
        list_mse_lasso_lambda_1se<- append(list_mse_lasso_lambda_1se, list(mse_lasso_lambda_1se))
        
        
      }
      
      
      mse_mean_lasso_lambda_min <-  mean(unlist(list_mse_lasso_lambda_min)) # mean over all 5 train/test folds
      mse_mean_lasso_lambda_1se <- mean(unlist(list_mse_lasso_lambda_1se))
      river_list_mse_mean_lasso_lambda_min <- append(river_list_mse_mean_lasso_lambda_min,list(mse_mean_lasso_lambda_min))
      river_list_mse_mean_lasso_lambda_1se <- append(river_list_mse_mean_lasso_lambda_1se,list(mse_mean_lasso_lambda_1se))
      
      river_list_lasso_Model <- append(river_list_lasso_Model,list(list_lasso_Model))
    }  
    
    
    #this is the new
    
    
    
  }
  
  
  do_step_full <-T
  if (do_step_full==T) {
    river_list_mse_mean_step_aic <- list()
    river_list_mse_mean_step_bic<-list()
    
    per_river_list_mse_mean_step_aic<-list()
    per_river_list_mse_mean_step_bic<-list()
    
    river_list_selection_aic<- list()
    river_list_selection_bic<- list()
    
    river_list_mse_step_aic_evaluation_oos<-list()
    river_list_mse_step_bic_evaluation_oos<-list()
    #error on full dataset
    river_list_mse_step_aic_evaluation<-list()
    river_list_mse_step_bic_evaluation<-list()
  }
  do_step_5<-T
  if (do_step_5==T) {
    river_list_mse_mean_step_5_aic <- list()
    river_list_mse_mean_step_5_bic<-list()
    
    per_river_list_mse_mean_step_5_aic<-list()
    per_river_list_mse_mean_step_5_bic<-list()
    
    river_list_selection_5_aic<- list()
    river_list_selection_5_bic<- list()
    
    river_list_mse_step_5_aic_evaluation_oos<-list()
    river_list_mse_step_5_bic_evaluation_oos<-list()
    #error on full dataset
    river_list_mse_step_5_aic_evaluation<-list()
    river_list_mse_step_5_bic_evaluation<-list()
  }
  do_rf<-T
  #initializing rf formulas
  if(do_rf==T){
    per_river_list_mse_mean_rf <- list()
    river_list_mse_mean_rf <- list()
    river_list_rf_Model <- list()
    river_list_mse_rf_evaluation_oos<- list()
    #error on full dataset
    river_list_mse_rf_evaluation<- list()
  }
  
  
  for (river_number in 1:length(river_list_data_train)) {
    #river_number <- 1
    list_data_train_river<- river_list_data_train[[river_number]]
    list_data_test_river<-river_list_data_test [[river_number]]
    list_data_train_bacteria_river<-river_list_data_train_bacteria [[river_number]]
    list_data_test_bacteria_river<-river_list_data_test_bacteria [[river_number]]
    
    #step
    if (do_step_full==T)
    {
      list_selection_aic<-  list()
      list_mse_step_aic<-list()
      list_selection_bic<-  list()
      list_mse_step_bic<-list()
      null1<- NA
      full1<- NA
      indx_test<- 1
      
      
      for(entry in list_data_train_river){
        #entry <- list_data_train_river[[1]]
        data_train<- entry
        data_test <- list_data_test_river[[indx_test]]
        test_bacteria <- list_data_test_bacteria_river[[indx_test]]
        n<-nrow(data_train)
        
        null <- lm(log_e.coli ~ 1, data = data_train) #model with only 1 variable
        null1<- null
        full <- lm(log_e.coli ~ .^2, data = data_train)
        full1<- full
        
        
        selection_aic <- step(null1, data = data_train ,
                              
                              direction = "forward",
                              
                              list(lower=null1, upper=full1), k = 2)   
        list_selection_aic<- append(list_selection_aic, list(selection_aic))
        
        prediction_aic<- predict(object = selection_aic, newdata = data_test)
        mse_step_aic <-mean(sqrt((test_bacteria - prediction_aic)^2))
        list_mse_step_aic<- append(list_mse_step_aic, list(mse_step_aic))
        
        selection_bic <- step(null1, data = data_train ,
                              
                              direction = "forward",
                              
                              list(lower=null1, upper=full1), k = log(n) )   
        list_selection_bic<- append(list_selection_bic, list(selection_bic))
        prediction_bic<- predict(object = selection_bic, newdata = data_test)
        mse_step_bic <-mean(sqrt((test_bacteria - prediction_bic)^2))
        list_mse_step_bic<- append(list_mse_step_bic, list(mse_step_bic))
        
        
        
        indx_test<- indx_test+1
      }  
      
      per_river_list_mse_mean_step_aic<- append(per_river_list_mse_mean_step_aic,list(list_mse_step_aic))
      per_river_list_mse_mean_step_bic<- append(per_river_list_mse_mean_step_bic,list(list_mse_step_bic))
      
      mse_mean_step_aic <- mean(unlist(list_mse_step_aic))
      mse_mean_step_bic <- mean(unlist(list_mse_step_bic))
      
      river_list_selection_aic<-append(river_list_selection_aic,list(list_selection_aic))
      river_list_selection_bic<-append(river_list_selection_bic,list(list_selection_bic))
      
      
      
      river_list_mse_mean_step_aic<- append(river_list_mse_mean_step_aic,mse_mean_step_aic)
      river_list_mse_mean_step_bic<- append(river_list_mse_mean_step_bic,mse_mean_step_bic)
    }
    
    if (do_step_5==T)
    {
      list_selection_5_aic<-  list()
      list_mse_step_5_aic<-list()
      list_selection_5_bic<-  list()
      list_mse_step_5_bic<-list()
      null1<- null
      full1<- full
      indx_test<- 1
      
      
      for(entry in list_data_train_river){
        #entry <- list_data_train_river[[1]]
        data_train<- entry
        data_test <- list_data_test_river[[indx_test]]
        test_bacteria <- list_data_test_bacteria_river[[indx_test]]
        n<-nrow(data_train)
        
        null <- lm(log_e.coli ~ 1, data = data_train) #model with only 1 variable
        null1<- null
        full <- lm(log_e.coli ~ .^2, data = data_train)
        full1<- full
        
        
        selection_5_aic <- step(null1, data = data_train ,
                                
                                direction = "forward",
                                
                                list(lower=null1, upper=full1), k = 2, steps = 5)   
        list_selection_5_aic<- append(list_selection_5_aic, list(selection_5_aic))
        
        prediction_aic<- predict(object = selection_5_aic, newdata = data_test)
        mse_step_5_aic <-mean(sqrt((test_bacteria - prediction_aic)^2))
        list_mse_step_5_aic<- append(list_mse_step_5_aic, list(mse_step_5_aic))
        
        selection_5_bic <- step(null1, data = data_train ,
                                
                                direction = "forward",
                                
                                list(lower=null1, upper=full1), k = log(n) , steps = 5)   
        
        list_selection_5_bic<- append(list_selection_5_bic, list(selection_5_bic))
        prediction_bic<- predict(object = selection_5_bic, newdata = data_test)
        mse_step_5_bic <-mean(sqrt((test_bacteria - prediction_bic)^2))
        list_mse_step_5_bic<- append(list_mse_step_5_bic, list(mse_step_5_bic))
        
        
        
        indx_test<- indx_test+1
      }  
      
      per_river_list_mse_mean_step_5_aic<- append(per_river_list_mse_mean_step_5_aic,list(list_mse_step_5_aic))
      per_river_list_mse_mean_step_5_bic<- append(per_river_list_mse_mean_step_5_bic,list(list_mse_step_5_bic))
      
      mse_mean_step_5_aic <- mean(unlist(list_mse_step_5_aic))
      mse_mean_step_5_bic <- mean(unlist(list_mse_step_5_bic))
      
      river_list_selection_5_aic<-append(river_list_selection_5_aic,list(list_selection_5_aic))
      river_list_selection_5_bic<-append(river_list_selection_5_bic,list(list_selection_5_bic))
      
      
      
      river_list_mse_mean_step_5_aic<- append(river_list_mse_mean_step_5_aic,mse_mean_step_5_aic)
      river_list_mse_mean_step_5_bic<- append(river_list_mse_mean_step_5_bic,mse_mean_step_5_bic)
    } 
    
    
    
    if(do_rf==T){
      list_mse_rf <- list()
      list_rf_Model <- list()
      
      indx_test<-1
      for(entry in list_data_train_river){
        #entry <- list_data_train_river[[1]]
        data_train<- entry
        data_test <- list_data_test_river[[indx_test]]
        train_bacteria <- data_train$log_e.coli
        full_no_interaction <- lm(log_e.coli ~ ., data = data_train)
        full_no_interaction_test <- lm(log_e.coli ~ ., data = data_test)
        train <- model.matrix(full_no_interaction, data_train)
        test <- model.matrix(full_no_interaction_test, data_test)
        
        test_bacteria <- list_data_test_bacteria_river[[indx_test]]
        
        #null <- lm(log_e.coli ~ 1, data = data_train) #model with only 1 variable
        #null1<- null
        #full <- lm(log_e.coli ~ .^2, data = data_train)
        #full1<- full
        
        rfModel<-randomForest(train, y = train_bacteria, na.rm =T, keep.forest = T) 
        
        #names(data_train)
        #names(data_test)
        
        
        list_rf_Model<- append(list_rf_Model, list(rfModel))
        
        
        prediction_rf<- predict(rfModel, newdata = test)
        
        mse_rf <-mean(sqrt((test_bacteria - prediction_rf)^2))
        list_mse_rf<- append(list_mse_rf, list(mse_rf))
        
        
        
        indx_test<- indx_test+1
      }  
      per_river_list_mse_mean_rf <- append(per_river_list_mse_mean_rf,list(list_mse_rf))
      mse_mean_rf <- mean(unlist(list_mse_rf))
      
      river_list_mse_mean_rf <- append(river_list_mse_mean_rf,mse_mean_rf)
      river_list_rf_Model <- append(river_list_rf_Model,list(list_rf_Model))
    }
    
    
  }
  
  if (do_lasso==T) {
    iteration_river_list_lasso_Model<-append(iteration_river_list_lasso_Model,list(river_list_lasso_Model))
    iteration_river_list_mse_mean_lasso_lambda_min<- append(iteration_river_list_mse_mean_lasso_lambda_min, list(river_list_mse_mean_lasso_lambda_min))
    iteration_river_list_mse_mean_lasso_lambda_1se<- append(iteration_river_list_mse_mean_lasso_lambda_1se, list(river_list_mse_mean_lasso_lambda_1se))
  }
  
  if(do_rf==T){
    iteration_river_list_rf_Model<-append(iteration_river_list_rf_Model,list(river_list_rf_Model))
    iteration_river_list_mse_mean_rf <- append(iteration_river_list_mse_mean_rf,list(river_list_mse_mean_rf))
  }
  
  
  if(do_step_full==T){
    iteration_river_list_mse_mean_step_aic <- append(iteration_river_list_mse_mean_step_aic, list(river_list_mse_mean_step_aic))
    iteration_river_list_mse_mean_step_bic <- append(iteration_river_list_mse_mean_step_bic, list(river_list_mse_mean_step_bic))
    iteration_river_list_selection_aic<-append(iteration_river_list_selection_aic,list(river_list_selection_aic))
    iteration_river_list_selection_bic<-append(iteration_river_list_selection_bic,list(river_list_selection_bic))
  }
  #prediction and mse with found models
  
  if(do_step_5==T){
    iteration_river_list_mse_mean_step_5_aic <- append(iteration_river_list_mse_mean_step_5_aic, list(river_list_mse_mean_step_5_aic))
    iteration_river_list_mse_mean_step_5_bic <- append(iteration_river_list_mse_mean_step_5_bic, list(river_list_mse_mean_step_5_bic))
    iteration_river_list_selection_5_aic <-append(iteration_river_list_selection_5_aic,list(river_list_selection_5_aic))
    iteration_river_list_selection_5_bic<-append(iteration_river_list_selection_5_bic,list(river_list_selection_5_bic))
    
  }
  
  
}

end_time<-Sys.time()

processing_time<-end_time-start_time
processing_time


do_save_data_and_models <- F
if(do_save_data_and_models==T){
  #data 
  #- for analysis part
  saved_iteration_river_list_data<- iteration_river_list_data
  saved_iteration_river_list_foldids <- iteration_river_list_foldids
  saved_iteration_river_list_data_train<- iteration_river_list_data_train
  saved_iteration_river_list_data_test<- iteration_river_list_data_test 
  saved_iteration_river_list_lasso_Model<-iteration_river_list_lasso_Model
  saved_iteration_river_list_rf_Model<-iteration_river_list_rf_Model
  saved_iteration_river_list_selection_aic<- iteration_river_list_selection_aic
  saved_iteration_river_list_selection_bic <-iteration_river_list_selection_bic
  saved_iteration_river_list_selection_5_aic<- iteration_river_list_selection_5_aic
  saved_iteration_river_list_selection_5_bic <-iteration_river_list_selection_5_bic
  #for analysis part  
  #for analysis part
  
  #model s
  #for analysis part
  #for analysis part
  #for analysis part
}


#validation dataset
######## 
{
  iteration_river_list_data_validation <- list()
  iteration_river_list_data_train_validation <- list()
  iteration_river_list_data_test_validation <- list()
  
  iteration_river_list_foldids_validation <- list()
  set.seed(1)
  validation_samples<-sample(100:150, size = 5)
  for (validation_iteration in validation_samples) {
  #for (validation_iteration in 16:20) {
#  for (validation_iteration in 21:26) {  
    #validation_iteration <- 11
    river_list_data_validation<-list()
    river_list_foldids_validation<-list()
    #preloading data
    {
      list_train_rows_validation <- list()
      list_test_rows_validation <- list()
      for (river_path_river in list_river_pathes) {
        
        river_paths<-river_path_river
        
        #river_paths<- list_river_pathes[[6]]
        {
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
        }
        
        river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
        names(river_data) <- rivers
        {
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
        }
        #run here
        iterations <-validation_iteration
        new_train_test_split <-T
        
        set.seed(iterations)
        pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)"
        riverdata <- river_data_ts[[river]]
        # prepare variables out of all cominations (given by pattern)
        # variables for interaction get replaced by q_new (remove q_old)
        vars1 <- (riverdata[-1] %>% unroll_physical_data() %>%
                    lapply(names) %>% unlist() %>% unique())[-1]
        
        vars2 <- vars1[stringr::str_detect(vars1, pattern)]
        # prepare formulas
        
        data <- process_model_riverdata(riverdata, c("log_e.coli", vars2)) %>%  dplyr::select(-datum) 
        data <- na.omit(data)
        
        river_list_data_validation <- append(river_list_data_validation,list(data))
        
        datapoints<-seq(1, nrow(data))
        
        
        if(new_train_test_split ==T){
          foldid=sample(rep(seq(5),length=nrow(data)))  #fixes the train/test for cv.glmnet
          #length(foldid)
          river_list_foldids_validation<- append(river_list_foldids_validation, list(foldid))
          train_rows<- list()
          test_rows<- list()
          for (fold in 1:5) {
            train_rows[[fold]]<-datapoints[foldid!=fold] 
            test_rows[[fold]]<-datapoints[foldid==fold] 
          }
          list_train_rows_validation <- append(list_train_rows_validation,list(train_rows))
          list_test_rows_validation <- append(list_test_rows_validation,list(test_rows))
        }
        
      }  
    }
  }
}

#list_train_rows_validation_alt[[6]][[1]] ==list_train_rows_validation[[6]][[1]]

#### load new validation_data
{
  #new data_train data_test
  
  
  river_list_data_train_validation <- list()
  river_list_data_test_validation <- list()
  river_list_data_train_bacteria <- list()
  river_list_data_test_bacteria <- list()
  #rive_data initalization
  for (indx_river in 1:length(river_list_data_validation)) {
    #indx_river<-1
    list_data_train_validation<- list()
    list_data_test_validation<- list()
    list_data_train_bacteria_validation<- list()
    list_data_test_bacteria_validation<- list()
    
    data <- river_list_data_validation[[indx_river]]
    train_rows<- list_train_rows_validation[[indx_river]]
    test_rows<- list_test_rows_validation[[indx_river]]
    for(indx_fold in 1:length(train_rows)){
      #indx_fold<-5
      data_train_validation <- data[train_rows[[indx_fold]],]
      data_train_validation <-data.frame(scale(data_train_validation))
      list_data_train_validation<- append(list_data_train_validation, list(data_train_validation))
      
      #test_data fold
      data_test_validation <- data[-train_rows[[indx_fold]],]
      data_test_validation <-data.frame(scale(data_test_validation))
      
      
      list_data_test_validation<- append(list_data_test_validation, list(data_test_validation))
      
      train_bacteria_validation <- data_train_validation$log_e.coli
      list_data_train_bacteria_validation<- append(list_data_train_bacteria_validation, list(train_bacteria_validation))
      
      test_bacteria_validation <- data_test_validation$log_e.coli
      list_data_test_bacteria_validation<- append(list_data_test_bacteria_validation, list(test_bacteria_validation))
    }
    
    river_list_data_train_validation <- append(river_list_data_train_validation, list(list_data_train_validation))
    river_list_data_test_validation <- append(river_list_data_test_validation, list(list_data_test_validation))
    river_list_data_train_bacteria <- append(river_list_data_train_bacteria, list(list_data_train_bacteria_validation))
    river_list_data_test_bacteria <- append(river_list_data_test_bacteria, list(list_data_test_bacteria_validation))
  }
  
  iteration_river_list_data_validation<- append(iteration_river_list_data_validation, list(river_list_data_validation))
  
  iteration_river_list_foldids_validation <- append(iteration_river_list_foldids_validation, list(river_list_foldids_validation))
  
  iteration_river_list_data_train_validation <- append(iteration_river_list_data_train_validation, list(river_list_data_train_validation))
  iteration_river_list_data_test_validation <- append(iteration_river_list_data_test_validation, list(river_list_data_test_validation))
}
do_save<- T
{
  if(do_save==T){
    
    saved_iteration_river_list_data_validation<- iteration_river_list_data_validation
    
    saved_iteration_river_list_foldids_validation <- iteration_river_list_foldids_validation
    
    saved_iteration_river_list_data_train_validation<- iteration_river_list_data_train_validation
    saved_iteration_river_list_data_test_validation<- iteration_river_list_data_test_validation 
  }
}

write_saved_to_file <- F
if(write_saved_to_file==T){
  
  path1 <- "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/final_results/"
  
  save_object_as_rds_file_At_path<-function(object_to_save, path){
    savepath<-paste(path, deparse(substitute(object_to_save)),".rds", sep = "")
    saveRDS(object =  object_to_save, file =  savepath)
  }
  save_object_as_rds_file_At_path(saved_iteration_river_list_foldids, path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_foldids_validation, path1)
  
  save_object_as_rds_file_At_path(saved_iteration_river_list_data,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_data_validation,path1)
  
  save_object_as_rds_file_At_path(saved_iteration_river_list_data_train,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_data_train_validation,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_data_test,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_data_test_validation,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_lasso_Model,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_rf_Model,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_selection_aic,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_selection_bic,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_selection_5_aic,path1)
  save_object_as_rds_file_At_path(saved_iteration_river_list_selection_5_bic,path1)
  
}






##stop

do_save_mses_all_models <- F
if (do_save_mses_all_models==T) {
  
  saved_iteration_river_list_mse_mean_lasso_lambda_min<-iteration_river_list_mse_mean_lasso_lambda_min
  saved_iteration_river_list_mse_mean_lasso_lambda_1se<-iteration_river_list_mse_mean_lasso_lambda_1se
  
  saved_iteration_river_list_mse_mean_rf<-iteration_river_list_mse_mean_rf
  
  saved_iteration_river_list_mse_mean_step_aic<-iteration_river_list_mse_mean_step_aic
  saved_iteration_river_list_mse_mean_step_bic<-iteration_river_list_mse_mean_step_bic
  
  saved_iteration_river_list_mse_mean_step_5_aic<-iteration_river_list_mse_mean_step_5_aic
  saved_iteration_river_list_mse_mean_step_5_bic<-iteration_river_list_mse_mean_step_5_bic
}