havel<-saved_river_list_df_all_algorithms_with_mse_on_test[[1]]
ilz<- saved_river_list_df_all_algorithms_with_mse_on_test[[2]]
isar<- saved_river_list_df_all_algorithms_with_mse_on_test[[3]]
rhein<- ruhr<- saved_river_list_df_all_algorithms_with_mse_on_test[[4]]
mosel<- ruhr<- saved_river_list_df_all_algorithms_with_mse_on_test[[5]]
ruhr<- saved_river_list_df_all_algorithms_with_mse_on_test[[6]]

#havel_MSE_lasso<-havel%>% select(MSE, lasso_formula)%>% filter(lasso_formula==T)

{
  check_if_formula_was_found_more_than_once<-function(df_all_algorithms_with_mse_on_test){
    
    #sorted_modellist<-havel
    df_all_algorithms_with_mse_on_test$n_times <- 0
    
    
    for(r in 1:nrow(df_all_algorithms_with_mse_on_test)){
      counter<-0
      if (df_all_algorithms_with_mse_on_test$lasso[r] == T) {
        counter<-counter+1
        
      }else if (df_all_algorithms_with_mse_on_test$rf[r] == T) {
        counter<-counter+1
        
      }else if (df_all_algorithms_with_mse_on_test$step[r] == T) {
        counter<-counter+1
        
      }else if (df_all_algorithms_with_mse_on_test$elnet[r] == T) {
        counter<-counter+1
        
      }
      
      df_all_algorithms_with_mse_on_test$n_times[r]<- counter
    }
    
    return(df_all_algorithms_with_mse_on_test)
  }
}

havel<-check_if_formula_was_found_more_than_once(havel)
ilz<-check_if_formula_was_found_more_than_once(ilz)
isar<-check_if_formula_was_found_more_than_once(isar)
rhein<-check_if_formula_was_found_more_than_once(rhein)
mosel<-check_if_formula_was_found_more_than_once(mosel)
ruhr<-check_if_formula_was_found_more_than_once(ruhr)
{
build_algorithm_colum <-function(sorted_modellist){
  sorted_modellist$algo <- 'init'
  sorted_modellist$algo[sorted_modellist$lasso==T]<-'lasso'
  sorted_modellist$algo[sorted_modellist$elnet==T]<-'elnet'
  sorted_modellist$algo[sorted_modellist$step==T]<-'step'
  sorted_modellist$algo[sorted_modellist$rf==T]<-'rf'
  
  return(sorted_modellist)  
}
}
havel<-build_algorithm_colum(havel)
ilz<-build_algorithm_colum(ilz)
isar<-build_algorithm_colum(isar)
rhein<-build_algorithm_colum(rhein)
mosel<-build_algorithm_colum(mosel)
ruhr<-build_algorithm_colum(ruhr)


havel$algo <- 'init'
havel$algo[havel$lasso_formula==T]<-'lasso'
havel$algo[havel$elnet_formula==T]<-'elnet'
havel$algo[havel$step_formula==T]<-'step'
havel$algo[havel$rf_formula==T]<-'rf'

#havel$lasso_formula[havel$lasso_formula==T]<-2
#havel$elnet_formula[havel$elnet_formula==T]<-2
#havel$step_formula[havel$step_formula==T]<-2
#havel$rf_formula[havel$rf_formula==T]<-2


box_df <- rbind(havel, isar, ilz, rhein, mosel, ruhr)

par(mfrow = c(1,2))
boxplot(havel$mse~havel$algo)
boxplot(havel$adj.r.squared~havel$algo)

boxplot(ilz$mse~ilz$algo)
boxplot(ilz$adj.r.squared~ilz$algo)

boxplot(isar$mse~isar$algo)
boxplot(isar$adj.r.squared~isar$algo)

boxplot(rhein$mse~rhein$algo)
boxplot(rhein$adj.r.squared~rhein$algo)

boxplot(mosel$mse~mosel$algo)
boxplot(mosel$adj.r.squared~mosel$algo)

boxplot(ruhr$mse~ruhr$algo)
boxplot(ruhr$adj.r.squared~ruhr$algo)



boxplot(box_df$mse~box_df$algo)
boxplot(box_df$adj.r.squared~box_df$algo)


boxplot(havel$adj.r.squared~havel$algo)
boxplot(ilz$adj.r.squared~ilz$algo)
boxplot(isar$adj.r.squared~isar$algo)
boxplot(rhein$adj.r.squared~rhein$algo)
boxplot(mosel$adj.r.squared~mosel$algo)
boxplot(ruhr$adj.r.squared~ruhr$algo)

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


iteration_river_list_df_min_mse_algo_oos<- list()
iteration_river_list_rf_Model<-list()
iteration_river_list_selection_aic<-list()
iteration_river_list_selection_bic <-list()
for(iteration in 1:15){
  
  river_list_data<-list()
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
  
  #new data_train data_test
  
  
  river_list_data_train <- list()
  river_list_data_test <- list()
  river_list_data_train_bacteria <- list()
  river_list_data_test_bacteria <- list()
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
  
  
  
  
  
  do_step_full<-T
  #initializing step formulas
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
        #fmla[[i]] <- as.list(selection[[i]]$call)$formula
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
    
    
    
    
    
    if(do_rf<-T){
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
      
      river_list_mse_mean_rf <- append(river_list_mse_mean_rf,list(mse_mean_rf))
      river_list_rf_Model <- append(river_list_rf_Model,list(list_rf_Model))
    }
  }
  
  iteration_river_list_rf_Model<-append(iteration_river_list_rf_Model,list(river_list_rf_Model))
  iteration_river_list_selection_aic<-append(iteration_river_list_selection_aic,list(river_list_selection_aic))
  iteration_river_list_selection_bic<-append(iteration_river_list_selection_bic,list(river_list_selection_bic))
  
  #prediction and mse with found models
  
  river_list_selection_aic
  
  
  ################################
  
  
  do_full<-F
  if(do_full==T)
  #error on whole dataset of every model
  do_rf_prediction<-T
  do_step_prediction<-T
  for (river_number in 1:length(river_list_data_train)) {
    #river_number<-3
    data_river<-river_list_data [[river_number]]
    data_river <- as.data.frame(scale(data_river))
    
    data_bacteria_river<-data_river$log_e.coli
    
    list_of_rf_models_per_river <- river_list_rf_Model[[river_number]] #6*5 models --> 30 models to test #mses = 150 
    
    list_of_step_aic_models_per_river<-river_list_selection_aic[[river_number]]
    list_of_step_bic_models_per_river<-river_list_selection_bic[[river_number]]
    
    list_models_mse_rf_evaluation<-list()
    full_no_interaction_test <- lm(log_e.coli ~ ., data = data_river)
    full_df <- model.matrix(full_no_interaction_test, data_river)
    #rf prediction
    if(do_rf_prediction==T){
      for(entry in list_of_rf_models_per_river){ #5 models
        #entry<- list_of_rf_models_per_river[[1]]
        rfModel <- entry
        list_mse_rf_evaluation<- list()
        
        prediction_rf<- predict(rfModel, newdata = full_df)
        mse_rf <-mean(sqrt((data_bacteria_river - prediction_rf)^2))
        
        list_mse_rf_evaluation<- append(list_mse_rf_evaluation, list(mse_rf))
        
        list_models_mse_rf_evaluation<- append(list_models_mse_rf_evaluation, list(list_mse_rf_evaluation))
      }  
      #river_list_mse_rf_evaluation<-list()
      river_list_mse_rf_evaluation<- append(river_list_mse_rf_evaluation,list(list_models_mse_rf_evaluation))
    }
    
    list_models_mse_step_aic_evaluation<-list()
    list_models_mse_step_bic_evaluation<-list()
    #step prediction
    if(do_step_prediction==T){
      for(entry_idx in 1:length(list_of_step_aic_models_per_river)){ #5 models
        #entry_idx<-1
        step_aic_Model <- list_of_step_aic_models_per_river[[entry_idx]]
        step_bic_Model <- list_of_step_bic_models_per_river[[entry_idx]]
        
        list_mse_step_aic_evaluation<- list()
        list_mse_step_bic_evaluation<- list()
        
        prediction_step_aic<- predict(step_aic_Model, newdata = data_river)
        prediction_step_bic<- predict(step_bic_Model, newdata = data_river)
        
        mse_step_aic <-mean(sqrt((data_bacteria_river - prediction_step_aic)^2))
        mse_step_bic <-mean(sqrt((data_bacteria_river - prediction_step_bic)^2))
        
        list_mse_step_aic_evaluation<- append(list_mse_step_aic_evaluation, list(mse_step_aic))
        list_mse_step_bic_evaluation<- append(list_mse_step_bic_evaluation, list(mse_step_bic))
        
        list_models_mse_step_aic_evaluation<- append(list_models_mse_step_aic_evaluation, list(list_mse_step_aic_evaluation))
        list_models_mse_step_bic_evaluation<- append(list_models_mse_step_bic_evaluation, list(list_mse_step_bic_evaluation))
      }  
      #river_list_mse_step_aic_evaluation<-list()
      #river_list_mse_step_bic_evaluation<-list()
      river_list_mse_step_aic_evaluation<- append(river_list_mse_step_aic_evaluation,list(list_models_mse_step_aic_evaluation))
      river_list_mse_step_bic_evaluation<- append(river_list_mse_step_bic_evaluation,list(list_models_mse_step_bic_evaluation))
      
    }
    
}
  
  
  par(mfrow=(c(1,1)))
    
    build_mse_df<-function(river_list_mse_algo_evaluation){
      #river_list_mse_algo_evaluation<-river_list_mse_step_bic_evaluation
      names_river <- list("havel", "isar", "ilz", "rhein", "mosel","ruhr")
      list_df<-list()
      for (river_idx in 1:length(names_river)) {
        
        #river_list_mse_algo_evaluation<- river_list_mse_step_aic_evaluation
        mse_model_1<-mean(unlist(river_list_mse_algo_evaluation[[river_idx]][[1]])) #mses from fold 1-5 for model1
        mse_model_2<-mean(unlist(river_list_mse_algo_evaluation[[river_idx]][[2]])) #mses from fold 1-5 for model1
        mse_model_3<-mean(unlist(river_list_mse_algo_evaluation[[river_idx]][[3]])) #mses from fold 1-5 for model1
        mse_model_4<-mean(unlist(river_list_mse_algo_evaluation[[river_idx]][[4]])) #mses from fold 1-5 for model1
        mse_model_5<-mean(unlist(river_list_mse_algo_evaluation[[river_idx]][[5]])) #mses from fold 1-5 for model1
        
        
        
        river_model_mses <- c(mse_model_1,mse_model_2,mse_model_3,mse_model_4,mse_model_5)
        #this are the mses for the found models on fixed river and over all mses
        df_models_mses<-data.frame(river_model_mses)
        
        algo_name<-deparse(substitute(river_list_mse_algo_evaluation))
        if(grepl("step_aic",algo_name) ){
          algo_name<- "step_aic"
        }else if(grepl("step_bic",algo_name) ){
          algo_name<- "step_bic"
        }else if(grepl("rf",algo_name) ){
          algo_name<- "rf"
        }
        
        df_models_mses$names <- algo_name
        list_df<-append(list_df, list(df_models_mses))
      }
      names(list_df)<-names_river
      return(list_df)
  }
  
  do_plot_full<-F
  if(do_plot_full==T){
    all_rivers_step_aic<-build_mse_df(river_list_mse_step_aic_evaluation)
    all_rivers_step_bic<-build_mse_df(river_list_mse_step_bic_evaluation)
    all_rivers_rf<-build_mse_df(river_list_mse_rf_evaluation)
    names_river <- list("havel", "isar", "ilz", "rhein", "mosel","ruhr")
    for (i in 1:6) {
      df_together <-rbind(all_rivers_step_aic[[i]],all_rivers_step_bic[[i]],all_rivers_rf[[i]])
      
      boxplot(df_together$river_model_mses ~df_together$names,main= names_river[i] )
    }
    
}  
    #df_havel<- rbind(havel_step_aic,havel_step_bic,havel_rf)
    
    #boxplot(df_havel$river_model_mses~df_ioio$names)
  
    
    river_list_mse_step_aic_evaluation_oos<-list()
    river_list_mse_step_bic_evaluation_oos<-list()
    river_list_mse_rf_evaluation_oos <- list()
    
    #do_oos<-T
    #if(do_oos=T){ #out of sample error
      do_rf_prediction<-T
      do_step_prediction<-T
      for (river_number in 1:length(river_list_data_train)){
        #river_number<-1
        list_data_test_river_oos<-river_list_data_test [[river_number]]
        list_data_test_bacteria_river_oos<-river_list_data_test_bacteria [[river_number]] # 5 tests 
        
        list_of_rf_models_per_river_oos <- river_list_rf_Model[[river_number]] #6*5 models --> 30 models to test #mses = 150 
        
        list_of_step_aic_models_per_river_oos<-river_list_selection_aic[[river_number]]
        list_of_step_bic_models_per_river_oos<-river_list_selection_bic[[river_number]]
        
        list_models_mse_rf_evaluation_oos<-list()
        
        if(do_rf_prediction==T){
          for(entry in list_of_rf_models_per_river_oos){ #5 models
            #entry<- list_of_rf_models_per_river_oos[[1]]
            rfModel <- entry
            list_mse_rf_evaluation_oos<- list()
            for (index_prediction in 1:length(list_data_test_river_oos)){
              #index_prediction<-1
              indx_test<- index_prediction
              data_test <- list_data_test_river_oos[[indx_test]]
              
              full_no_interaction_test <- lm(log_e.coli ~ ., data = data_test)
              
              test <- model.matrix(full_no_interaction_test, data_test)
              
              test_bacteria <- list_data_test_bacteria_river_oos[[indx_test]]
              
              
              prediction_rf<- predict(rfModel, newdata = test)
              
              mse_rf <-mean(sqrt((test_bacteria - prediction_rf)^2))
              list_mse_rf_evaluation_oos<- append(list_mse_rf_evaluation_oos, list(mse_rf))
              
              
              
              indx_test<- indx_test+1
            }
            list_models_mse_rf_evaluation_oos<- append(list_models_mse_rf_evaluation_oos, list(list_mse_rf_evaluation_oos))
          }  
          river_list_mse_rf_evaluation_oos<- append(river_list_mse_rf_evaluation_oos,list(list_models_mse_rf_evaluation_oos))
        }
        
        
        list_models_mse_step_aic_evaluation_oos<-list()
        list_models_mse_step_bic_evaluation_oos<-list()
        
        do_step_prediction<-T
        if(do_step_prediction==T){
          for(entry_idx in 1:length(list_of_step_aic_models_per_river_oos)){ #5 models
            #entry<- list_of_step_aic_models_per_river_oos[[1]]
            #entry_idx <-1
            step_aic_Model <- list_of_step_aic_models_per_river_oos[[entry_idx]]
            step_bic_Model <- list_of_step_bic_models_per_river_oos[[entry_idx]]
            
            list_mse_step_aic_evaluation_oos<- list()
            list_mse_step_bic_evaluation_oos<- list()
            for (index_prediction in 1:length(list_data_test_river_oos)){
              #index_prediction<-1
              indx_test<- index_prediction
              data_test <- list_data_test_river_oos[[indx_test]]
              
              #full_no_interaction_test <- lm(log_e.coli ~ ., data = data_test)
              
              #test <- model.matrix(full_no_interaction_test, data_test)
              
              test_bacteria <- list_data_test_bacteria_river_oos[[indx_test]]
              
              
              prediction_step_aic<- predict(step_aic_Model, newdata = data_test)
              prediction_step_bic<- predict(step_bic_Model, newdata = data_test)
              
              mse_step_aic <-mean(sqrt((test_bacteria - prediction_step_aic)^2))
              list_mse_step_aic_evaluation_oos<- append(list_mse_step_aic_evaluation_oos, list(mse_step_aic))
              
              mse_step_bic <-mean(sqrt((test_bacteria - prediction_step_bic)^2))
              list_mse_step_bic_evaluation_oos<- append(list_mse_step_bic_evaluation_oos, list(mse_step_bic))
              
              
              indx_test<- indx_test+1
            }
            list_models_mse_step_aic_evaluation_oos<- append(list_models_mse_step_aic_evaluation_oos, list(list_mse_step_aic_evaluation_oos))
            list_models_mse_step_bic_evaluation_oos<- append(list_models_mse_step_bic_evaluation_oos, list(list_mse_step_bic_evaluation_oos))
          }  
          river_list_mse_step_aic_evaluation_oos<- append(river_list_mse_step_aic_evaluation_oos,list(list_models_mse_step_aic_evaluation_oos))
          river_list_mse_step_bic_evaluation_oos<- append(river_list_mse_step_bic_evaluation_oos,list(list_models_mse_step_bic_evaluation_oos))
        }
      }
    #}    
    
  
   #calculate the mses per model over all 5 folds
    get_model_mses_on_all_5_test_fold <- function(river_list_mse_algo_evaluation){
      #river_list_mse_algo_evaluation<-river_list_mse_step_aic_evaluation_oos
      river_list_model_mean_mses_on_all_5_folds<-list()
      for (river_idx in 1:length(river_list_mse_algo_evaluation)) {
        #river_idx <-1
        list_model_mses<-river_list_mse_algo_evaluation[[river_idx]]
        list_model_mean_mses_on_all_5_folds<-list()
        for (model_idx in 1:length(list_model_mses)) {
          #model_idx<-1
          model_mean_mses_on_all_5_folds<-mean(unlist(list_model_mses[[model_idx]]))
          list_model_mean_mses_on_all_5_folds <-append(list_model_mean_mses_on_all_5_folds,list(model_mean_mses_on_all_5_folds))
        }
        river_list_model_mean_mses_on_all_5_folds<- append(river_list_model_mean_mses_on_all_5_folds, list(list_model_mean_mses_on_all_5_folds))
      }
      return(river_list_model_mean_mses_on_all_5_folds)
    }
    
    river_list_df_min_mse_algo_oos <- list()
    for(river_idx in 1:6){
      #river_idx<-1
      min_mse_step_aic_oos<-min(unlist(get_model_mses_on_all_5_test_fold(river_list_mse_step_aic_evaluation_oos)[[river_idx]]))
      min_mse_step_bic_oos<-min(unlist(get_model_mses_on_all_5_test_fold(river_list_mse_step_bic_evaluation_oos)[[river_idx]]))
      min_mse_rf_oos<-min(unlist(get_model_mses_on_all_5_test_fold(river_list_mse_rf_evaluation_oos)[[river_idx]]))
      
      df_min_mse_algo_oos <- data.frame(min_mse_step_aic_oos,min_mse_step_bic_oos,min_mse_rf_oos)
      river_list_df_min_mse_algo_oos <- append(river_list_df_min_mse_algo_oos, list(df_min_mse_algo_oos))
      
    }
   
    
    iteration_river_list_df_min_mse_algo_oos<-append(iteration_river_list_df_min_mse_algo_oos,list(river_list_df_min_mse_algo_oos))
    
    
    
  
    
   
}
havel_mses_best_model_oos <- data.frame()
isar_mses_best_model_oos <- data.frame()
ilz_mses_best_model_oos <- data.frame()
mosel_mses_best_model_oos <- data.frame()
rhein_mses_best_model_oos <- data.frame()
ruhr_mses_best_model_oos <- data.frame()


for (iteration in 1:iterations) {
  #iteration <-1
  havel_mses_best_model_oos <-  rbind(havel_mses_best_model_oos,iteration_river_list_df_min_mse_algo_oos[[iteration]][[1]])
  isar_mses_best_model_oos <- rbind(isar_mses_best_model_oos,iteration_river_list_df_min_mse_algo_oos[[iteration]][[2]])
  ilz_mses_best_model_oos <- rbind(ilz_mses_best_model_oos,iteration_river_list_df_min_mse_algo_oos[[iteration]][[3]])
  mosel_mses_best_model_oos <- rbind(mosel_mses_best_model_oos,iteration_river_list_df_min_mse_algo_oos[[iteration]][[4]])
  rhein_mses_best_model_oos <- rbind(rhein_mses_best_model_oos,iteration_river_list_df_min_mse_algo_oos[[iteration]][[5]])
  ruhr_mses_best_model_oos <- rbind(ruhr_mses_best_model_oos,iteration_river_list_df_min_mse_algo_oos[[iteration]][[6]])
  
  
}  
algo_names<-c("step_aic","step_bic", "rf")
names(havel_mses_best_model_oos)<-names( isar_mses_best_model_oos)<- names(ilz_mses_best_model_oos)<- names(mosel_mses_best_model_oos)<- names(rhein_mses_best_model_oos)<- names(ruhr_mses_best_model_oos)<- c("step_aic","step_bic", "rf")
  
par(mfrow =c(1,1))
boxplot(havel_mses_best_model_oos, main= "havel")
boxplot(isar_mses_best_model_oos, main= "isar")
boxplot(ilz_mses_best_model_oos,main= "ilz")
boxplot(mosel_mses_best_model_oos, main= "mosel")
boxplot(rhein_mses_best_model_oos, main= "rhein")
boxplot(ruhr_mses_best_model_oos, main= "ruhr")

naming<-function(river_list_mse_mean_algo){
  names_river <- list("havel", "isar", "ilz",  "mosel","rhein","ruhr")
  for (idx_naming in 1:6) {
    names(river_list_mse_mean_algo)[[idx_naming]]<-names_river[[idx_naming]]
  }
  return(river_list_mse_mean_algo)
}

all_rivers_step_aic_oos<-build_mse_df(river_list_mse_step_aic_evaluation_oos)
all_rivers_step_bic_oos <-build_mse_df(river_list_mse_step_bic_evaluation_oos)
all_rivers_rf_oos<-build_mse_df(river_list_mse_rf_evaluation_oos)
par(mfrow = c(1,2))

for (i in 1:6) {
  df_together <-rbind(all_rivers_step_aic[[i]],all_rivers_step_bic[[i]],all_rivers_rf[[i]])
  df_together_oos <-rbind(all_rivers_step_aic_oos[[i]],all_rivers_step_bic_oos[[i]],all_rivers_rf_oos[[i]])
  max_value<-max(df_together_oos$river_model_mses, df_together$river_model_mses)
  min_value<-min(df_together_oos$river_model_mses, df_together$river_model_mses)
  boxplot(df_together$river_model_mses ~df_together$names,main= names_river[i], ylim=c(min_value, max_value) )
  boxplot(df_together_oos$river_model_mses ~df_together$names,main= paste(names_river[i], "OOS"),ylim=c(min_value, max_value) )
}

river_list_mse_mean_rf<-naming(river_list_mse_mean_rf)
river_list_mse_mean_step_aic<-naming(river_list_mse_mean_step_aic)
river_list_mse_mean_step_bic<-naming(river_list_mse_mean_step_bic)
                                     
river_list_mse_mean_rf$havel
river_list_mse_mean_step_aic$havel
river_list_mse_mean_step_bic$havel
uio<-unlist(unlist(river_list_mse_mean_rf))
boxplot(river_list_mse_mean_rf)
