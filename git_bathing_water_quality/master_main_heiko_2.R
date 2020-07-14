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
  
  library(kwb.flusshygiene)
}
#data
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
#INSTANZIERUNG
river_list_analysis_df <- list()

river_list_river_stat_tests<- list()
river_list_sorted_modellist <-list()

river_list_unique_found_formulas<-list()

river_list_unique_step_formulas<-list()
river_list_unique_lasso_formulas<-list()
river_list_unique_rf_formulas<-list()
river_list_unique_elnet_formulas<-list()

river_list_rf_feature_occurence<- list()
river_list_step_feature_occurence<- list()
river_list_lasso_feature_occurence<- list()
river_list_elnet_feature_occurence<- list()


river_list_df_all_algorithms_with_mse_on_test<-list()

for (river_path_river in list_river_pathes) {
  river_paths<-river_path_river

  #river_paths<- river_paths1
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
    iterations <-2
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
    
    #5-Fold_cross
    datapoints<-seq(1, nrow(data))
    
    if(new_train_test_split ==T){
      foldid=sample(rep(seq(5),length=nrow(data)))  #fixes the train/test for cv.glmnet
      train_rows<- list()
      for (fold in 1:5) {
        train_rows[[fold]]<-datapoints[foldid!=fold]  
      }
    }
    
    
    full_1 <- lm(log_e.coli ~ .^2, data = data) #only for df_with_all_variable_names
    df_with_all_variable_names <- model.matrix(full_1, data)
    {
      build_rename_cols_to_variable_names <- function(df_with_all_vars){
        df<-data.frame(matrix(0, ncol = length((colnames(df_with_all_vars)))+2, nrow = 1))
        colnames(df)<- colnames(df_with_all_vars)
        names(df)[length(names(df))-1]<-"formel" 
        names(df)[length(names(df))]<-"split_id" 
        
        
        return(df)
      }
    }
    
    
    
    
    #saving all selected features during every fold
    list_unique_step_formulas <- list()
    list_unique_rf_formulas <- list()
    list_unique_lasso_formulas <- list()
    list_unique_elnet_formulas <- list()
    
    step_features_occurence<-build_rename_cols_to_variable_names(df_with_all_variable_names)
    rf_features_occurence<-build_rename_cols_to_variable_names(df_with_all_variable_names)
    lasso_features_occurence<-build_rename_cols_to_variable_names(df_with_all_variable_names)
    elnet_features_occurence<-build_rename_cols_to_variable_names(df_with_all_variable_names)
    
    list_river_stat_tests<-list()
    #feature selection
    for (indx_fold in 1:length(train_rows)) {

      #instanzierung dataframes for coefficients
      {
        rf_df_1_coef <- build_rename_cols_to_variable_names(df_with_all_variable_names)
        rf_df_2_coef <- build_rename_cols_to_variable_names(df_with_all_variable_names)
        rf_df_3_coef<- build_rename_cols_to_variable_names(df_with_all_variable_names)
        rf_df_4_coef<- build_rename_cols_to_variable_names(df_with_all_variable_names)
        rf_df_5_coef<- build_rename_cols_to_variable_names(df_with_all_variable_names)
        
        step_df_1_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        step_df_2_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        step_df_3_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        step_df_4_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        step_df_5_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        
        lasso_df_1_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        lasso_df_2_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        lasso_df_3_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        lasso_df_4_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        lasso_df_5_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        
        elnet_df_1_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        elnet_df_2_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        elnet_df_3_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        elnet_df_4_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        elnet_df_5_coef<-build_rename_cols_to_variable_names(df_with_all_variable_names)
      }
      
      
      #indx_fold<-1
      #train_data fold
      data_train <- data[train_rows[[indx_fold]],]
      data_train <-data.frame(scale(data_train))
      #test_data fold
      data_test <- data[-train_rows[[indx_fold]],]
      data_test <-data.frame(scale(data_test))
      
      train_bacteria <- data_train$log_e.coli
      test_bacteria <- data_test$log_e.coli
      
      
      bacteria<-names(data_train)[1]
      form<-formula(paste(bacteria," ~ .^2"))
      
      null <- lm(log_e.coli ~ 1, data = data_train) #model with only 1 variable
      full <- lm(log_e.coli ~ .^2, data = data_train)
      
      
      train <- model.matrix(full, data_train)
      train_sparse <- sparse.model.matrix(full, data_train) #data must be dataframe
      
      df_with_all_variable_names <-train
      iteration_name<-paste("iterations", iterations,"fold",indx_fold,sep = "_")
      
      
      #feature_selection_step
      {
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
      }
      step_returns <- stepwise(river = river, pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)", data = data_train ,null, full)
      fmla <- step_returns[[1]]
      selection <- step_returns[[2]]
      {  
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
        {
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
        }
        var_step_model_1<- paste_together_step_coefficients(var_step_model_1)
        var_step_model_2<-paste_together_step_coefficients(var_step_model_2)
        var_step_model_3<-paste_together_step_coefficients(var_step_model_3)
        var_step_model_4<-paste_together_step_coefficients(var_step_model_4)
        var_step_model_5<-paste_together_step_coefficients(var_step_model_5)
        
        {
          get_new_iteration_row<-function(df_save,var_list){
            #df_save<-rf_df_1_coef
            #var_list<- rf_formula_11
            #df_save<-step_df_5_coef
            #names(df_save)<- names(rf_df_2_coef)
            #  var_list <-var_rf_model_3
            #var_list<-var_step_model_5
            #var_list<-var_rf_model_5
            #all(sapply(var_list, is.character))
            #df_save<-lasso_df_3_coef
            #var_list<-unique_lasso_formulas_coef_3
            
            #list_unqiue_formulas_all_algorithms
            
            o<-var_list #adds the formula
            
            if(all(sapply(var_list, is.character))==F){
              o<- o[[2]][1]
            }
            
            new_row <- data.frame(matrix(0, ncol = length((colnames(df_with_all_variable_names)))+2, nrow = 1))
            colnames(new_row)<- colnames(df_with_all_variable_names)
            new_row[ncol(new_row)-1] <- o
            names(new_row)[length(names(new_row))-1]<-"formel"
            new_row[ncol(new_row)] <- indx_fold
            names(new_row)[length(names(new_row))]<-"split_id"
            #check for variables
            a<-strsplit( o, split = " \\+ ")
            
            #add a 1 in var_column if formula contains variable
            for (idx in 1:length(a[[1]])) {
              #idx = 3
              if(grepl(':',a[[1]][idx] )){ #sometimes, step changes order of variables with interaction
                string1<-a[[1]][idx]
                new_string_list<-str_split(string1,":")
                string2 <- paste(new_string_list[[1]][2],new_string_list[[1]][1], sep = ":")
                i<-paste("^",string1, "$", sep = "")
                j<-paste("^",string2, "$", sep = "")
                indx1 <- grepl(i, colnames(df_save))
                indx2 <- grepl(j, colnames(df_save))
                new_row[1,indx1]=1
                new_row[1,indx2]=1
                
              }else{
                i<-paste("^",a[[1]][idx], "$", sep = "")
                
                indx <- grepl(i, colnames(df_save))
                #   any(indx)
                new_row[1,indx]=1
              }
              
            }
            
            #select(new_row,q_mean_mean_12, ka_mean_mean_45, ka_mean_mean_34,'ka_mean_mean_45:q_mean_mean_12')
            return(new_row)
            
          }
        }
        #new row
        new_row_step_df_1_coef <- get_new_iteration_row(step_df_1_coef, var_step_model_1 )
        new_row_step_df_2_coef <- get_new_iteration_row(step_df_2_coef, var_step_model_2 )
        new_row_step_df_3_coef <- get_new_iteration_row(step_df_3_coef, var_step_model_3 )
        new_row_step_df_4_coef <- get_new_iteration_row(step_df_4_coef, var_step_model_4 )
        new_row_step_df_5_coef <- get_new_iteration_row(step_df_5_coef, var_step_model_5 )
        
        #name new column
        row.names(new_row_step_df_1_coef) <- iteration_name
        row.names(new_row_step_df_2_coef) <- iteration_name
        row.names(new_row_step_df_3_coef) <- iteration_name
        row.names(new_row_step_df_4_coef) <- iteration_name
        row.names(new_row_step_df_5_coef) <- iteration_name
        
        #add new train row
        step_df_1_coef<-rbind(step_df_1_coef, new_row_step_df_1_coef)
        step_df_2_coef<-rbind(step_df_2_coef, new_row_step_df_2_coef)
        step_df_3_coef<-rbind(step_df_3_coef, new_row_step_df_3_coef)
        step_df_4_coef<-rbind(step_df_4_coef, new_row_step_df_4_coef)
        step_df_5_coef<-rbind(step_df_5_coef, new_row_step_df_5_coef)
        
        {
          remove_all_cols_that_are_zero <- function(df){
            #df<-a
            no_zero_df<-df[,apply(df,2,function(df) !all(df==0))]
            no_zero_df<- no_zero_df[-1,]
            return(no_zero_df)
          }
        }
        #remove all columns that are only 0
        # coefficients that a selected from
        step_df_1_coef_save <-remove_all_cols_that_are_zero(step_df_1_coef)
        step_df_2_coef_save <-remove_all_cols_that_are_zero(step_df_2_coef)
        step_df_3_coef_save <-remove_all_cols_that_are_zero(step_df_3_coef)
        step_df_4_coef_save <-remove_all_cols_that_are_zero(step_df_4_coef)
        step_df_5_coef_save <-remove_all_cols_that_are_zero(step_df_5_coef)
        
        unique_step_formulas<-unique(unlist(list(step_df_1_coef_save$formel,step_df_2_coef_save$formel,step_df_3_coef_save$formel,step_df_4_coef_save$formel,step_df_5_coef_save$formel)))
        
        list_unique_step_formulas <- unique(unlist(append(list_unique_step_formulas, list(unique_step_formulas))))
      }
      #step selection ende
      
      
      #rf_selection
      
      rfModel<-randomForest(train, y = train_bacteria, na.rm =T, keep.forest = T) 
      
      #most_selected_features_by random_forest with importance
      imp_rf <- as.data.frame(varImp(rfModel, scale=T))
      
      imp_rf <- data.frame(overall = imp_rf$Overall,
                           names   = rownames(imp_rf))
      imp_rf<-imp_rf[order(imp_rf$overall,decreasing = T),]
      
      {
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
      }
      {
        rf_formula_11<-build_formula(imp_rf[1,2])
        rf_formula_21<-build_formula(imp_rf[1:2,2]) #most important 1:2
        rf_formula_31<-build_formula(imp_rf[1:3,2])
        rf_formula_41<-build_formula(imp_rf[1:4,2])
        rf_formula_51<-build_formula(imp_rf[1:5,2])
        
        var_rf_model_1 <-  rf_formula_11[[2]]
        var_rf_model_2 <- rf_formula_21[[2]]
        var_rf_model_3 <- rf_formula_31[[2]]
        var_rf_model_4 <- rf_formula_41[[2]]
        var_rf_model_5 <- rf_formula_51[[2]]
        
        
        #add new row for every fold
        new_row_rf_df_1_coef <- get_new_iteration_row(rf_df_1_coef, rf_formula_11)
        new_row_rf_df_2_coef <- get_new_iteration_row(rf_df_2_coef, rf_formula_21)
        new_row_rf_df_3_coef <- get_new_iteration_row(rf_df_3_coef, rf_formula_31)
        new_row_rf_df_4_coef <- get_new_iteration_row(rf_df_4_coef, rf_formula_41)
        new_row_rf_df_5_coef <- get_new_iteration_row(rf_df_5_coef, rf_formula_51)
        
        #iteration_name<-paste("iterations", iterations,"fold",j,sep = "_")
        #naming_new_row
        row.names(new_row_rf_df_1_coef) <- iteration_name
        row.names(new_row_rf_df_2_coef) <- iteration_name
        row.names(new_row_rf_df_3_coef) <- iteration_name
        row.names(new_row_rf_df_4_coef) <- iteration_name
        row.names(new_row_rf_df_5_coef) <- iteration_name
        #bind new row to df
        rf_df_1_coef<-rbind(rf_df_1_coef, new_row_rf_df_1_coef)
        rf_df_2_coef<-rbind(rf_df_2_coef, new_row_rf_df_2_coef)
        rf_df_3_coef<-rbind(rf_df_3_coef, new_row_rf_df_3_coef)
        rf_df_4_coef<-rbind(rf_df_4_coef, new_row_rf_df_4_coef)
        rf_df_5_coef<-rbind(rf_df_5_coef, new_row_rf_df_5_coef)
        
        #rf_save
        rf_df_1_coef_save <-remove_all_cols_that_are_zero(rf_df_1_coef)
        rf_df_2_coef_save <-remove_all_cols_that_are_zero(rf_df_2_coef)
        rf_df_3_coef_save <-remove_all_cols_that_are_zero(rf_df_3_coef)
        rf_df_4_coef_save <-remove_all_cols_that_are_zero(rf_df_4_coef)
        rf_df_5_coef_save <-remove_all_cols_that_are_zero(rf_df_5_coef)
        
        unique_rf_formulas<-unique(unlist(list(rf_df_1_coef_save$formel,rf_df_2_coef_save$formel,rf_df_3_coef_save$formel,rf_df_4_coef_save$formel,rf_df_5_coef_save$formel)))
        
        list_unique_rf_formulas <- unique(unlist(append(list_unique_rf_formulas, list(unique_rf_formulas))))
        
      }
      
      #rf_selection ende
      
      {
        get_formula_mse_and_lambda_from_cg.glmnet_fit<- function(lambdas_with_number_of_coeff, fit_glmnet){
          #lambdas_with_number_of_coeff<-lambdas_lasso_fits_3_coef
          #lambdas_with_number_of_coeff<-fit_elnet_base_cross_stand$lambda.min
          #fit_glmnet<-fit_elnet_base_cross_stand
          #fit_glmnet<-fit_lasso_base_cross_stand
          df<-data.frame()
          for (idx in 1:length(lambdas_with_number_of_coeff)) {
            #idx=1
            
            
            single_lambdas_with_number_of_coeff<-lambdas_with_number_of_coeff[idx] #lambda for first fit with 5 coef 
            
            
            lambda.index <- which(fit_glmnet$lambda == single_lambdas_with_number_of_coeff) # index of lambda for first fit with 5 coef 
            mse_fit <- fit_glmnet$cvm[lambda.index]  # mse for that index
            
            coefss<-fit_glmnet$glmnet.fit$beta[,lambda.index] # all coef for that fit
            indexs<-fit_glmnet$glmnet.fit$beta[,lambda.index]!=0 # index for coef =! 0
            
            non_zero_coef<-coefss[indexs] # coef =! of the fit
            coef_names_non_zero<-paste_together_step_coefficients(names(non_zero_coef))
            
            
            
            lasso_df_for_coef_number<-coef_names_non_zero
            lasso_df_for_coef_number<- as.data.frame(lasso_df_for_coef_number)
            lasso_df_for_coef_number<- cbind(lasso_df_for_coef_number,mse_fit, lambda.index,  single_lambdas_with_number_of_coeff)
            
            df<-rbind(df, lasso_df_for_coef_number)
            
          }
          return(df)
        }
      }
      {
        get_occurences_glmnet_feature_selection<-function(df_x_coef,unique_glmnet_formulas_coef_x){
          #df_x_coef<-lasso_df_1_coef
          #unique_glmnet_formulas_coef_x<- unique_lasso_formulas_coef_1
          if (length(unique_glmnet_formulas_coef_x)==0) {
            u<- data.frame()
          }else{
            for (idx in 1: length(unique_glmnet_formulas_coef_x)) {
              new_row_glmnet_df_x_coef<-get_new_iteration_row(df_x_coef,unique_glmnet_formulas_coef_x[idx])
              df_x_coef<-rbind(df_x_coef,new_row_glmnet_df_x_coef)
            }
            u<-df_x_coef[-1,] %>% select_if(~ !is.numeric(.) || sum(.) != 0)
          }
          
          return(u)
        }
      }
      #lasso
      fit_lasso_base <- glmnet(train_sparse,data_train$log_e.coli,type.measure="mse", alpha=1, family="gaussian",relax = F)#--> alpha =1:  lasso regressio)
      
      
      
      
      
      #s_lasso_fits_5_coef<-fit_lasso_base_cross_stand$nzero==5
      #lambdas_lasso_fits_5_coef<-fit_lasso_base_cross_stand$lambda[s_lasso_fits_5_coef]
      
      #search formulas for 2 coef
      lasso_df_coeficcient_lambdas <- data.frame()
      for (s in 1:100) {
        #s<-20
        coefficient_names<-names(fit_lasso_base$beta[,s][fit_lasso_base$beta[,s]!=0 ])
        coefficient_names_pasted<-paste(coefficient_names,collapse=" + ")
        lasso_df_coeficcient_lambdas[s,1]<- s
        lasso_df_coeficcient_lambdas[s,2]<- coefficient_names_pasted
        lasso_df_coeficcient_lambdas[s,3]<- length(coefficient_names)
        
      }
      names(lasso_df_coeficcient_lambdas)[1]<- "lambda_idx"
      names(lasso_df_coeficcient_lambdas)[2]<- "formula"
      names(lasso_df_coeficcient_lambdas)[3]<- "n_features"
      
      
      
      
      
      #unique_lasso_formulas_coef_1<-
      unique_lasso_formulas_coef_1<-unique(lasso_df_coeficcient_lambdas$formula[lasso_df_coeficcient_lambdas$n_features==1])
      unique_lasso_formulas_coef_2<-unique(lasso_df_coeficcient_lambdas$formula[lasso_df_coeficcient_lambdas$n_features==2])
      unique_lasso_formulas_coef_3<-unique(lasso_df_coeficcient_lambdas$formula[lasso_df_coeficcient_lambdas$n_features==3])
      unique_lasso_formulas_coef_4<-unique(lasso_df_coeficcient_lambdas$formula[lasso_df_coeficcient_lambdas$n_features==4])
      unique_lasso_formulas_coef_5<-unique(lasso_df_coeficcient_lambdas$formula[lasso_df_coeficcient_lambdas$n_features==5])
      
      
      #build occurance df
      lasso_df_1_coef_save<-get_occurences_glmnet_feature_selection(lasso_df_1_coef, unique_lasso_formulas_coef_1)
      lasso_df_2_coef_save<-get_occurences_glmnet_feature_selection(lasso_df_2_coef, unique_lasso_formulas_coef_2)
      lasso_df_3_coef_save<-get_occurences_glmnet_feature_selection(lasso_df_3_coef, unique_lasso_formulas_coef_3)
      lasso_df_4_coef_save<-get_occurences_glmnet_feature_selection(lasso_df_4_coef, unique_lasso_formulas_coef_4)
      lasso_df_5_coef_save<-get_occurences_glmnet_feature_selection(lasso_df_5_coef, unique_lasso_formulas_coef_5)
      
      unique_lasso_formulas<-unique(unlist(list(lasso_df_1_coef_save$formel,lasso_df_2_coef_save$formel,lasso_df_3_coef_save$formel,lasso_df_4_coef_save$formel,lasso_df_5_coef_save$formel)))
      
      list_unique_lasso_formulas <- unique(unlist(append(list_unique_lasso_formulas, list(unique_lasso_formulas))))
      
      #end lasso
      
      #elnet
      fit_elnet_base <- glmnet(train_sparse,data_train$log_e.coli,type.measure="mse", alpha=0.5, family="gaussian",relax = F)#--> alpha =1:  lasso regressio)
      
      
      
      
      
      #s_elnet_fits_5_coef<-fit_elnet_base_cross_stand$nzero==5
      #lambdas_elnet_fits_5_coef<-fit_elnet_base_cross_stand$lambda[s_elnet_fits_5_coef]
      
      #search formulas for 2 coef
      elnet_df_coeficcient_lambdas <- data.frame()
      for (s in 1:100) {
        #s<-20
        coefficient_names<-names(fit_elnet_base$beta[,s][fit_elnet_base$beta[,s]!=0 ])
        coefficient_names_pasted<-paste(coefficient_names,collapse=" + ")
        elnet_df_coeficcient_lambdas[s,1]<- s
        elnet_df_coeficcient_lambdas[s,2]<- coefficient_names_pasted
        elnet_df_coeficcient_lambdas[s,3]<- length(coefficient_names)
        
      }
      names(elnet_df_coeficcient_lambdas)[1]<- "lambda_idx"
      names(elnet_df_coeficcient_lambdas)[2]<- "formula"
      names(elnet_df_coeficcient_lambdas)[3]<- "n_features"
      
      
      
      
      
      #unique_elnet_formulas_coef_1<-
      unique_elnet_formulas_coef_1<-unique(elnet_df_coeficcient_lambdas$formula[elnet_df_coeficcient_lambdas$n_features==1])
      unique_elnet_formulas_coef_2<-unique(elnet_df_coeficcient_lambdas$formula[elnet_df_coeficcient_lambdas$n_features==2])
      unique_elnet_formulas_coef_3<-unique(elnet_df_coeficcient_lambdas$formula[elnet_df_coeficcient_lambdas$n_features==3])
      unique_elnet_formulas_coef_4<-unique(elnet_df_coeficcient_lambdas$formula[elnet_df_coeficcient_lambdas$n_features==4])
      unique_elnet_formulas_coef_5<-unique(elnet_df_coeficcient_lambdas$formula[elnet_df_coeficcient_lambdas$n_features==5])
      
      
      #build occurance df
      elnet_df_1_coef_save<-get_occurences_glmnet_feature_selection(elnet_df_1_coef, unique_elnet_formulas_coef_1)
      elnet_df_2_coef_save<-get_occurences_glmnet_feature_selection(elnet_df_2_coef, unique_elnet_formulas_coef_2)
      elnet_df_3_coef_save<-get_occurences_glmnet_feature_selection(elnet_df_3_coef, unique_elnet_formulas_coef_3)
      elnet_df_4_coef_save<-get_occurences_glmnet_feature_selection(elnet_df_4_coef, unique_elnet_formulas_coef_4)
      elnet_df_5_coef_save<-get_occurences_glmnet_feature_selection(elnet_df_5_coef, unique_elnet_formulas_coef_5)
      
      unique_elnet_formulas<-unique(unlist(list(elnet_df_1_coef_save$formel,elnet_df_2_coef_save$formel,elnet_df_3_coef_save$formel,elnet_df_4_coef_save$formel,elnet_df_5_coef_save$formel)))
      
      list_unique_elnet_formulas <- unique(unlist(append(list_unique_elnet_formulas, list(unique_elnet_formulas))))
      #end elnet
      
      
      #end of selection --> now get fold_mse
      
      
      
    }
    #save all formulas from acual river
    river_list_unique_step_formulas<-append(river_list_unique_step_formulas,list(list_unique_step_formulas))
    river_list_unique_lasso_formulas<-append(river_list_unique_lasso_formulas,list(list_unique_lasso_formulas))
    river_list_unique_rf_formulas<-append(river_list_unique_rf_formulas,list(list_unique_rf_formulas))
    river_list_unique_elnet_formulas<-append(river_list_unique_elnet_formulas,list(list_unique_elnet_formulas))

    #all unique formulas
    all_unique_selected_formulas <- unique(unlist(list(list_unique_step_formulas,list_unique_rf_formulas,list_unique_lasso_formulas, list_unique_elnet_formulas)))
    
    unique_step_formulas <-  unique(unlist(list_unique_step_formulas))
    unique_rf_formulas <-  unique(unlist(list_unique_rf_formulas))
    unique_elnet_formulas <-  unique(unlist(list_unique_elnet_formulas))
    unique_lasso_formulas <-  unique(unlist(list_unique_lasso_formulas))
    
   
    
   
    
  
      {
        prediction_and_mse<- function(model1, test1, test_bacteria1){
          #model1<-rf_model_1
          #rf_formula_1
          
          #step_model_1
          #test_bacteria1 = test_bacteria
          #test1 = test
          #test_bacteria = test_bacteria
          
          prediction<- predict(object = model1, newdata = test1)
          mse <-mean(sqrt((test_bacteria1 - prediction)^2))
          return(mse)
        }
      }
      
      #unique formulas with 1-5 coefficients
      #not functional right now
      list_unqiue_formulas_all_algorithms_coef_1 <-unique(unlist(list(rf_df_1_coef_save$formel ,step_df_1_coef_save$formel,lasso_df_1_coef_save$formel,elnet_df_1_coef_save$formel )))
      list_unqiue_formulas_all_algorithms_coef_2 <-unique(unlist(list(rf_df_2_coef_save$formel ,step_df_2_coef_save$formel,lasso_df_2_coef_save$formel,elnet_df_2_coef_save$formel )))
      list_unqiue_formulas_all_algorithms_coef_3 <-unique(unlist(list(rf_df_3_coef_save$formel ,step_df_3_coef_save$formel,lasso_df_3_coef_save$formel,elnet_df_3_coef_save$formel )))
      list_unqiue_formulas_all_algorithms_coef_4 <-unique(unlist(list(rf_df_4_coef_save$formel ,step_df_4_coef_save$formel,lasso_df_4_coef_save$formel,elnet_df_4_coef_save$formel )))
      list_unqiue_formulas_all_algorithms_coef_5 <-unique(unlist(list(rf_df_5_coef_save$formel ,step_df_5_coef_save$formel,lasso_df_5_coef_save$formel,elnet_df_5_coef_save$formel )))
      
      {
        calc_mse_for_unique_formulas_with_test_set<-function(list_unqiue_formulas_all_algorithms){
          
          #list_unqiue_formulas_all_algorithms<-list_unqiue_formulas_all_algorithms_coef_1
          mse<-data.frame()
          R2<-data.frame()
          idx<- 1
          for (entry in list_unqiue_formulas_all_algorithms) {
            #entry<-list_unqiue_formulas_all_algorithms[1]
            formel<-paste("log_e.coli ~ ",entry)
            
            model<- lm(formel, data_train)
            R2_iteration = summary(model)[["adj.r.squared"]]
            R2[idx,1] <-R2_iteration
            
            mse_iteration<-prediction_and_mse(model1 = model, test_bacteria1 = data_test$log_e.coli, test1 = data_test)
            mse[idx,1] <-mse_iteration
            idx<- idx+1
            
          }
          #order_index<-order(mse$V1)[1]
          df<-data.frame(list_unqiue_formulas_all_algorithms , mse, R2)
          #order_index<-order(mse$V1)[1]
          #df<-data.frame(list_unqiue_formulas_all_algorithms[order_index] , mse[order_index,])
          
          names(df)[1]<- "formula_with_lowest_mse_on_test"
          names(df)[2]<- "mse"
          names(df)[3]<- "adj.r.squared"
          return(df)
          
        }
      }
      list_df_mse_adj_r_squared_all_algorithms <-list()
      
      #new for loop for mse calculation over all training errors
      for (indx_fold in 1:length(train_rows)) {
        data_train <- data[train_rows[[indx_fold]],]
        data_train <-data.frame(scale(data_train))
        #test_data fold
        data_test <- data[-train_rows[[indx_fold]],]
        data_test <-data.frame(scale(data_test))
        
        train_bacteria <- data_train$log_e.coli
        test_bacteria <- data_test$log_e.coli
        
      
        df_mse_adj_r_squared_all_algorithms<-calc_mse_for_unique_formulas_with_test_set(all_unique_selected_formulas)
        list_df_mse_adj_r_squared_all_algorithms <- append(list_df_mse_adj_r_squared_all_algorithms,list(df_mse_adj_r_squared_all_algorithms))
        
      }
      
      #do mse
      mse_adj_r_2_fold_1<-as.data.frame(list_df_mse_adj_r_squared_all_algorithms[[1]])
      mse_adj_r_2_fold_2<-as.data.frame(list_df_mse_adj_r_squared_all_algorithms[[2]])
      mse_adj_r_2_fold_3<-as.data.frame(list_df_mse_adj_r_squared_all_algorithms[[3]])
      mse_adj_r_2_fold_4<-as.data.frame(list_df_mse_adj_r_squared_all_algorithms[[4]])
      mse_adj_r_2_fold_5<-as.data.frame(list_df_mse_adj_r_squared_all_algorithms[[5]])
      
      df_all_algorithms_with_mse_on_test<- list_df_mse_adj_r_squared_all_algorithms[[1]]
      
      df_all_algorithms_with_mse_on_test$mse <- rowMeans(cbind(mse_adj_r_2_fold_1$mse,mse_adj_r_2_fold_2$mse,mse_adj_r_2_fold_3$mse,mse_adj_r_2_fold_4$mse,mse_adj_r_2_fold_5$mse))
      df_all_algorithms_with_mse_on_test$adj.r.squared <- rowMeans(cbind(mse_adj_r_2_fold_1$adj.r.squared,mse_adj_r_2_fold_2$adj.r.squared,mse_adj_r_2_fold_3$adj.r.squared,mse_adj_r_2_fold_4$adj.r.squared,mse_adj_r_2_fold_5$adj.r.squared))
      
      
      formula_mse_on_test_coef_1<-calc_mse_for_unique_formulas_with_test_set(list_unqiue_formulas_all_algorithms_coef_1)
      formula_mse_on_test_coef_2<-calc_mse_for_unique_formulas_with_test_set(list_unqiue_formulas_all_algorithms_coef_2)
      formula_mse_on_test_coef_3<-calc_mse_for_unique_formulas_with_test_set(list_unqiue_formulas_all_algorithms_coef_3)
      formula_mse_on_test_coef_4<-calc_mse_for_unique_formulas_with_test_set(list_unqiue_formulas_all_algorithms_coef_4)
      formula_mse_on_test_coef_5<-calc_mse_for_unique_formulas_with_test_set(list_unqiue_formulas_all_algorithms_coef_5)
      
      
      
      #df_all_algorithms_with_mse_on_test<-rbind(formula_mse_on_test_coef_1,formula_mse_on_test_coef_2,formula_mse_on_test_coef_3,formula_mse_on_test_coef_4,formula_mse_on_test_coef_5)%>% arrange(mse)
      
      
      
      
      df_all_algorithms_with_mse_on_test$step <-F
      df_all_algorithms_with_mse_on_test$rf <-F
      df_all_algorithms_with_mse_on_test$lasso <-F
      df_all_algorithms_with_mse_on_test$elnet <-F
      #df_all_algorithms_with_mse_on_test$indx_fold<- indx_fold
      
      
      
      #df_all_algorithms_with_mse_on_test$formula_with_lowest_mse_on_test[1] == unique_rf_formulas[7]
      
      
      ##check if rf _found_formula  
      for (jdx in 1:length(unique_rf_formulas)) {
        for (idx in 1:nrow(df_all_algorithms_with_mse_on_test)) {
          #idx<-1
          if (df_all_algorithms_with_mse_on_test$formula_with_lowest_mse_on_test[idx]== unique_rf_formulas[jdx]) {
            df_all_algorithms_with_mse_on_test$rf[idx]<- T
            
            
          }
          
          
        }
        
      }  
      #check if lasso _found_formula  
      for (jdx in 1:length(unique_lasso_formulas)) {
        for (idx in 1:nrow(df_all_algorithms_with_mse_on_test)) {
          #idx<-1
          if (df_all_algorithms_with_mse_on_test$formula_with_lowest_mse_on_test[idx]== unique_lasso_formulas[jdx]) {
            df_all_algorithms_with_mse_on_test$lasso[idx]<- T
            
            
          }
          
          
        }
        
      }   
      #check if step _found_formula  
      for (jdx in 1:length(unique_step_formulas)) {
        for (idx in 1:nrow(df_all_algorithms_with_mse_on_test)) {
          #idx<-1
          if (df_all_algorithms_with_mse_on_test$formula_with_lowest_mse_on_test[idx]== unique_step_formulas[jdx]) {
            df_all_algorithms_with_mse_on_test$step[idx]<- T
            
            
          }
          
          
        }
        
      }   
      #check if elnet_found_formula
      for (jdx in 1:length(unique_elnet_formulas)) {
        #jdx<-1
        for (idx in 1:nrow(df_all_algorithms_with_mse_on_test)) {
          #idx<-5
          if (df_all_algorithms_with_mse_on_test$formula_with_lowest_mse_on_test[idx]== unique_elnet_formulas[jdx]) {
            df_all_algorithms_with_mse_on_test$elnet[idx]<- T
          }
        }
      }
      
      
      
      
      
      
      #letzte klammer!
      df_all_algorithms_with_mse_on_test<-df_all_algorithms_with_mse_on_test%>%arrange(desc(adj.r.squared))
      #save river
      river_list_df_all_algorithms_with_mse_on_test<-append(river_list_df_all_algorithms_with_mse_on_test, list(df_all_algorithms_with_mse_on_test))
      
      #list_unique_step_formulas <- append(list_unique_step_formulas,unique_step_formulas)
      #list_unique_rf_formulas <- append(list_unique_rf_formulas,unique_rf_formulas)
      #list_unique_lasso_formulas <- append(list_unique_lasso_formulas,unique_lasso_formulas)
      #list_unique_elnet_formulas <- append(list_unique_elnet_formulas,unique_elnet_formulas)
     
      
      
    
#end of river_path loop
    
    
    
    #analysis featureselection
    list_unique_step_formulas<- unique(unlist(list_unique_step_formulas))
    list_unique_lasso_formulas<- unique(unlist(list_unique_lasso_formulas))
    list_unique_elnet_formulas<- unique(unlist(list_unique_elnet_formulas))
    list_unique_rf_formulas<- unique(unlist(list_unique_rf_formulas))
    
    calc_col_means <- function(df_x_coef_save){
      #df_x_coef_save<-rf_features_occurence
      new_df <- as.data.frame(colMeans(df_x_coef_save[-c((ncol(df_x_coef_save)-1):ncol(df_x_coef_save))]))
      new_df <- new_df %>% t()
      new_df<-as.data.frame(new_df)
      for (indx in 1:(ncol(df_x_coef_save)-2)) {
        #indx<-6
        df_x_coef_save[df_x_coef_save[indx]!=0,indx]<- new_df[indx]
      }
      #df_x_coef_save$ka_mean_mean_123[df_x_coef_save$ka_mean_mean_123!=0]<- new_df$ka_mean_mean_123
      
      
      
      #new_df <- cbind(new_df,colMeans(df_x_coef_save$mse))
      #colnames(new_df)[ncol(new_df)] <- "MSE"
      
      #new_df <- cbind(new_df,mean(df_x_coef_save$n_features))
      #colnames(new_df)[ncol(new_df)] <- "n_features"
      
      
      #new_rowname<-deparse(substitute(df_x_coef_save))
      #rownames(new_df)<-deparse(substitute(df_x_coef_save))
      #new_df<- as.data.frame(new_df)
      return(df_x_coef_save)
    }
    
    step_features_occurence<-calc_col_means(get_occurences_glmnet_feature_selection(step_features_occurence, list_unique_step_formulas))
    rf_features_occurence<-calc_col_means(get_occurences_glmnet_feature_selection(rf_features_occurence, list_unique_rf_formulas))
    lasso_features_occurence<-calc_col_means(get_occurences_glmnet_feature_selection(lasso_features_occurence, list_unique_lasso_formulas))
    elnet_features_occurence<-calc_col_means(get_occurences_glmnet_feature_selection(elnet_features_occurence, list_unique_elnet_formulas))
    
    
    river_list_rf_feature_occurence<- append(river_list_rf_feature_occurence,list(rf_features_occurence))
    river_list_step_feature_occurence<- append(river_list_step_feature_occurence,list(step_features_occurence))
    river_list_lasso_feature_occurence<- append(river_list_lasso_feature_occurence,list(lasso_features_occurence))
    river_list_elnet_feature_occurence<- append(river_list_elnet_feature_occurence,list(elnet_features_occurence))
    
    
    
    
    unique_found_formulas<-all_unique_selected_formulas
    
    #end of frequentistic mse

    #mcmc
    do_mcmc<-F
    if (do_mcmc==T) {
      list_unqiue_formulas_all_algorithms <-unique_found_formulas
      
      #list_unqiue_formulas_all_algorithms <- list_formulas_all_algorithms[[1]]
      fb<-list()
      idx<-1
      for(idx in 1:length(list_unqiue_formulas_all_algorithms)){
        fmla[[idx]]  <- paste("log_e.coli ~ ",list_unqiue_formulas_all_algorithms[idx])
        fb[[idx]] <- lm(fmla[[idx]], data = data_train) #because of cross validation for mcmc
      }
      
      ################ Validation ########################
      
      names(fb) <- sprintf(paste0(river,"model_%02d"), seq_along(1:length(fb)))
      
      # calculate statistical tests for residuals: Normality and s2 = const
      
      # shapiro-wilk test and breusch-pagan test
      
      get_stat_tests <- function(model) {
        c(N = shapiro.test(model$residuals)$p.value, lmtest::bptest(model)$p.value,
          R2 = summary(model)[["adj.r.squared"]], n_obs = length(model$residuals))
        
      }
      
      
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
      
      if(class(fmla[[length(fmla)]]) !="formula"){
        print("new element is no formula!!")
      }
      
      test_beta <- function(true, false, percentile){
        if( pbeta(q = percentile, shape1 = true + 1, shape2 = false + 1) > 0.05){
          TRUE}
        else{FALSE}
        
      }
      
      
      names(fmla) <- sprintf(paste0(river,"model_%02d"), seq_along(1:length(fb)))
      
      counter<-0
      
      erro_df <- data.frame()
      
      #cross validation   needs fb, and fmla
      
      { 
        for(i in names(fb)){
          #i<- names(fb)[1]
          counter<- counter+1
          #i="havelmodel_01"
          test_error_df_model <- data.frame()
          assign(paste(i,"_test_error_df_model",sep = ""),test_error_df_model)
          
          for(j in 1:5){
            counter <- counter+1
            # j=1
            
            data_train <- data[train_rows[[indx_fold]],]
            data_train <-data.frame(scale(data_train))
            #test_data fold
            data_test <- data[-train_rows[[indx_fold]],]
            data_test <-data.frame(scale(data_test))
            
            train_bacteria <- data_train$log_e.coli
            test_bacteria <- data_test$log_e.coli
            #training <- as.data.frame(fb[[i]]$model)[c(train_rows[[j]]),]
            #training <- as.data.frame(fb[[6]]$model)[c(train_rows[[1]]),]
            #test <- as.data.frame(fb[[i]]$model)[-c(train_rows[[j]]),]
            #test <- as.data.frame(fb[[6]]$model)[-c(train_rows[[1]]),]
            #formel<-formula(formula_heiko_1)
            
            
            #fmla[6]<- list(formel)
            
            
            
            fit <- rstanarm::stan_glm(fmla[[i]], data = data_train ) #fitting
            #fit <- rstanarm::stan_glm(fmla[[1]], data = training) #fitting
            
            
            df <- apply(rstanarm::posterior_predict(fit, newdata = data_test), 2, quantile, #predicting
                        
                        probs = c(0.025, 0.25, 0.5 , 0.75, 0.9, 0.95, 0.975)) %>% t() %>% as.data.frame() %>%
              
              dplyr::mutate(log_e.coli = data_test$log_e.coli, #evaluating ther model has to be classified correctly with every single test train split
                            #--> here 5 different splits, if all validations correct than everywhere ==5
                            
                            below95 = log_e.coli < `95%`,
                            
                            below90 = log_e.coli < `90%`,
                            
                            within95 = log_e.coli < `97.5%`& log_e.coli > `2.5%`,
                            
                            within50 = log_e.coli < `75%`& log_e.coli > `25%`,
                            
              ) 
            #error on testset
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
              
              
              #beta test
              test_beta(true = sum(df$within50), false = sum(!df$within50), .5)
            #add MSE to stat_tests
            river_stat_tests$MSE[river_stat_tests$model == i] <- mean(test_error_df_model$squared_error)
            
            
            #add selected features and coeeficients to statistical test
            river_stat_tests$selected_features[river_stat_tests$model == i] <- list((fmla[[i]]))
            #river_stat_tests$selected_features[river_stat_tests$model == i] <- list(all.vars(fmla[[i]]))
            river_stat_tests$coef_selected[river_stat_tests$model == i]<- list(coef(fb[[i]]))
            
            #list_river_stat_tests<- append(list_river_stat_tests, list(river_stat_tests))
            
          } 
          
        } 
      }
      
      
      
      
      
      
      
    }
    
    sorted_modellist <- river_stat_tests %>%
      filter( below95 == 5 & below90 == 5& in95 ) %>%
      dplyr::arrange(desc(in50), MSE)  
    
    
    
    #check if formula was found by algorithm
    sorted_modellist$lasso_formula <- F
    sorted_modellist$rf_formula <- F
    sorted_modellist$step_formula <- F
    sorted_modellist$elnet_formula <- F
    
    
    for (idx in 1: nrow(sorted_modellist)) {
      #idx<-3
      selected_features_posterior<-sorted_modellist[idx,]$selected_features[[1]][1]
      g<-str_split(selected_features_posterior,"~  ")
      l<-g[[1]][2]
      #l <- selected_features_posterior
      if(!is.na(match(l,unlist(list_unique_lasso_formulas)))){
        sorted_modellist$lasso_formula[idx] =T
      }else if(!is.na(match(l,unlist(list_unique_rf_formulas)))){
        sorted_modellist$rf_formula[idx] =T
      }else if(!is.na(match(l,unlist(list_unique_step_formulas)))){
        sorted_modellist$step_formula[idx] =T
      }else if(!is.na(match(l,unlist(list_unique_elnet_formulas)))){
        sorted_modellist$elnet_formula[idx] =T
      }
    }
    
    
    
    #analysis - frequentistic
    analysis_df <- data.frame(unique_found_formulas)
    analysis_df$mse <- 5
    
    #build amalysis table
    for (formula_indx in 1:nrow(analysis_df)) {
      #formula_indx<-1
      mse_list<-list()
      for(indx_fold in 1:length(river_list_df_all_algorithms_with_mse_on_test)){
        
        
        try(index_row_match<-match(analysis_df$unique_found_formulas[formula_indx],river_list_df_all_algorithms_with_mse_on_test[[indx_fold]]$formula_with_lowest_mse_on_test)) #gives back the indexrow where to find mse in list_df_all_alg..
        try(mse_list<-append(mse_list,river_list_df_all_algorithms_with_mse_on_test[[indx_fold]][index_row_match,2] ))#2 is for mse col
        
      }
      mse<-mean(unlist(mse_list), na.rm=T)
      analysis_df$found_n_times[formula_indx]<- sum(!is.na(mse_list))
      analysis_df$mse[formula_indx]<-mse
    }
    
    analysis_df<-analysis_df%>%arrange(desc(found_n_times),mse)
    
    river_list_river_stat_tests<- append(river_list_river_stat_tests, list(river_stat_tests))
    river_list_sorted_modellist <- append(river_list_sorted_modellist, list(sorted_modellist))
    river_list_analysis_df <- append(river_list_analysis_df, list(analysis_df))
    river_list_unique_found_formulas<-append(river_list_unique_found_formulas,list(unique_found_formulas))
    
   
    
  }
  

saved_river_list_river_stat_tests<- river_list_river_stat_tests
saved_river_list_sorted_modellist <- river_list_sorted_modellist
saved_river_list_analysis_df <- river_list_analysis_df 
saved_river_list_unique_found_formulas<-river_list_unique_found_formulas

saved_river_list_elnet_feature_occurence<-river_list_elnet_feature_occurence
saved_river_list_rf_feature_occurence<-river_list_rf_feature_occurence
saved_river_list_step_feature_occurence<-river_list_step_feature_occurence
saved_river_list_lasso_feature_occurence<-river_list_lasso_feature_occurence

saved_river_list_unique_found_formulas<-river_list_unique_found_formulas

saved_river_list_rf_feature_occurence<-  river_list_rf_feature_occurence
saved_river_list_step_feature_occurence<- river_list_step_feature_occurence
saved_river_list_lasso_feature_occurence<- river_list_lasso_feature_occurence
saved_river_list_elnet_feature_occurence<- river_list_elnet_feature_occurence
saved_river_list_df_all_algorithms_with_mse_on_test<-river_list_df_all_algorithms_with_mse_on_test

#saved_have_data
saveRDS(saved_river_list_river_stat_tests[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_river_stat_tests.rds")
saveRDS(saved_river_list_sorted_modellist[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_sorted_modellist.rds")
saveRDS(saved_river_list_analysis_df[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_analysis_df.rds")

saveRDS(saved_river_list_elnet_feature_occurence[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_elnet_feature_occurence.rds")
saveRDS(saved_river_list_rf_feature_occurence[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_rf_feature_occurence.rds")
saveRDS(saved_river_list_step_feature_occurence[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_step_feature_occurence.rds")
saveRDS(saved_river_list_lasso_feature_occurence[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_lasso_feature_occurence.rds")
saveRDS(saved_river_list_unique_found_formulas[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_unique_found_formulas.rds")
saveRDS(saved_river_list_df_all_algorithms_with_mse_on_test[[1]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/havel_results_1/havel_saved_river_list_df_all_algorithms_with_mse_on_test.rds")

#saved_isar_data
saveRDS(saved_river_list_river_stat_tests[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_river_stat_tests.rds")
saveRDS(saved_river_list_sorted_modellist[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_sorted_modellist.rds")
saveRDS(saved_river_list_analysis_df[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_analysis_df.rds")

saveRDS(saved_river_list_elnet_feature_occurence[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_elnet_feature_occurence.rds")
saveRDS(saved_river_list_rf_feature_occurence[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_rf_feature_occurence.rds")
saveRDS(saved_river_list_step_feature_occurence[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_step_feature_occurence.rds")
saveRDS(saved_river_list_lasso_feature_occurence[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_lasso_feature_occurence.rds")
saveRDS(saved_river_list_unique_found_formulas[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_unique_found_formulas.rds")
saveRDS(saved_river_list_df_all_algorithms_with_mse_on_test[[2]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/isar_results_1/isar_saved_river_list_df_all_algorithms_with_mse_on_test.rds")

#saved_ilz_data
saveRDS(saved_river_list_river_stat_tests[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_river_stat_tests.rds")
saveRDS(saved_river_list_sorted_modellist[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_sorted_modellist.rds")
saveRDS(saved_river_list_analysis_df[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_analysis_df.rds")

saveRDS(saved_river_list_elnet_feature_occurence[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_elnet_feature_occurence.rds")
saveRDS(saved_river_list_rf_feature_occurence[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_rf_feature_occurence.rds")
saveRDS(saved_river_list_step_feature_occurence[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_step_feature_occurence.rds")
saveRDS(saved_river_list_lasso_feature_occurence[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_lasso_feature_occurence.rds")
saveRDS(saved_river_list_unique_found_formulas[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_unique_found_formulas.rds")
saveRDS(saved_river_list_df_all_algorithms_with_mse_on_test[[3]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ilz_results_1/ilz_saved_river_list_df_all_algorithms_with_mse_on_test.rds")

#saved_mosel_data
saveRDS(saved_river_list_river_stat_tests[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_river_stat_tests.rds")
saveRDS(saved_river_list_sorted_modellist[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_sorted_modellist.rds")
saveRDS(saved_river_list_analysis_df[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_analysis_df.rds")

saveRDS(saved_river_list_elnet_feature_occurence[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_elnet_feature_occurence.rds")
saveRDS(saved_river_list_rf_feature_occurence[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_rf_feature_occurence.rds")
saveRDS(saved_river_list_step_feature_occurence[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_step_feature_occurence.rds")
saveRDS(saved_river_list_lasso_feature_occurence[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_lasso_feature_occurence.rds")
saveRDS(saved_river_list_unique_found_formulas[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_unique_found_formulas.rds")
saveRDS(saved_river_list_df_all_algorithms_with_mse_on_test[[4]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/mosel_results_1/mosel_saved_river_list_df_all_algorithms_with_mse_on_test.rds")

#saved_rhein_data
saveRDS(saved_river_list_river_stat_tests[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_river_stat_tests.rds")
saveRDS(saved_river_list_sorted_modellist[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_sorted_modellist.rds")
saveRDS(saved_river_list_analysis_df[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_analysis_df.rds")

saveRDS(saved_river_list_elnet_feature_occurence[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_elnet_feature_occurence.rds")
saveRDS(saved_river_list_rf_feature_occurence[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_rf_feature_occurence.rds")
saveRDS(saved_river_list_step_feature_occurence[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_step_feature_occurence.rds")
saveRDS(saved_river_list_lasso_feature_occurence[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_lasso_feature_occurence.rds")
saveRDS(saved_river_list_unique_found_formulas[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_unique_found_formulas.rds")
saveRDS(saved_river_list_df_all_algorithms_with_mse_on_test[[5]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/rhein_results_1/rhein_saved_river_list_df_all_algorithms_with_mse_on_test.rds")

#saved_ruhr_data
saveRDS(saved_river_list_river_stat_tests[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_river_stat_tests.rds")
saveRDS(saved_river_list_sorted_modellist[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_sorted_modellist.rds")
saveRDS(saved_river_list_analysis_df[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_analysis_df.rds")

saveRDS(saved_river_list_elnet_feature_occurence[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_elnet_feature_occurence.rds")
saveRDS(saved_river_list_rf_feature_occurence[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_rf_feature_occurence.rds")
saveRDS(saved_river_list_step_feature_occurence[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_step_feature_occurence.rds")
saveRDS(saved_river_list_lasso_feature_occurence[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_lasso_feature_occurence.rds")
saveRDS(saved_river_list_unique_found_formulas[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_unique_found_formulas.rds")
saveRDS(saved_river_list_df_all_algorithms_with_mse_on_test[[6]], file = "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/git_bathing_water_quality/ruhr_results_1/ruhr_saved_river_list_df_all_algorithms_with_mse_on_test.rds")






havel_saved_river_list_sorted_modellist <- write_csv( river_list_sorted_modellist[[1]])
havel_saved_river_list_analysis_df <- river_list_analysis_df [[1]]
havel_saved_river_list_unique_found_formulas<-river_list_unique_found_formulas[[1]]

isar_results <-
ilz_results <-
mosel_results <-
rhein_results <-
ruhr_results <-




havel_sorted_modellist <- as_tibble(river_list_analysis_df[1:18])
isar_sorted_modellist  <- as_tibble(river_list_analysis_df[19:36])
ila_sorted_modellist <- as_tibble(river_list_analysis_df[37:54])
mosel_sorted_modellist <- as_tibble(river_list_analysis_df[55:72] )
rhein_sorted_modellist <- as_tibble(river_list_analysis_df[73:90])
rhur_sorted_modellist <- as_tibble(river_list_analysis_df[91:108])




#mcmc analysis

#select only models that are validated - sorted by desc R2 - but is this a good kpi??





list_river_stat_tests[[1]]
fold_1_mcmc<-list_river_stat_tests[[13]]$selected_features
fold_2_mcmc<-list_river_stat_tests[[29]]$selected_features
fold_3_mcmc<-list_river_stat_tests[[44]]$selected_features
fold_4_mcmc<-list_river_stat_tests[[59]]$selected_features
fold_5_mcmc<-list_river_stat_tests[[73]]$selected_features

Reduce(intersect, list(fold_1_mcmc,fold_2_mcmc,fold_3_mcmc,fold_4_mcmc,fold_5_mcmc))

inner_join(fold_1_mcmc,fold_2_mcmc)


comparison <- compare(fold_1_mcmc$selected_features[[5]], fold_2_mcmc$selected_features, allowAll =    T)
comparison
fold_1_mcmc$selected_features[[3]]



