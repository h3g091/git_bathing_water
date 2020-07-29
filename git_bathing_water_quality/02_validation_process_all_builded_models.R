

{
  get_all_formulas_over_all_iterations_per_river_step <- function(saved_iteration_river_list_selection_aic_temp, river_number){
    #saved_iteration_river_list_selection_aic_temp<-saved_iteration_river_list_selection_aic
    #river_number <- 1 #havel
    list_river_models_all_iterations_step <- list()
    #list_river_models_all_iterations_lambda_1se <- list()  
    for(iteration_idx in 1:15){
      #iteration_idx<-1
      models_in_iteration_i<-saved_iteration_river_list_selection_aic_temp[[iteration_idx]][[river_number]]  
      formulas_river_iteration<-get_step_formulas_from_models(models_in_iteration_i)
      
      
      
      list_river_models_all_iterations_step <- append(list_river_models_all_iterations_step,list(formulas_river_iteration))
      
    }
    return_obj <- list_river_models_all_iterations_step
    
    return(return_obj)
  }  
}

{
  get_step_formulas_from_models <- function(river_lasso_model_iteration){
    #river_step_model_iteration<-havel_step_models_iteration_1
    #river_step_model_iteration<-models_in_iteration_i
    coef(models_in_iteration_i[[1]])
    step_coef_iteration_1<-lapply(river_lasso_model_iteration, coef)
    list_formula_temp_step<-list()
    for(i in 1:5){
      #i<-1
      formula_temp_step<-paste("log_e.coli ~ ",paste(names(step_coef_iteration_1[[i]][-1]), collapse = " + "), sep = "")
      list_formula_temp_step <- append(list_formula_temp_step, list(formula_temp_step))
    }
    
    found_formulas_step<-unlist(list_formula_temp_step)      
    if (any(found_formulas_step == "log_e.coli ~ ")) {
      index_to_reduce<-grep("^log_e.coli ~ $",found_formulas_step, )
      found_formulas_step<-found_formulas_step[-index_to_reduce]
    }
    
    return_obj<-list(found_formulas_step)
    return(return_obj)      
  }
}    



{
get_lasso_formulas_from_models <- function(river_lasso_model_iteration){
    #river_lasso_model_iteration<-havel_lasso_models_iteration_1
    #river_lasso_model_iteration<-models_in_iteration_i
      lasso_lambda_min_coef_iteration_1<-lapply(river_lasso_model_iteration, extract.coef, lambda = "lambda.min")
      lasso_lambda_1se_coef_iteration_1<-lapply(river_lasso_model_iteration, extract.coef, lambda = "lambda.1se")
      list_formula_temp_lambda_1se<-list()
      list_formula_temp_lambda_min<-list()
      for(i in 1:5){
        #i<-1
        formula_temp_lambda_1se<-paste("log_e.coli ~ ",paste(lasso_lambda_1se_coef_iteration_1[[i]]$Coefficient[-1], collapse = " + "), sep = "")
        list_formula_temp_lambda_1se <- append(list_formula_temp_lambda_1se, list(formula_temp_lambda_1se))
        formula_temp_lambda_min<-paste("log_e.coli ~ ",paste(lasso_lambda_min_coef_iteration_1[[i]]$Coefficient[-1], collapse = " + "), sep = "")
        list_formula_temp_lambda_min <- append(list_formula_temp_lambda_min, list(formula_temp_lambda_min))
      }
      found_formulas_lambda_1se<-unlist(list_formula_temp_lambda_1se)
      found_formulas_lambda_min<-unlist(list_formula_temp_lambda_min)      
      if (any(found_formulas_lambda_min == "log_e.coli ~ ")) {
        index_to_reduce<-grep("^log_e.coli ~ $",found_formulas_lambda_min, )
        found_formulas_lambda_min<-found_formulas_lambda_min[-index_to_reduce]
      }
      
      if (any(found_formulas_lambda_1se == "log_e.coli ~ ")) {
        index_to_reduce<-grep("^log_e.coli ~ $",found_formulas_lambda_1se, )
        found_formulas_lambda_1se<-found_formulas_lambda_1se[-index_to_reduce]
      }
      
      return_obj<-list(found_formulas_lambda_min, found_formulas_lambda_1se)
  return(return_obj)      
}
}    
{
get_all_formulas_over_all_iterations_per_river_lasso <- function(saved_iteration_river_list_lasso_Model_temp, river_number){
  #saved_iteration_river_list_lasso_Model_temp<-saved_iteration_river_list_lasso_Model
  #river_number <- 1 #havel
    list_river_models_all_iterations_lambda_min <- list()
    list_river_models_all_iterations_lambda_1se <- list()  
    for(iteration_idx in 1:15){
      #iteration_idx<-1
      models_in_iteration_i<-saved_iteration_river_list_lasso_Model_temp[[iteration_idx]][[river_number]]  
      formulas_river_iteration<-get_lasso_formulas_from_models(models_in_iteration_i)
      formulas_river_iteration_lasso_lambda_min<-formulas_river_iteration[[1]]
      formulas_river_iteration_lasso_lambda_1se<-formulas_river_iteration[[2]]
      
      list_river_models_all_iterations_lambda_min <- append(list_river_models_all_iterations_lambda_min,list(formulas_river_iteration_lasso_lambda_min))
      list_river_models_all_iterations_lambda_1se <- append(list_river_models_all_iterations_lambda_1se,list(formulas_river_iteration_lasso_lambda_1se))
    }
  return_obj <- list(list_river_models_all_iterations_lambda_min,list_river_models_all_iterations_lambda_1se)
  
  return(return_obj)
}  
}





havel_data_train_first_iteration<-saved_iteration_river_list_data_train[[1]][[1]]
havel_data_test_first_iteration<-saved_iteration_river_list_data_test[[1]][[1]]            

havel_step_aic_all_found_formulas<-get_all_formulas_over_all_iterations_per_river_step(saved_iteration_river_list_selection_aic,1)
havel_step_bic_all_found_formulas<-get_all_formulas_over_all_iterations_per_river_step(saved_iteration_river_list_selection_bic,1)
list_unique_step_aic_formulas<- unique(unlist(havel_step_aic_all_found_formulas))    
list_unique_step_bic_formulas<- unique(unlist(havel_step_bic_all_found_formulas))



havel_lasso_lambda_min_all_found_formulas<-get_all_formulas_over_all_iterations_per_river_lasso(saved_iteration_river_list_lasso_Model, 1)[[1]]
havel_lasso_lambda_1se_all_found_formulas<-get_all_formulas_over_all_iterations_per_river_lasso(saved_iteration_river_list_lasso_Model, 1)[[2]]
list_unique_lasso_lambda_min_formulas<- unique(unlist(havel_lasso_lambda_min_all_found_formulas))    
list_unique_lasso_lambda_1se_formulas<- unique(unlist(havel_lasso_lambda_1se_all_found_formulas))
      

      

      
{     
validation <- function(formel,data_train,data_test){      
    #data_train <- havel_data_train_first_iteration
    #data_test <- havel_data_test_first_iteration
  #formel<-list_unique_lasso_formulas[[1]]
  dummy_df <- data.frame(0,0,0,0,0,0,0,0,0)
  names(dummy_df) <- c("mean_MSE", "mean_R2", "n_obs", "mean_sd_pred_train_to_true","in95", "below95", "below90", "in50", "Formula")
  dummy_df$Formula <- formel
  
  fold_indx <- 1
  
  #river_stat_tests$in95 <- river_stat_tests$below95 <-river_stat_tests$below90 <- river_stat_tests$in50 <- river_stat_tests$MSE <- 0
  
  data_train_fold <- data_train[[fold_indx]]
  data_test_fold  <- data_test[[fold_indx]]
  
  train_bacteria  <- data_train_fold$log_e.coli
  test_bacteria <- data_test_fold$log_e.coli
  full_with_interaction <- formula(formel, data = data_train_fold)
  linear_model<- lm(formula = full_with_interaction, data = data_train_fold )
  list_R2_iteration <- summary(linear_model)[["adj.r.squared"]]
  
  #prediction
  pred_train <- predict(linear_model,newdata = data_train_fold)
  pred_test <- predict(linear_model, newdata =  data_test_fold)
  
  list_test_error <- mean((pred_test- data_test_fold$log_e.coli)^2)
  
  #get sd from difference between train_bacteria_true to the predicted! mean is the prediction
  sd_pred_train_to_true<-sqrt((sum((train_bacteria - pred_train)^ 2))/length(train_bacteria))
  list_sd_pred_train_to_true <- sd_pred_train_to_true
  #dummy_df$sd_pred_train_to_true <- sd_pred_train_to_true
  
  percentile_train_2_5<- qnorm(0.025, mean = pred_test, sd = sd_pred_train_to_true)
  percentile_train_25<- qnorm(0.25, mean = pred_test, sd = sd_pred_train_to_true)
  percentile_train_75<- qnorm(0.75, mean = pred_test, sd = sd_pred_train_to_true)
  percentile_train_90<- qnorm(0.90, mean = pred_test, sd = sd_pred_train_to_true)
  percentile_train_95<- qnorm(0.95, mean = pred_test, sd = sd_pred_train_to_true)
  percentile_train_97_5<- qnorm(0.975, mean = pred_test, sd = sd_pred_train_to_true)
  
  #evaluating ther model has to be classified correctly with every single test train split
  #--> here 5 different splits, if all validations correct than everywhere ==5
  
  below95 = test_bacteria < percentile_train_95
  below90 = test_bacteria < percentile_train_90
  within95 = test_bacteria < percentile_train_97_5& test_bacteria > perc_train_2_5
  within50 = test_bacteria < percentile_train_75& test_bacteria > perc_train_25
  
  dummy_df$in95 <- dummy_df$in95 + test_beta(true = sum(within95), false = sum(!within95), percentile = .95 )
  dummy_df$below95 <- dummy_df$below95+test_beta(true = sum(below95), false = sum(!below95), percentile = .95 )
  dummy_df$below90 <- dummy_df$below90+test_beta(true = sum(below90), false = sum(!below90), percentile = .90 )
  dummy_df$in50 <- dummy_df$in50+test_beta(true = sum(within50), false = sum(!within50), percentile = .5)
  
  for(fold_indx in 2:5){
      #first_fold
      #fold_indx <- 5
      data_train_fold <- data_train[[fold_indx]]
      data_test_fold  <- data_test[[fold_indx]]
      
      train_bacteria  <- data_train_fold$log_e.coli
      test_bacteria <- data_test_fold$log_e.coli
      full_with_interaction <- formula(formel, data = data_train_fold)
      linear_model<- lm(formula = full_with_interaction, data = data_train_fold )
      
      
      
      list_R2_iteration <-append(list_R2_iteration,list( summary(linear_model)[["adj.r.squared"]]))
      
      #prediction
      pred_train <- predict(linear_model,newdata = data_train_fold)
      pred_test <- predict(linear_model, newdata =  data_test_fold)
      
      #get sd from difference between train_bacteria_true to the predicted! mean is the prediction
      sd_pred_train_to_true<-sqrt((sum((train_bacteria - pred_train)^ 2))/length(train_bacteria))
      
      
      percentile_train_2_5<- qnorm(0.025, mean = pred_test, sd = sd_pred_train_to_true)
      percentile_train_25<- qnorm(0.25, mean = pred_test, sd = sd_pred_train_to_true)
      percentile_train_75<- qnorm(0.75, mean = pred_test, sd = sd_pred_train_to_true)
      percentile_train_90<- qnorm(0.90, mean = pred_test, sd = sd_pred_train_to_true)
      percentile_train_95<- qnorm(0.95, mean = pred_test, sd = sd_pred_train_to_true)
      percentile_train_97_5<- qnorm(0.975, mean = pred_test, sd = sd_pred_train_to_true)
      
     
      #evaluating ther model has to be classified correctly with every single test train split
                    #--> here 5 different splits, if all validations correct than everywhere ==5
      
        below95 = test_bacteria < percentile_train_95
        
        below90 = test_bacteria < percentile_train_90
        
        within95 = test_bacteria < percentile_train_97_5 & test_bacteria > percentile_train_2_5
        
        within50 = test_bacteria < percentile_train_75 & test_bacteria > percentile_train_25
        
        
        dummy_df$in95 <- dummy_df$in95 + test_beta(true = sum(within95), false = sum(!within95), percentile = .95 )
        dummy_df$below95 <- dummy_df$below95 + test_beta(true = sum(below95), false = sum(!below95), percentile = .95 )
        dummy_df$below90 <- dummy_df$below90 + test_beta(true = sum(below90), false = sum(!below90), percentile = .90 )
        dummy_df$in50 <- dummy_df$in50 + test_beta(true = sum(within50), false = sum(!within50), percentile = .5)
        
        list_test_error <- append(list_test_error,list(mean((pred_test- data_test_fold$log_e.coli)^2)))
        list_sd_pred_train_to_true <-append(list_sd_pred_train_to_true, list(sd_pred_train_to_true))        
      
  }
  
  dummy_df$n_obs<-nrow(data_train_fold)+nrow(data_test_fold)
  dummy_df$mean_MSE<-mean(unlist(list_test_error))
  dummy_df$mean_sd_pred_train_to_true<-mean(unlist(list_sd_pred_train_to_true))
  dummy_df$mean_R2 <-mean(unlist(list_R2_iteration))
  
  return(dummy_df)
}
}

{      
validation_for_list_of_found_formulas <- function(list_formulas, data_train, data_test){
  #list_formulas <- list_unique_lasso_lambda_1se_formulas
  #data_train <- havel_data_train_first_iteration
  #data_test <- havel_data_test_first_iteration
  validation_dataframe<-validation(formel = list_formulas[[1]],data_train,data_test)
  for(i in 2:length(list_formulas)){
    #i<-8
    validation_dataframe<-rbind(validation_dataframe,validation(formel = list_formulas[[i]],data_train,data_test))
  }
  
  return(validation_dataframe)
}      
}      

validation_dataframe_havel_lasso_lambda_min<-validation_for_list_of_found_formulas(list_unique_lasso_lambda_min_formulas, havel_data_train_first_iteration,havel_data_test_first_iteration)
validation_dataframe_havel_lasso_lambda_1se<-validation_for_list_of_found_formulas(list_unique_lasso_lambda_1se_formulas, havel_data_train_first_iteration,havel_data_test_first_iteration)

validation_dataframe_havel_step_aic<-validation_for_list_of_found_formulas(list_unique_step_aic_formulas, havel_data_train_first_iteration,havel_data_test_first_iteration)
validation_dataframe_havel_step_bic<-validation_for_list_of_found_formulas(list_unique_step_bic_formulas, havel_data_train_first_iteration,havel_data_test_first_iteration)

{
combine_validation_and_most_selected_dfs<- function(validation_df, most_selected_df){
  #most_selected_df<-most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min
  #validation_df<-validation_dataframe_havel_lasso_lambda_min
  most_selected_df$Formula<-paste("log_e.coli ~ ", as.character(most_selected_df$Formula),sep = "")
  check_up_df<- full_join(validation_df,most_selected_df, by="Formula")
  check_up_df_sorted <- check_up_df %>% arrange(desc(in50), mean_MSE)
  return(check_up_df_sorted)
}
}


combi_validation_selection_dataframe_havel_lasso_lambda_1se<-combine_validation_and_most_selected_dfs(validation_dataframe_havel_lasso_lambda_1se, most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se)
combi_validation_selection_dataframe_havel_lasso_lambda_min<-combine_validation_and_most_selected_dfs(validation_dataframe_havel_lasso_lambda_min, most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min)

combi_validation_selection_dataframe_havel_step_aic<-combine_validation_and_most_selected_dfs(validation_dataframe_havel_step_aic, most_selected_formulas_analysis_feature_selection_havel_step_aic)
combi_validation_selection_dataframe_havel_step_bic<-combine_validation_and_most_selected_dfs(validation_dataframe_havel_step_bic, most_selected_formulas_analysis_feature_selection_havel_step_bic)

view(combi_validation_selection_dataframe_havel_step_aic)
