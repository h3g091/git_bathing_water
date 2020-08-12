#change river_number to analyse different river
river_number <- 6

{     
  validation <- function(formel,data_train,data_test){      
    #data_train <- ruhr_data_train_first_iteration
    #data_test <- ruhr_data_test_first_iteration
    #formel<-list_unique_lasso_formulas[[1]]
    #formel<-er1)
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
    list_linear_models <<- append(list_linear_models, list(linear_model))
    #list_R2_iteration <- summary(linear_model)[["r.squared"]]
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
    within95 = test_bacteria < percentile_train_97_5& test_bacteria > percentile_train_2_5
    within50 = test_bacteria < percentile_train_75& test_bacteria > percentile_train_25
    
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
  validation_for_rf <- function(rfModel,data_train,data_test, rfModel_name){      
    #data_train <- ruhr_data_train_first_iteration
    #data_test <- ruhr_data_test_first_iteration
    #formel<-
    #rfModel<- ruhr_rf_all_builded_models[[1]]
    #rfModel_name<-"formula_rf"
    #rfModel<-rfModel_temp
    #rfModel_name<-rfModel_name
    
    
    dummy_df <- data.frame(0,0,0,0,0,0,0,0,0)
    names(dummy_df) <- c("mean_MSE", "mean_R2", "n_obs", "mean_sd_pred_train_to_true","in95", "below95", "below90", "in50", "Formula")
    dummy_df$Formula <- rfModel_name
    
    fold_indx <- 1
    
    #river_stat_tests$in95 <- river_stat_tests$below95 <-river_stat_tests$below90 <- river_stat_tests$in50 <- river_stat_tests$MSE <- 0
    
    data_train_fold <- data_train[[fold_indx]]
    data_test_fold  <- data_test[[fold_indx]]
    
    train_bacteria  <- data_train_fold$log_e.coli
    test_bacteria <- data_test_fold$log_e.coli
    full_no_interaction <- lm(log_e.coli ~ ., data = data_train_fold)
    full_no_interaction_test <- lm(log_e.coli ~ ., data = data_test_fold)
    train <- model.matrix(full_no_interaction, data_train)
    test <- model.matrix(full_no_interaction_test, data_test)
    
    
    
    #full_with_interaction <- formula(log_e.coli ~ .^2, data = data_train_fold)
    #linear_model<- lm(formula = full_with_interaction, data = data_train_fold )
    #list_R2_iteration <- summary(linear_model)[["adj.r.squared"]]
    
    #prediction
    
    pred_train <- predict(rfModel, newdata = train)
    pred_test <- predict(rfModel, newdata =  test)
    
    r2<- 1 - sum((data_train_fold$log_e.coli-pred_train)^2)/sum((data_train_fold$log_e.coli-mean(data_train_fold$log_e.coli))^2)
    list_R2_iteration<- r2
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
    within95 = test_bacteria < percentile_train_97_5& test_bacteria > percentile_train_2_5
    within50 = test_bacteria < percentile_train_75& test_bacteria > percentile_train_25
    
    dummy_df$in95 <- dummy_df$in95 + test_beta(true = sum(within95), false = sum(!within95), percentile = .95 )
    dummy_df$below95 <- dummy_df$below95+test_beta(true = sum(below95), false = sum(!below95), percentile = .95 )
    dummy_df$below90 <- dummy_df$below90+test_beta(true = sum(below90), false = sum(!below90), percentile = .90 )
    dummy_df$in50 <- dummy_df$in50+test_beta(true = sum(within50), false = sum(!within50), percentile = .5)
    
    for(fold_indx in 2:5){
      #first_fold
      #fold_indx <- 2
      data_train_fold <- data_train[[fold_indx]]
      data_test_fold  <- data_test[[fold_indx]]
      
      train_bacteria  <- data_train_fold$log_e.coli
      test_bacteria <- data_test_fold$log_e.coli
      full_no_interaction <- lm(log_e.coli ~ ., data = data_train_fold)
      full_no_interaction_test <- lm(log_e.coli ~ ., data = data_test_fold)
      train <- model.matrix(full_no_interaction, data_train)
      test <- model.matrix(full_no_interaction_test, data_test)
      
      
      
      #list_R2_iteration <-append(list_R2_iteration,list( summary(linear_model)[["adj.r.squared"]]))
      
      #prediction
      pred_train <- predict(linear_model,newdata = data_train_fold)
      pred_test <- predict(linear_model, newdata =  data_test_fold)
      
      r2<- 1 - sum((data_train_fold$log_e.coli-pred_train)^2)/sum((data_train_fold$log_e.coli-mean(data_train_fold$log_e.coli))^2)
      list_R2_iteration <-append(list_R2_iteration,list(r2 ))
      #for r2 1 - sum((y-predicted)^2)/sum((y-mean(y))^2)
      
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
    #data_train <- ruhr_data_train_first_iteration
    #data_test <- ruhr_data_test_first_iteration
    
    validation_dataframe<-validation(formel = list_formulas[[1]],data_train,data_test)
    for(i in 2:length(list_formulas)){
      #i<-8
      validation_dataframe<-rbind(validation_dataframe,validation(formel = list_formulas[[i]],data_train,data_test))
    }
    
    return(validation_dataframe)
  }      
}    
{      
  validation_for_list_of_found_formulas_for_rf <- function(list_models, data_train, data_test){
    #list_models <- ruhr_rf_all_builded_models
    #data_train <- ruhr_data_train_first_iteration
    #data_test <- ruhr_data_test_first_iteration
    
    
    i<-1
    rfModel_temp <- list_models[[i]]
    rfModel_name<- paste("rfModel", i, sep = "_")
    validation_dataframe<-validation_for_rf(rfModel = rfModel_temp,data_train,data_test,rfModel_name)
    for(i in 2:length(list_models)){
      #i<-2
      rfModel_temp <- list_models[[i]]
      rfModel_name<- paste("rfModel", i, sep = "_")
      validation_dataframe2<-validation_for_rf(rfModel = rfModel_temp,data_train,data_test,rfModel_name)
      validation_dataframe<-rbind(validation_dataframe,validation_dataframe2)
    }
    
    return(validation_dataframe)
  }      
}    


ruhr_data_train_first_iteration<-saved_iteration_river_list_data_train_validation[[1]][[river_number]]
ruhr_data_test_first_iteration<-saved_iteration_river_list_data_test_validation[[1]][[river_number]]
ruhr_rf_all_builded_models <- list_river_rf_all_builded_models[[river_number]]

list_unique_lasso_lambda_min_formulas <- list_all_rivers_unique_lasso_lambda_min_formulas[[river_number]]
list_unique_lasso_lambda_1se_formulas <- list_all_rivers_unique_lasso_lambda_1se_formulas[[river_number]]
list_unique_step_aic_formulas <- list_all_rivers_unique_step_aic_formulas[[river_number]]
list_unique_step_bic_formulas <- list_all_rivers_unique_step_bic_formulas[[river_number]]
list_unique_step_5_aic_formulas<- list_all_rivers_unique_step_5_aic_formulas[[river_number]]
list_unique_step_5_bic_formulas<- list_all_rivers_unique_step_5_bic_formulas[[river_number]]

most_selected_formulas_analysis_feature_selection_ruhr_lasso_lambda_1se <- list_most_selected_formulas_analysis_feature_selection_all_rivers_lasso_lambda_1se[[river_number]]
most_selected_formulas_analysis_feature_selection_ruhr_lasso_lambda_min<- list_most_selected_formulas_analysis_feature_selection_all_rivers_lasso_lambda_min[[river_number]]
most_selected_formulas_analysis_feature_selection_ruhr_step_aic<- list_most_selected_formulas_analysis_feature_selection_all_rivers_step_aic[[river_number]]
most_selected_formulas_analysis_feature_selection_ruhr_step_bic<- list_most_selected_formulas_analysis_feature_selection_all_rivers_step_aic[[river_number]]
most_selected_formulas_analysis_feature_selection_ruhr_step_5_aic<- list_most_selected_formulas_analysis_feature_selection_all_rivers_step_5_aic[[river_number]]
most_selected_formulas_analysis_feature_selection_ruhr_step_5_bic<- list_most_selected_formulas_analysis_feature_selection_all_rivers_step_5_bic[[river_number]]


#stopped_here




# do rf
validation_dataframe_ruhr_rf<-validation_for_list_of_found_formulas_for_rf(ruhr_rf_all_builded_models, ruhr_data_train_first_iteration,ruhr_data_test_first_iteration)

#do this to build same datframes
validation_dataframe_ruhr_rf$Freq <- 1
validation_dataframe_ruhr_rf$n_features <- 1


validation_dataframe_ruhr_lasso_lambda_min<-validation_for_list_of_found_formulas(list_unique_lasso_lambda_min_formulas, ruhr_data_train_first_iteration,ruhr_data_test_first_iteration)


validation_dataframe_ruhr_lasso_lambda_1se<-validation_for_list_of_found_formulas(list_unique_lasso_lambda_1se_formulas, ruhr_data_train_first_iteration,ruhr_data_test_first_iteration)



validation_dataframe_ruhr_step_aic<-validation_for_list_of_found_formulas(list_unique_step_aic_formulas, ruhr_data_train_first_iteration,ruhr_data_test_first_iteration)
validation_dataframe_ruhr_step_bic<-validation_for_list_of_found_formulas(list_unique_step_bic_formulas, ruhr_data_train_first_iteration,ruhr_data_test_first_iteration)


validation_dataframe_ruhr_step_5_aic<-validation_for_list_of_found_formulas(list_unique_step_5_aic_formulas, ruhr_data_train_first_iteration,ruhr_data_test_first_iteration)
validation_dataframe_ruhr_step_5_bic<-validation_for_list_of_found_formulas(list_unique_step_5_bic_formulas, ruhr_data_train_first_iteration,ruhr_data_test_first_iteration)






{
  combine_validation_and_most_selected_dfs<- function(validation_df, most_selected_df){
    #most_selected_df<-most_selected_formulas_analysis_feature_selection_ruhr_lasso_lambda_min
    #validation_df<-validation_dataframe_ruhr_lasso_lambda_min
    most_selected_df$Formula<-paste("log_e.coli ~ ", as.character(most_selected_df$Formula),sep = "")
    check_up_df<- full_join(validation_df,most_selected_df, by="Formula")
    check_up_df_sorted <- check_up_df %>% arrange(desc(in50), mean_MSE)
    return(check_up_df_sorted)
  }
}



combi_validation_selection_dataframe_ruhr_lasso_lambda_1se<-combine_validation_and_most_selected_dfs(validation_dataframe_ruhr_lasso_lambda_1se, most_selected_formulas_analysis_feature_selection_ruhr_lasso_lambda_1se)
combi_validation_selection_dataframe_ruhr_lasso_lambda_min<-combine_validation_and_most_selected_dfs(validation_dataframe_ruhr_lasso_lambda_min, most_selected_formulas_analysis_feature_selection_ruhr_lasso_lambda_min)

combi_validation_selection_dataframe_ruhr_step_aic<-combine_validation_and_most_selected_dfs(validation_dataframe_ruhr_step_aic, most_selected_formulas_analysis_feature_selection_ruhr_step_aic)
combi_validation_selection_dataframe_ruhr_step_bic<-combine_validation_and_most_selected_dfs(validation_dataframe_ruhr_step_bic, most_selected_formulas_analysis_feature_selection_ruhr_step_bic)

combi_validation_selection_dataframe_ruhr_step_5_aic<-combine_validation_and_most_selected_dfs(validation_dataframe_ruhr_step_5_aic, most_selected_formulas_analysis_feature_selection_ruhr_step_5_aic)
combi_validation_selection_dataframe_ruhr_step_5_bic<-combine_validation_and_most_selected_dfs(validation_dataframe_ruhr_step_5_bic, most_selected_formulas_analysis_feature_selection_ruhr_step_5_bic)



big_validation_data_frame<-rbind(combi_validation_selection_dataframe_ruhr_lasso_lambda_1se,combi_validation_selection_dataframe_ruhr_lasso_lambda_min,combi_validation_selection_dataframe_ruhr_step_aic,combi_validation_selection_dataframe_ruhr_step_bic,combi_validation_selection_dataframe_ruhr_step_5_aic,combi_validation_selection_dataframe_ruhr_step_5_bic, validation_dataframe_ruhr_rf)
big_validation_data_frame<-big_validation_data_frame%>% arrange(desc(in50), mean_R2, mean_MSE)
#View(big_validation_data_frame)

# remove duplicates cvan not use unique because different mean_mse depends which rain split was found in


#big_validation_data_frame[big_validation_data_frame$Formula== "log_e.coli ~ r_mean_mean_123 + ka_mean_mean_12 + r_mean_mean_345 + r_mean_mean_123:ka_mean_mean_12 + ka_mean_mean_12:r_mean_mean_345",]

big_validation_data_frame_removed_duplicates <- big_validation_data_frame[-c(which(duplicated(big_validation_data_frame$Formula))),]


big_validation_data_frame_only_duplicates <- big_validation_data_frame[c(which(duplicated(big_validation_data_frame$Formula))),]
big_validation_data_frame_only_duplicates_aggregated_formulas<-aggregate(Freq ~ mean_MSE+mean_R2+ n_obs+  mean_sd_pred_train_to_true+in95+below95+below90+in50+Formula+n_features, big_validation_data_frame_only_duplicates,sum)
{
  aggregate_big_dataframe_by_formula <- function(big_validation_data_frame_temp){
    #big_validation_data_frame_temp<-big_validation_data_frame_only_duplicates
    new_df_agg<- aggregate(Freq ~ mean_MSE+mean_R2+ n_obs+  mean_sd_pred_train_to_true+in95+below95+below90+in50+Formula+n_features, big_validation_data_frame_temp,sum)
    return(new_df_agg)
  }
}
big_validation_data_frame_only_duplicates_aggregated<-aggregate_big_dataframe_by_formula(big_validation_data_frame_only_duplicates)
{
  percentage_of_validated_formulas <- function(big_validation_data_frame_temp){
    #big_validation_data_frame_temp<-big_validation_data_frame_only_duplicates_aggregated
    only_validated<- big_validation_data_frame_temp%>% filter(in50==5)
    n_formulas_valid <- nrow(only_validated)
    n_formulas <- nrow(big_validation_data_frame_temp)
    percentage_validated<-nrow(only_validated) / nrow(big_validation_data_frame_temp)
    mean_MSE<-mean(only_validated$mean_MSE)
    mean_R2<-mean(only_validated$mean_R2)
    mean_sd_pred_train_to_true<-mean(only_validated$mean_sd_pred_train_to_true)
    n_features<-mean(only_validated$n_features)
    df_temp <- data.frame(percentage_validated,mean_MSE, mean_R2, mean_sd_pred_train_to_true, n_features, n_formulas,n_formulas_valid) 
    return(df_temp)
  }
}

big_validation_data_frame_removed_duplicates_Freq_1<-big_validation_data_frame_removed_duplicates %>% filter(Freq==1)

df_percentage_of_validated_formulas_only_duplicated<- percentage_of_validated_formulas(big_validation_data_frame_only_duplicates_aggregated)
df_percentage_of_validated_formulas_removed_duplicates <- percentage_of_validated_formulas(big_validation_data_frame_removed_duplicates_Freq_1)
comparison_df_validation_duplicated_vs_unique_formulas<-rbind(df_percentage_of_validated_formulas_only_duplicated,df_percentage_of_validated_formulas_removed_duplicates)
rownames(comparison_df_validation_duplicated_vs_unique_formulas) <- c("duplicated_formulas", "found_only_once")
#in results
comparison_df_validation_duplicated_vs_unique_formulas


######speciel filter out rf


big_validation_data_frame_removed_duplicates_Freq_1_rf<-big_validation_data_frame_removed_duplicates %>% filter(Freq==1, rf !=T)

df_percentage_of_validated_formulas_only_duplicated<- percentage_of_validated_formulas(big_validation_data_frame_only_duplicates_aggregated)
df_percentage_of_validated_formulas_removed_duplicates <- percentage_of_validated_formulas(big_validation_data_frame_removed_duplicates_Freq_1_rf)
comparison_df_validation_duplicated_vs_unique_formulas_rf<-rbind(df_percentage_of_validated_formulas_only_duplicated,df_percentage_of_validated_formulas_removed_duplicates)
rownames(comparison_df_validation_duplicated_vs_unique_formulas_rf) <- c("duplicated_formulas", "found_only_once")
#in results
comparison_df_validation_duplicated_vs_unique_formulas_rf


#make formula out of this!
big_validation_data_frame_removed_duplicates$step_aic <- F
big_validation_data_frame_removed_duplicates$step_bic <- F
big_validation_data_frame_removed_duplicates$step_5_aic <- F
big_validation_data_frame_removed_duplicates$step_5_bic <- F
big_validation_data_frame_removed_duplicates$lasso_lambda_min <- F
big_validation_data_frame_removed_duplicates$lasso_lambda_1se <- F
big_validation_data_frame_removed_duplicates$rf <- F
{
  k<-sapply(list_unique_lasso_lambda_min_formulas, function(x) which(x == big_validation_data_frame_removed_duplicates$Formula) )
  big_validation_data_frame_removed_duplicates$lasso_lambda_min[c(k)] <- T
  
  k<-sapply(list_unique_lasso_lambda_1se_formulas, function(x) which(x == big_validation_data_frame_removed_duplicates$Formula) )
  big_validation_data_frame_removed_duplicates$lasso_lambda_1se[c(k)] <- T
  
  k<-sapply(list_unique_step_aic_formulas, function(x) which(x == big_validation_data_frame_removed_duplicates$Formula) )
  big_validation_data_frame_removed_duplicates$step_aic[c(k)] <- T
  
  
  k<-sapply(list_unique_step_bic_formulas, function(x) which(x == big_validation_data_frame_removed_duplicates$Formula) )
  big_validation_data_frame_removed_duplicates$step_bic[c(k)] <- T
  
  k<-sapply(list_unique_step_5_aic_formulas, function(x) which(x == big_validation_data_frame_removed_duplicates$Formula) )
  big_validation_data_frame_removed_duplicates$step_5_aic[c(k)] <- T
  
  k<-sapply(list_unique_step_5_bic_formulas, function(x) which(x == big_validation_data_frame_removed_duplicates$Formula) )
  big_validation_data_frame_removed_duplicates$step_5_bic[c(k)] <- T
  
  k<-unlist(lapply(big_validation_data_frame_removed_duplicates$Formula, str_detect ,"rfModel"))
  big_validation_data_frame_removed_duplicates$rf[c(k)] <- T
  
}

nrow(big_validation_data_frame_removed_duplicates %>% filter(in50==5, rf ==T))
#fot blue red barplot occurence vs occurence_validated_only
{
  build_df_for_percentage_occurence_barplot <- function(big_validation_data_frame_removed_duplicates_temp){
    #big_validation_data_frame_removed_duplicates_temp <- big_validation_data_frame_removed_duplicates
    unique_formula_lengths<- sort(unique(big_validation_data_frame_removed_duplicates_temp$n_features))
    new_df<-as.data.frame(unique_formula_lengths)
    names(new_df)<- "formula_length"
    
    
    new_df$occurence<-sapply(unique_formula_lengths, function(x) sum(x ==big_validation_data_frame_removed_duplicates_temp$n_features) )
    big_validation_data_frame_removed_duplicates_temp_removed_duplicates_only <-big_validation_data_frame_removed_duplicates_temp%>%filter(below95==5,below90==5,in95==5,in50==5)
    new_df$occurence_validated_only<-sapply(unique_formula_lengths, function(x) sum(x ==big_validation_data_frame_removed_duplicates_temp_removed_duplicates_only$n_features) )
    new_df<-new_df %>% mutate(percentage_validated=occurence_validated_only/occurence)
    new_df$occurence_cum<-cumsum( new_df$occurence)
    return(new_df)  
  }
}



df_percentage_of_validated_formulas_per_length<- build_df_for_percentage_occurence_barplot(big_validation_data_frame_removed_duplicates)
df_percentage_of_validated_formulas_per_length[df_percentage_of_validated_formulas_per_length$formula_length==1,]$formula_length <- 0

{
  plot_df_percentage_of_validated_formulas_per_length <- function(df_percentage_of_validated_formulas_per_length_temp){
    # df_percentage_of_validated_formulas_per_length_temp<-df_percentage_of_validated_formulas_per_length
    e<-df_percentage_of_validated_formulas_per_length_temp%>% select(formula_length, occurence, occurence_validated_only)
    
    my_data_long <- melt(e, id.vars = c("formula_length"))
    #plott the long_df
    ggplot(data=my_data_long,aes(x=formula_length, y=value, fill=variable, color=variable, alpha=variable)) +
      geom_bar(stat="identity",position ="identity") +
      #geom_bar(stat="identity",position ="fill") +
      scale_x_continuous(breaks = 0:max(my_data_long$formula_length))+
      scale_y_continuous(breaks = 0:max(my_data_long$value) )+
      #scale_y_continuous(labels = scales::percent_format())+
      
      scale_colour_manual(values=c("lightblue4","red")) +
      scale_fill_manual(values=c("lightblue","pink")) +
      scale_alpha_manual(values=c(.3, .8))
    
  }
}

#in results occurence DF
{
  plot_df_percentage_of_validated_formulas_per_length_100percent <- function(df_percentage_of_validated_formulas_per_length_temp){
    # df_percentage_of_validated_formulas_per_length_temp<-df_percentage_of_validated_formulas_per_length
    e<-df_percentage_of_validated_formulas_per_length_temp%>% select(formula_length, occurence, occurence_validated_only)
    e<-e%>% mutate(occurence_validated_perc = occurence_validated_only/occurence)
    e$occurence_validated_perc[is.na(e$occurence_validated_perc)] <- 0
    e$occurence[e$occurence!=0]<-1 
    e<-e%>% select(formula_length, occurence, occurence_validated_perc)
    my_data_long <- melt(e, id.vars = c("formula_length"))
    #plott the long_df
    ggplot(data=my_data_long,aes(x=formula_length, y=value, fill=variable, color=variable, alpha=variable)) +
      geom_bar(stat="identity",position ="identity") +
      #geom_bar(stat="identity",position ="fill") +
      scale_x_continuous(breaks = 0:max(my_data_long$formula_length))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      
      scale_colour_manual(values=c("lightblue4","red")) +
      scale_fill_manual(values=c("lightblue","pink")) +
      scale_alpha_manual(values=c(.3, .8))
    
  }
}
#formulas with length 0 are trees

tyty<-df_percentage_of_validated_formulas_per_length
tyty$cumsum_valid<- cumsum(tyty$occurence_validated_only)
tyty$cumsum_occurence<-cumsum(tyty$occurence)
tyty<-tyty%>% mutate(validated_cumsum = cumsum_valid/ max(cumsum_valid))

tyty2 <- tyty %>% select(formula_length, validated_cumsum)
tyty2$all_validated <- 1
tyty2<-tyty2[,c(1,3,2)]
my_data_long <- melt(tyty2, id.vars = c("formula_length"))
#plott the long_df
ggplot(data=my_data_long,aes(x=formula_length, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity",position ="identity") +
  #geom_bar(stat="identity",position ="fill") +
  scale_x_continuous(breaks = 0:max(my_data_long$formula_length))+
  scale_y_continuous(breaks = seq(0, 1, .05),
                     labels = scales::percent,
                     limits = c(0, 1)) +
  
  scale_colour_manual(values=c("lightblue4","red")) +
  scale_fill_manual(values=c("lightblue","pink")) +
  scale_alpha_manual(values=c(.3, .8))

#in results
plot_df_percentage_of_validated_formulas_per_length(df_percentage_of_validated_formulas_per_length)

plot_df_percentage_of_validated_formulas_per_length_100percent(df_percentage_of_validated_formulas_per_length)





#big_validation_data_frame_removed_duplicates%>% filter(step_aic==T,step_bic==T, step_5_aic==T,step_5_bic==T)#, lasso_lambda_min==T,lasso_lambda_1se==T)

#big_validation_data_frame_removed_duplicates%>% filter( step_aic==T,step_bic==T)

#view(big_validation_data_frame_removed_duplicates%>% filter( step_aic==T,step_bic==T, step_5_aic==T,step_5_bic==T))
#view(big_validation_data_frame_removed_duplicates)


#in results
big_validation_data_frame_removed_duplicates_validated_only<-big_validation_data_frame_removed_duplicates%>%filter(below95==5,below90==5,in95==5,in50==5)
seq_nrow<-seq(1:nrow(big_validation_data_frame_removed_duplicates_validated_only))
Formula_names<-sapply(seq_nrow, function(x) paste("F_",x, sep = ""))
big_validation_data_frame_removed_duplicates_validated_only$Formulanames <- Formula_names
view(big_validation_data_frame_removed_duplicates_validated_only)




#change validated  and add another column for the comparisonbox
big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box <- big_validation_data_frame_removed_duplicates_validated_only

big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$n_selected_variables <- big_validation_data_frame_removed_duplicates_validated_only$n_features
big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$formel <- big_validation_data_frame_removed_duplicates_validated_only$Formula

#comparison
#only take models / without rf n_featues
big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$n_features[grepl("rfM",big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$formel)] <- NA
big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$n_selected_variables[grepl("rfM",big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$formel)] <- NA
#in results important!
build_dataframe_row_comparison_box_analysis_df( big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box)

#in results comparison box n_features
{
  a<-build_dataframe_row_comparison_box_big_validation_df(big_validation_data_frame_removed_duplicates_validated_only %>% filter(lasso_lambda_min==T))
  b<-build_dataframe_row_comparison_box_big_validation_df(big_validation_data_frame_removed_duplicates_validated_only %>% filter(lasso_lambda_1se==T))
  c<-build_dataframe_row_comparison_box_big_validation_df(big_validation_data_frame_removed_duplicates_validated_only %>% filter(step_aic==T))
  d<-build_dataframe_row_comparison_box_big_validation_df(big_validation_data_frame_removed_duplicates_validated_only %>% filter(step_5_bic==T))
  e<-build_dataframe_row_comparison_box_big_validation_df(big_validation_data_frame_removed_duplicates_validated_only %>% filter(step_5_aic==T))
  f<-build_dataframe_row_comparison_box_big_validation_df(big_validation_data_frame_removed_duplicates_validated_only %>% filter(step_5_bic==T))
  
  #in results comparison_df
  comparison_df<-rbind(a,b,c,d,e,f)
  rownames(comparison_df) <- c("lasso_min","lasso_1se", "step_aic", "step_bic","step_5_aic", "step_5_bic")
  comparison_df<-comparison_df%>% arrange(mean)
  comparison_df
}
#





#analysos formulas

{
  barplot_r2_mse<-function(big_validation_data_frame_removed_duplicates_validated_only_temp){
    big_validation_data_frame_removed_duplicates_validated_only_temp<-big_validation_data_frame_removed_duplicates_validated_only
    
    n_data_points<-mean(big_validation_data_frame_removed_duplicates_validated_only_temp$n_obs)
    title_name <- "Validated formulas - MSE"
    algo_name<- title_name
    annotation_label<-paste("Number of datapoints:", n_data_points, sep = " ")
    max_features_formula<-max(big_validation_data_frame_removed_duplicates_validated_only$mse)
    x_axis_labels <- 0:max_features_formula
    ymax<-max(table(big_validation_data_frame_removed_duplicates_validated_only$mse))
    
    plot_temp_bar<-ggplot(big_validation_data_frame_removed_duplicates_validated_only, aes(x=n_features)) + 
      geom_histogram(binwidth = 0.5,colour="black", fill="white") + 
      stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1) + 
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+#, limits = x_axis_labels) + 
      annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label) + 
      ggtitle(algo_name)
    
  }
}
{
  boxplot_validated_formulas_R2_MSE<-function(big_validation_data_frame_removed_duplicates_validated_only_temp, do_mse=T){
    #big_validation_data_frame_removed_duplicates_validated_only_temp<-big_validation_data_frame_removed_duplicates_validated_only%>% filter(rf !=T)
    #title_name <- "Validated formulas - R2"
    if (do_mse==T) {
      big_validation_data_frame_removed_duplicates_validated_only_temp$plot<-big_validation_data_frame_removed_duplicates_validated_only_temp$mean_MSE
      title_name <- "Validated formulas - MSE"
      max_mse<-max(big_validation_data_frame_removed_duplicates_validated_only_temp$plot)
      min_mse<-min(big_validation_data_frame_removed_duplicates_validated_only_temp$plot)
      y_axis_labels <- seq(0.2,0.7 , by = 0.05) 
      
      x_axis_labels <- 0
      xlabel<- "Mean-MSE"
    }else if (do_mse==F) {
      big_validation_data_frame_removed_duplicates_validated_only_temp$plot<-big_validation_data_frame_removed_duplicates_validated_only_temp$mean_R2
      title_name <- "Validated formulas - R2"
      
      y_axis_labels <- seq(0.2, 0.7, by = 0.05)
      x_axis_labels <- 0
      xlabel<- "Mean-R2"
    }
    max_features_formula<-max(big_validation_data_frame_removed_duplicates_validated_only_temp$plot)
    
    plot_temp<-ggplot(big_validation_data_frame_removed_duplicates_validated_only_temp, aes( y=plot)) + 
      geom_boxplot()+
      scale_y_continuous(labels = y_axis_labels, breaks = y_axis_labels, limits = c(0.25,0.7)) + 
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      xlab(xlabel)+#annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label) + 
      
      ggtitle(title_name)+
      coord_flip()
    
    return(plot_temp)
  } 
}




boxplot_validated_formulas_R2_MSE(big_validation_data_frame_removed_duplicates_validated_only,do_mse = T)

boxplot_validated_formulas_R2_MSE(big_validation_data_frame_removed_duplicates_validated_only,do_mse = F)

#in results
ggarrange(boxplot_validated_formulas_R2_MSE(big_validation_data_frame_removed_duplicates_validated_only,do_mse = T),boxplot_validated_formulas_R2_MSE(big_validation_data_frame_removed_duplicates_validated_only,do_mse = F), ncol = 1, nrow = 2)

#filtered out rf
ggarrange(boxplot_validated_formulas_R2_MSE(big_validation_data_frame_removed_duplicates_validated_only%>% filter(rf !=T),do_mse = T),boxplot_validated_formulas_R2_MSE(big_validation_data_frame_removed_duplicates_validated_only%>% filter(rf !=T),do_mse = F), ncol = 1, nrow = 2)

#max(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)

#plotting r2 vs mse

#in results performance df table()
{
  #analysis performance df
  b1 <-  mean(big_validation_data_frame_removed_duplicates_validated_only$mean_R2)
  c1 <- median(big_validation_data_frame_removed_duplicates_validated_only$mean_R2)
  d1 <- min(big_validation_data_frame_removed_duplicates_validated_only$mean_R2)
  e1 <- max(big_validation_data_frame_removed_duplicates_validated_only$mean_R2)
  f1 <- getmode(big_validation_data_frame_removed_duplicates_validated_only$mean_R2)
  g1<-quantile(big_validation_data_frame_removed_duplicates_validated_only$mean_R2)[2]
  h1<-quantile(big_validation_data_frame_removed_duplicates_validated_only$mean_R2)[4]
  t1<- as.data.frame(list(b1,c1,d1,e1,f1,g1,h1))  
  names(t1) <- c("mean", "median", "min", "max", "modal", "25%", "75%")
  t1
  
  b <-  mean(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)
  c <- median(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)
  d <- min(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)
  e <- max(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)
  f <- getmode(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)
  g<-quantile(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)[2]
  h<-quantile(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)[4]
  t<- as.data.frame(list(b,c,d,e,f,g,h))  
  names(t) <- c("mean", "median", "min", "max", "modal", "25%", "75%")
  t
  
  
  
  
  
  
  t3<-rbind(t,t1)
  
  rownames(t3) <- c("MSE", "R2")
  t3
  
}



mean_mean_Mse<-mean(big_validation_data_frame_removed_duplicates_validated_only$mean_MSE)

big_validation_data_frame_removed_duplicates_validated_only_filtered_mean<-big_validation_data_frame_removed_duplicates_validated_only%>% filter(mean_MSE<mean_mean_Mse)
{ 
  plot_mse_vs_r2 <- function(big_validation_data_frame_removed_duplicates_validated_only_temp, do_mean_filtering=F){
    #big_validation_data_frame_removed_duplicates_validated_only_temp<-big_validation_data_frame_with_separated_duplicated_rows_validated_only
    #length(unique(big_validation_data_frame_removed_duplicates_validated_only_temp$Formulanames))    check this should be 150
    #length(unique(big_validation_data_frame_removed_duplicates_validated_only$Formulanames))
    #do_mean_filtering<- F
    #seq_nrow<-seq(1:nrow(big_validation_data_frame_removed_duplicates_validated_only_temp))
    #Formula_names<-sapply(seq_nrow, function(x) paste("F_",x, sep = ""))
    #big_validation_data_frame_removed_duplicates_validated_only_temp$Formulanames <- Formula_names
    if(do_mean_filtering==T){
      mean_mse_df<-mean(big_validation_data_frame_removed_duplicates_validated_only_temp$mean_MSE)
      
      big_validation_data_frame_removed_duplicates_validated_only_temp <- big_validation_data_frame_removed_duplicates_validated_only_temp %>% filter(mean_MSE<mean_mse_df) 
    }
    max_y<-max(big_validation_data_frame_removed_duplicates_validated_only_temp$mean_MSE, big_validation_data_frame_removed_duplicates_validated_only_temp$mean_R2)
    
    ggplot(big_validation_data_frame_removed_duplicates_validated_only_temp, aes(x = reorder(Formulanames, mean_MSE))) +
      geom_point(aes(y=mean_MSE),stat="identity",alpha=0.8,fill='lightblue',color='lightblue4', )+
      geom_point(aes(y=mean_R2),stat="identity",alpha=0.8,fill='pink',color='orange', )+
      
      theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      ggtitle(paste("Different formulas: ", nrow(big_validation_data_frame_removed_duplicates_validated_only_temp)))+
      xlab("Unique formulas")+
      ylab("MSE/R2")+
      scale_y_continuous(breaks=seq(from = 0, to = max_y,by = round(max_y/20,2)))
    
    
    
  } 
}

#in results
plot_mse_vs_r2(big_validation_data_frame_removed_duplicates_validated_only, do_mean_filtering = F)

plot_mse_vs_r2_filtered_mean<-plot_mse_vs_r2(big_validation_data_frame_removed_duplicates_validated_only, do_mean_filtering = T)


#in results!! but make plot nice beforehand
klakl<-big_validation_data_frame_with_separated_duplicated_rows_validated_only%>% select(mean_MSE, mean_R2, algorithm)%>% filter(mean_MSE< mean(mean_MSE))

gathered_klk<-gather(klakl,key = quantity, value = value, -mean_MSE, - algorithm)
head(gathered_klk)

ggplot(data = gathered_klk, mapping = aes(x = mean_MSE)) +
  geom_point(aes(y = value, color= algorithm))

{
  plot_boxplot_mse_vs_r2 <- function(big_validation_data_frame_removed_duplicates_validated_only_temp, do_mean_filtering=F){
    #big_validation_data_frame_removed_duplicates_validated_only_temp<-big_validation_data_frame_with_separated_duplicated_rows_validated_only
    
    #seq_nrow<-seq(1:nrow(big_validation_data_frame_removed_duplicates_validated_only_temp))
    #Formula_names<-sapply(seq_nrow, function(x) paste("F_",x, sep = ""))
    #big_validation_data_frame_removed_duplicates_validated_only_temp$Formulanames <- Formula_names
    if(do_mean_filtering==T){
      mean_mse_df<-mean(big_validation_data_frame_removed_duplicates_validated_only_temp$mean_MSE)
      
      big_validation_data_frame_removed_duplicates_validated_only_temp <- big_validation_data_frame_removed_duplicates_validated_only_temp %>% filter(mean_MSE<mean_mse_df) 
    }
    min_y<-min(big_validation_data_frame_removed_duplicates_validated_only_temp$mean_MSE, big_validation_data_frame_removed_duplicates_validated_only_temp$mean_R2)
    max_y <- max(big_validation_data_frame_removed_duplicates_validated_only_temp$mean_MSE, big_validation_data_frame_removed_duplicates_validated_only_temp$mean_R2)
    ggplot(big_validation_data_frame_removed_duplicates_validated_only_temp, aes(x = algorithm))+#, y=mean_MSE)) +
      #geom_boxplot(alpha=0.8,fill='lightblue',color='lightblue4')+
      geom_boxplot(aes(y=mean_MSE),alpha=0.8,fill='lightblue',color='lightblue4')+
      geom_boxplot(aes(y=mean_R2),alpha=0.8,fill='pink',color='orange', )+
      xlab("Unique formulas")+
      ylab("MSE/R2")+
      scale_y_continuous(breaks=seq(from = 0, to = 1,by = 0.02), limits= c(min_y,max_y))
    #ylim(c(0,1),)+
    
    
    
    
    
    
  } 
  }

#in results
plot_boxplot_mse_vs_r2(big_validation_data_frame_with_separated_duplicated_rows_validated_only, do_mean_filtering = F)


#e<-big_validation_data_frame_removed_duplicates_validated_only_temp%>% filter(step_aic == T)
#f<-big_validation_data_frame_removed_duplicates_validated_only_temp%>% filter(lasso_lambda_1se == T)
#g<-big_validation_data_frame_removed_duplicates_validated_only_temp%>% filter(lasso_lambda_min == T)
#mean(e$mean_MSE)
#mean(f$mean_MSE)
#mean(g$mean_MSE)
#unique_found_combindations_only_two_times_seleted[2,]


#w[,grep("^step_aic$", colnames(w) ):grep("^lasso_lambda_1se$", colnames(w) )] == unique_found_combindations_only_two_times_seleted[2,]
#which(apply(w[,grep("^step_aic$", colnames(w) ):grep("^lasso_lambda_1se$", colnames(w) )], MARGIN = 1, function(x) all(x==unique_found_combindations_only_two_times_seleted[1,] )))
{ 
  build_duplicated_rows_for_facetted_df<-function(big_dataframe_validated){
    #big_dataframe_validated<- big_validation_data_frame_removed_duplicates_validated_only
    w<-big_dataframe_validated
    #unique(w %>% select(step_aic,step_bic,step_5_aic,step_5_bic, lasso_lambda_min,lasso_lambda_1se, rf))
    
    
    
    unique_found_combindations<-unique(w %>% select(step_aic,step_bic,step_5_aic,step_5_bic, lasso_lambda_min,lasso_lambda_1se, rf))
    unique_found_combindations_only_two_times_seleted<-unique_found_combindations[1,]
    
    for( i in 1:nrow(unique_found_combindations)){
      #i<-1
      number_of_trues<-sum(unique_found_combindations[i,]==T)
      if(number_of_trues >1){
        unique_found_combindations_only_two_times_seleted<- rbind(unique_found_combindations_only_two_times_seleted,unique_found_combindations[i,])
        
      }
    }
    unique_found_combindations_only_two_times_seleted<-unique_found_combindations_only_two_times_seleted[-1,]
    list_unique_found_formula_selected_two_times <- list()
    for (i in 1:nrow(w)) {
      #i=1
      
      for (j in 1:nrow(unique_found_combindations_only_two_times_seleted)) {
        #j=3
        unique_found_formula_selected_two_times<-all(w[i,grep("^step_aic$", colnames(w) ):grep("^rf$", colnames(w) )] == unique_found_combindations_only_two_times_seleted[j,])
        list_unique_found_formula_selected_two_times<- append(list_unique_found_formula_selected_two_times,list(unique_found_formula_selected_two_times))
        if (unique_found_formula_selected_two_times) {
          
          
          w_new <- w[i,]
          
          list_index<-grep(T,w_new)
          
          for(entry_indx in 1:length(list_index)){
            #entry_indx<-1
            if(entry_indx ==1){
              new_df <- w[i,]
              switch_to_false<-list_index[-entry_indx]
              new_df[,c(switch_to_false)] <- F 
              
            }
            else{
              new_row <- w[i,]
              switch_to_false<-list_index[-entry_indx]
              new_row[,c(switch_to_false)] <- F 
              new_df <- rbind(new_df, new_row) 
              
            }
            
          }
          
        } 
        ###sum(list_unique_found_formula_selected_two_times==T) == 1 instanziert den df
        if(sum(list_unique_found_formula_selected_two_times==T) == 1 && unique_found_formula_selected_two_times){
          all_duplicated_rows <- new_df
        }
        
        else if (sum(list_unique_found_formula_selected_two_times==T) != 1 && unique_found_formula_selected_two_times){
          all_duplicated_rows<-rbind(all_duplicated_rows,new_df)
        }    
        
      }
      
    }
    list_duplicated_rownames<-unique(all_duplicated_rows$Formulanames)
    big_dataframe_validated_new<-big_dataframe_validated
    for(i in 1:length(list_duplicated_rownames)){ #get rid of row that was found by more than 1 algorithm. to finally add new builded rows
      #i<- 1
      row_indx<-which(big_dataframe_validated$Formulanames ==  list_duplicated_rownames[i] )
      big_dataframe_validated_new<- big_dataframe_validated_new[-row_indx,]
    }
    
    big_dataframe_validated_new<-rbind(big_dataframe_validated_new, all_duplicated_rows)
    k<-big_dataframe_validated_new
    k$algorithm <- "nan"
    
    k$algorithm[which(k$step_aic==T,)] = "step_aic"
    k$algorithm[which(k$step_bic==T,)] = "step_bic"
    k$algorithm[which(k$step_5_aic==T,)] = "step_5_aic"
    k$algorithm[which(k$step_5_bic==T,)] = "step_5_bic"
    k$algorithm[which(k$lasso_lambda_min==T,)] = "lasso_lambda_min"
    k$algorithm[which(k$lasso_lambda_1se==T,)] = "lasso_lambda_1se"
    k$algorithm[which(k$rf==T,)] = "rf"
    return(k)
  }
}

big_validation_data_frame_with_separated_duplicated_rows_validated_only<-build_duplicated_rows_for_facetted_df(big_validation_data_frame_removed_duplicates_validated_only)



#big_validation_data_frame_removed_duplicates_validated_only[big_validation_data_frame_removed_duplicates_validated_only$Formulanames =="F_76",]

#plot_mse_vs_r2(k, do_mean_filtering = F)
#plot_mse_r2_mean_filtered<-plot_mse_vs_r2(big_validation_data_frame_removed_duplicates_validated_only, do_mean_filtering = T)
#plot_mse_r2_mean_filtered_duplicated_rows_for_facet<-plot_mse_vs_r2(k, do_mean_filtering = T)
plot_mse_r2_mean_filtered_duplicated_rows_for_facet<-plot_mse_vs_r2(big_validation_data_frame_with_separated_duplicated_rows_validated_only, do_mean_filtering = T)
#in_results
plot_mse_r2_mean_filtered_duplicated_rows_for_facet
plot_mse_r2_mean_filtered_duplicated_rows_for_facet + facet_grid(algorithm ~ .)



#in results
big_validation_data_frame_with_separated_duplicated_rows_validated_only%>%filter(mean_MSE == min(big_validation_data_frame_removed_duplicates_validated_only_filtered_mean$mean_MSE)) %>% select(-c(Formula, in95,below95,in50,below90, step_aic,step_bic,step_5_aic,step_5_bic,lasso_lambda_1se, lasso_lambda_min,rf))
big_validation_data_frame_with_separated_duplicated_rows_validated_only%>%filter(mean_R2 == max(big_validation_data_frame_removed_duplicates_validated_only_filtered_mean$mean_R2)) %>% select(-c(Formula, in95,below95,in50,below90, step_aic,step_bic,step_5_aic,step_5_bic,lasso_lambda_1se, lasso_lambda_min,rf))

#big_validation_data_frame_removed_duplicates_validated_only[big_validation_data_frame_removed_duplicates_validated_only$Formulanames =="F_76",]
{
  print_out_without_formula_and_algo_cols <- function(big_validation_data_frame_removed_duplicates_validated_only_temp){
    return(big_validation_data_frame_removed_duplicates_validated_only_temp  %>% select(-c(Formula, in95,below95,in50,below90, step_aic,step_bic,step_5_aic,step_5_bic,lasso_lambda_1se, lasso_lambda_min)))
  }
}

#print_out_without_formula_and_algo_cols(big_validation_data_frame_removed_duplicates_validated_only_filtered_mean%>%filter(Formulanames=="F_76"))

#big_validation_data_frame_removed_duplicates_validated_only_filtered_mean%>%filter(Formulanames=="F_76")

#in results
big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formula[big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formulanames=="F_146"]
big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formula[big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formulanames=="F_150"]





#Correlation feature

#analyse which algorithm validated the most of its found fomrulas!
{
  validated_per_found_formula<-function(big_validation_data_frame_removed_duplicates_prefiltered){
    #big_validation_data_frame_removed_duplicates_prefiltered <- big_validation_data_frame_removed_duplicates%>% filter(rf ==T)
    big_validation_data_frame_removed_duplicates_prefiltered_validated<-big_validation_data_frame_removed_duplicates_prefiltered %>% filter(in50== 5)
    unique_found_models<-nrow(big_validation_data_frame_removed_duplicates_prefiltered)
    unique_found_models_validated<-nrow(big_validation_data_frame_removed_duplicates_prefiltered_validated)
    percentage_validated <-unique_found_models_validated/unique_found_models
    mean_MSE <- mean(big_validation_data_frame_removed_duplicates_prefiltered_validated$mean_MSE)
    mean_R2 <- mean(big_validation_data_frame_removed_duplicates_prefiltered_validated$mean_R2)
    mean_sd_pred_train_to_true<-mean(big_validation_data_frame_removed_duplicates_prefiltered_validated$mean_sd_pred_train_to_true)
    
    return(data.frame(unique_found_models_validated, unique_found_models, percentage_validated, mean_MSE, mean_R2,mean_sd_pred_train_to_true))
    
    
  }
}



df_rf<- big_validation_data_frame_removed_duplicates%>% filter(rf ==T)
df_step_aic<- big_validation_data_frame_removed_duplicates%>% filter(step_aic ==T)
df_step_bic<- big_validation_data_frame_removed_duplicates%>% filter(step_bic ==T)
df_step_5_aic<- big_validation_data_frame_removed_duplicates%>% filter(step_5_aic ==T)
df_step_5_bic<- big_validation_data_frame_removed_duplicates%>% filter(step_5_bic ==T)
df_lasso_lambda_min<- big_validation_data_frame_removed_duplicates%>% filter(lasso_lambda_min ==T)
df_lasso_lambda_1se<- big_validation_data_frame_removed_duplicates%>% filter(lasso_lambda_1se ==T)

df_percentage_validated_per_algo<- rbind(validated_per_found_formula(df_rf),
                                         validated_per_found_formula(df_step_aic),
                                         validated_per_found_formula(df_step_bic),
                                         validated_per_found_formula(df_step_5_aic),
                                         validated_per_found_formula(df_step_5_bic),
                                         validated_per_found_formula(df_lasso_lambda_min),
                                         validated_per_found_formula(df_lasso_lambda_1se))
rownames(df_percentage_validated_per_algo) <- c("rf", "step_aic","step_bic", "step_5_aic","step_5_bic", "lasso_lambda_min", "lasso_lambda_1se")
#in results
df_percentage_validated_per_algo%>% arrange(desc(percentage_validated))

#analysis features
{
  build_occurence_table_list_of_formulas<-function(formula_list){
    #formula_list <-big_validation_data_frame_removed_duplicates_validated_only$formel
    dummy_df<-data.frame(matrix(0, ncol = length((colnames(df_with_all_variable_names)))+3, nrow = 1))
    colnames(dummy_df)<- colnames(df_with_all_variable_names)
    names(dummy_df)[length(names(dummy_df))-2]<-"formel" 
    names(dummy_df)[length(names(dummy_df))-1]<-"iteration_number" 
    names(dummy_df)[length(names(dummy_df))]<-"n_selected_variables"   
    
    for (fold_idx in 1:length(formula_list)) {
      #fold_idx <-1
      a<-str_split(string = formula_list[[fold_idx]], pattern = "~ ")
      #b<-str_split(a[[1]][2], pattern = " \\+ ")
      coef_pasted_together<-a[[1]][2]
      #extracted_coefficients1<- extract.coef(model_list_all_folds1[[fold_idx]], lambda = lambda_value)
      if(str_count(string = extracted_coefficients, pattern = "\\+")==0){
        new_row<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        new_row$n_selected_variables <- 0
        new_row$iteration_number <- iteration_numb
      }else{
        #coef_pasted_together<-paste(names(extracted_coefficients[-1]), collapse = " + ")
        
        new_row<- get_occurences_glmnet_feature_selection_all_columns(build_rename_cols_to_variable_names(df_with_all_variable_names),coef_pasted_together ,iteration_numb = iteration_numb)
      }
      dummy_df<-rbind(dummy_df, new_row)
    }
    dummy_df<-dummy_df[,c(1:(ncol(dummy_df)-3),ncol(dummy_df),(ncol(dummy_df)-1),(ncol(dummy_df)-2))] #reorder formula to last row
    
    df_all_variables <- dummy_df[-1,]
    df_only_selected_variables<-dummy_df[-1,]%>% select_if(~ !is.numeric(.) || sum(.) != 0) 
    
    return_object<-list(df_only_selected_variables,df_all_variables)
    return(return_object)
  }
}





analysis_feature_selection_ruhr_validated_formulas <- build_occurence_table_list_of_formulas(big_validation_data_frame_removed_duplicates_validated_only$Formula)[[1]]


analyse_frequecy_selected_features_validated_formulas_sorted<-analysis_feature_selection_ruhr_validated_formulas[-c((ncol(analysis_feature_selection_ruhr_validated_formulas)-2):ncol(analysis_feature_selection_ruhr_validated_formulas))]%>% colSums()
analyse_frequecy_selected_features_validated_formulas_sorted <- sort(analyse_frequecy_selected_features_validated_formulas_sorted, decreasing = T)
analyse_frequecy_selected_features_validated_formulas_sorted <-as.data.frame(analyse_frequecy_selected_features_validated_formulas_sorted)
n_diff_features<-nrow(analyse_frequecy_selected_features_validated_formulas_sorted)
names(analyse_frequecy_selected_features_validated_formulas_sorted) <- "n_times"
analyse_frequecy_selected_features_validated_formulas_sorted$percentage <- analyse_frequecy_selected_features_validated_formulas_sorted$n_times/nrow(analysis_feature_selection_ruhr_validated_formulas )

#in results
view(analyse_frequecy_selected_features_validated_formulas_sorted)
#percentage of validated formulas found
percentage_validated_formulas_found_step_aic<-sum(big_validation_data_frame_removed_duplicates_validated_only$step_aic==T)/nrow(big_validation_data_frame_removed_duplicates_validated_only)
percentage_validated_formulas_found_step_bic<-sum(big_validation_data_frame_removed_duplicates_validated_only$step_bic==T)/nrow(big_validation_data_frame_removed_duplicates_validated_only)
percentage_validated_formulas_found_step_5_aic<-sum(big_validation_data_frame_removed_duplicates_validated_only$step_5_aic==T)/nrow(big_validation_data_frame_removed_duplicates_validated_only)
percentage_validated_formulas_found_step_5_bic<-sum(big_validation_data_frame_removed_duplicates_validated_only$step_5_bic==T)/nrow(big_validation_data_frame_removed_duplicates_validated_only)
percentage_validated_formulas_found_lasso_min<-sum(big_validation_data_frame_removed_duplicates_validated_only$lasso_lambda_min==T)/nrow(big_validation_data_frame_removed_duplicates_validated_only)
percentage_validated_formulas_found_lasso_1se<-sum(big_validation_data_frame_removed_duplicates_validated_only$lasso_lambda_1se==T)/nrow(big_validation_data_frame_removed_duplicates_validated_only)
percentage_validated_formulas_found_rf<-sum(big_validation_data_frame_removed_duplicates_validated_only$rf==T)/nrow(big_validation_data_frame_removed_duplicates_validated_only)
df_percentage_validated_formulas_found <- data.frame(c(percentage_validated_formulas_found_step_aic,
                                                       percentage_validated_formulas_found_step_bic,
                                                       percentage_validated_formulas_found_step_5_aic,
                                                       percentage_validated_formulas_found_step_5_bic,
                                                       percentage_validated_formulas_found_lasso_min,
                                                       percentage_validated_formulas_found_lasso_1se,
                                                       percentage_validated_formulas_found_rf
))%>%t()
colnames(df_percentage_validated_formulas_found) <- c("step_aic","step_bic","step_5_aic","step_5_bic","lasso_min", "lasso_1se", "rf")
rownames(df_percentage_validated_formulas_found) <-"1"

view(df_percentage_validated_formulas_found)



#in results
#plot for n_features to occurence
plot_percentage_algorithms_vaidated_formulas<-barplot(df_percentage_validated_formulas_found,ylim = c(0,1), main = "Percentage how often algorithm found validated formula") 
axis(2,at=seq(0,1,by=0.05),labels=F) 
ylabel_column<- paste(round(df_percentage_validated_formulas_found, digits = 3)*100,"%", sep = "")
text(x = plot_percentage_algorithms_vaidated_formulas, y = df_percentage_validated_formulas_found, label =ylabel_column , pos = 3, cex = 0.8, col = "black")



big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box<-big_validation_data_frame_removed_duplicates_validated_only
big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box$n_features[grepl("rfM",big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$Formula)] <- NA
big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box$n_selected_variables[grepl("rfM",big_validation_data_frame_removed_duplicates_validated_only_for_comparison_box$Formula)] <- NA
{
  n_data_points<-big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box$n_obs
  title_name <- "Validated formulas - all algorithm"
  algo_name<- title_name
  annotation_label<-paste("Number of datapoints:", n_data_points, sep = " ")
  max_features_formula<-max(big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box$n_features, na.rm = T)
  x_axis_labels <- 0:max_features_formula
  ymax<-max(table(big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box$n_features))
  
  plot_temp_bar<-ggplot(data=subset(big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box, !is.na(n_features)), aes(x=n_features)) + 
    geom_histogram(binwidth = 0.5,colour="black", fill="white") + 
    stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1) + 
    scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+#, limits = x_axis_labels) + 
    annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label) + 
    ggtitle(algo_name)
  
  
}
{
  boxplot_validated_formula<-function(big_validation_data_frame_removed_duplicates_validated_only_temp,title_name){
    #big_validation_data_frame_removed_duplicates_validated_only_temp<-big_validation_data_frame_removed_duplicates_validated_only_temp
    #title_name <- "Validated formulas - all algorithm"
    max_features_formula<-max(big_validation_data_frame_removed_duplicates_validated_only_temp$n_features, na.rm = T)
    y_axis_labels <- 0:max_features_formula
    x_axis_labels <- 0:max_features_formula
    plot_temp_box<-ggplot(data=subset(big_validation_data_frame_removed_duplicates_validated_only_temp, !is.na(n_features)), aes( y=n_features)) + 
      geom_boxplot()+
      scale_y_continuous(labels = y_axis_labels, breaks = y_axis_labels) + 
      scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
      #annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label) + 
      ggtitle(title_name)+
      coord_flip()
    
    
  } 
}
#in results
ggarrange(boxplot_validated_formula(big_validation_data_frame_removed_duplicates_validated_only_for_n_features_bar_box,"Validated formulas - all algorithm"),plot_temp_bar, ncol = 1, nrow = 2)


#in results
#analyze variance inflation factor

er1<-big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formula[big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formulanames=="F_146"]
formula_highest_R2<-big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formula[big_validation_data_frame_with_separated_duplicated_rows_validated_only$Formulanames=="F_150"]

formula(er1)
formula(er2)
{     #inly for getting linear model!!
  validation2 <- function(formel,data_train,data_test){      
    #data_train <- ruhr_data_train_first_iteration
    #data_test <- ruhr_data_test_first_iteration
    #formel<-list_unique_lasso_formulas[[1]]
    #formel<-e
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
    #list_linear_models <<- append(list_linear_models, list(linear_model))
    #list_R2_iteration <- summary(linear_model)[["r.squared"]]
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
    within95 = test_bacteria < percentile_train_97_5& test_bacteria > percentile_train_2_5
    within50 = test_bacteria < percentile_train_75& test_bacteria > percentile_train_25
    
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
    
    return(list(dummy_df, linear_model))
  }
}

{
  validate_formula_and_do_vif<-function(formula_for_function){
    #formula_for_function <- e
    linear_model<- validation2(formula_for_function,ruhr_data_train_first_iteration,ruhr_data_test_first_iteration )[[2]]
    VIF_model<-car::vif(linear_model)
    print(VIF_model)
    max_Vif<-max(VIF_model)
    
    if(max_Vif>10){
      feature_to_remove<-names(VIF_model[VIF_model==max_Vif])
      print(paste(feature_to_remove, " was removed"))
      #feature_to_remove <- paste(feature_to_remove, " + ", sep = "") 
      
      new_formula<-gsub(paste(" ",feature_to_remove," ", sep = ""),"",formula_for_function)
      new_formula<-gsub("\\+\\+", "+",new_formula)
      new_formula<-gsub("\\~\\+", "\\ ~ ",new_formula)
      
    }else if (max_Vif < 10) {
      new_formula <- formula_for_function
      print("no feature has variance inflation factor > 10")
      
    }
    return(new_formula)
  }
}


{
  automized_validation_formula_and_do_vif<-function(formula_for_function){
    #formula_for_function <- e
    linear_model<- validation2(formula_for_function,ruhr_data_train_first_iteration,ruhr_data_test_first_iteration )[[2]]
    VIF_model<-car::vif(linear_model)
    print(VIF_model)
    max_Vif<-max(VIF_model)
    if(max_Vif>10){
      feature_to_remove<-names(VIF_model[VIF_model==max_Vif])
      print(paste(feature_to_remove, " was removed"))
      #feature_to_remove <- paste(feature_to_remove, " + ", sep = "") 
      
      new_formula<-gsub(paste(" ",feature_to_remove," ", sep = ""),"",formula_for_function)
      new_formula<-gsub("\\+\\+", "+",new_formula)
      new_formula<-gsub("\\~\\+", "\\ ~ ",new_formula)
      
    }else if (max_Vif < 10) {
      new_formula <- formula_for_function
      print("no feature has variance inflation factor > 10")
      stop_index_vif <<-1
      
    }
    return(new_formula)
  }
}
#in results
automate_vif <- function(formula_for_function_to_test){
  stop_index_vif <<-0
  new_formula<- automized_validation_formula_and_do_vif(formula_for_function_to_test)
  while (stop_index_vif ==0) {
    new_formula<-automized_validation_formula_and_do_vif(new_formula)
  }
  return(new_formula)
}

formula_with_reduced_colliniarities<-automate_vif(formula_highest_R2)
vif_df<-rbind(validation(formula_highest_R2,ruhr_data_train_first_iteration, ruhr_data_test_first_iteration),
              validation(formula_with_reduced_colliniarities,ruhr_data_train_first_iteration, ruhr_data_test_first_iteration))
rownames(vif_df)<- c("original_formula", "reduced multicolliniarity")
vif_df[,-ncol(vif_df)]







#d <- d %>% select(mean_MSE, mean_R2, n_features, 
#                  algorithm)
#d.tidy <- d %>% gather(algorithm, true, -mean_MSE, -mean_R2, -n_features) %>% select(-true)
#d.long <- d %>% gather(quantitiy, value, -n_features, -algorithm)
#ggplot(data = d.long, mapping =  aes(x = n_features, 
#                                    y = value, 
#                                     col = quantitiy )) + 
#  geom_point() + scale_color_hue() + 
#  facet_wrap(.~algorithm) + scale_y_continuous(limits = c(0,1))

#in results 

vif_df$Formulanames<-"F_150_1"

ert<-big_validation_data_frame_removed_duplicates_validated_only%>% select(mean_MSE, mean_R2, Formulanames)
ert2<-rbind(ert,  vif_df %>% select(mean_MSE, mean_R2, Formulanames))
ert2$label <- "original"
ert2[ert2$Formulanames== "F_150_1" & rownames(ert2[ert2$Formulanames== "F_150_1",])== "original_formula",]$label <- "original formula"
ert2[ert2$Formulanames== "F_150_1" & rownames(ert2[ert2$Formulanames== "F_150_1",])!= "original_formula",]$label <- "reduced multicollinearity"



ert2<- ert2%>% filter(mean_MSE<mean(ert2$mean_MSE) )
ert2_long<-ert2 %>% gather(quantity, value, -Formulanames, -label)
#add new point to plot! in results vif
big_validation_data_frame_removed_duplicates_validated_only_temp<- ert2_long

max_y<-max(big_validation_data_frame_removed_duplicates_validated_only_temp$value)
big_validation_data_frame_removed_duplicates_validated_only_temp$quantity <- as.factor(ert2_long$quantity)
names(ert2_long)

ggplot(data = ert2_long, mapping =  aes(x = reorder(Formulanames, value), y = value, color = label , shape = quantity
                                        
)) + 
  geom_point()+
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle(paste("Different formulas: ", length(unique(big_validation_data_frame_removed_duplicates_validated_only_temp$Formulanames))))+
  xlab("Unique formulas")+
  ylab("MSE/R2")+
  scale_y_continuous(breaks=seq(from = 0, to = max_y,by = round(max_y/20,2)))







ggplot(big_validation_data_frame_removed_duplicates_validated_only_temp, aes(x = reorder(Formulanames, mean_MSE))) +
  geom_point(aes(y=mean_MSE),stat="identity",alpha=0.8,fill=label,color='lightblue4' )#+
geom_point(aes(y=mean_R2),stat="identity",alpha=0.8,fill=label,color='orange', )+
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle(paste("Different formulas: ", nrow(big_validation_data_frame_removed_duplicates_validated_only_temp)))+
  xlab("Unique formulas")+
  ylab("MSE/R2")+
  scale_y_continuous(breaks=seq(from = 0, to = max_y,by = round(max_y/20,2)))


plot_mse_vs_r2_filtered_mean+
  geom_point(data = vif_df[2,], aes(y=mean_MSE),stat="identity",alpha=0.8,fill='red',color='red', )+
  geom_point(data = vif_df[2,],aes(y=mean_R2),stat="identity",alpha=0.8,fill='red',color='red', )

{
  get_best_5_models_per_method_for_mse_and_R2 <- function(algo_found_formulas_big_validation){
    #algo_found_formulas_big_validation<- big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(step_aic ==T)
    
    algo_found_formulas_big_validation<- algo_found_formulas_big_validation%>% arrange(mean_MSE)
    algo_aic_found_models_best_5_mse<-algo_found_formulas_big_validation[1:5,]%>% select(mean_MSE, mean_R2, mean_sd_pred_train_to_true,Freq, algorithm,n_features,Formulanames ,Formula  )
    algo_found_formulas_big_validation<-algo_found_formulas_big_validation%>%arrange(desc(mean_R2))
    algo_aic_found_models_best_5_R2<-algo_found_formulas_big_validation[1:5,]%>% select(mean_MSE, mean_R2, mean_sd_pred_train_to_true,Freq,algorithm, n_features,Formulanames ,Formula )
    
    return_object<- list(algo_aic_found_models_best_5_mse,algo_aic_found_models_best_5_R2)
    return(return_object)
    
  }
  }

# best 5 models with smalles mse's and rˆ2

best_5_step_aic<-get_best_5_models_per_method_for_mse_and_R2(step_aic_found_models<-big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(step_aic ==T))
best_5_step_bic<-get_best_5_models_per_method_for_mse_and_R2(step_aic_found_models<-big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(step_bic ==T))
best_5_step_5_aic<-get_best_5_models_per_method_for_mse_and_R2(step_aic_found_models<-big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(step_5_aic ==T))
best_5_step_5_bic<-get_best_5_models_per_method_for_mse_and_R2(step_aic_found_models<-big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(step_5_bic ==T))
best_5_lasso_lambda_min<-get_best_5_models_per_method_for_mse_and_R2(step_aic_found_models<-big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(lasso_lambda_min ==T))
best_5_lasso_lambda_1se<-get_best_5_models_per_method_for_mse_and_R2(step_aic_found_models<-big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(lasso_lambda_1se ==T))
best_5_rf<-get_best_5_models_per_method_for_mse_and_R2(step_aic_found_models<-big_validation_data_frame_with_separated_duplicated_rows_validated_only %>% filter(rf ==T))


list_best_algos <- list(best_5_step_aic,best_5_step_bic,best_5_step_5_aic,best_5_step_5_bic,best_5_lasso_lambda_min,best_5_lasso_lambda_1se, best_5_rf)
{
  get_best_formula_mse <- function(best_5_algo){
    #best_5_algo<- best_5_step_aic
    best_mse<- best_5_algo[[1]][1,]
    #best_r2<- best_5_algo[[2]][1,]
    return(best_mse)  
  }
}

{
  get_best_formula_r2 <- function(best_5_algo){
    #best_5_algo<- best_5_step_aic
    best_r2<- best_5_algo[[2]][1,]
    #best_r2<- best_5_algo[[2]][1,]
    return(best_r2)  
  }
}

#only best of all
best_mse_models_all_algorithms_mse<-sapply(list_best_algos,get_best_formula_mse )
best_mse_models_all_algorithms_r2<-sapply(list_best_algos,get_best_formula_r2 )
#lapply auf list
#till now only for mses
{
  split_up_list_build_df <- function(best_mses_models_all_algorithms){
    #best_mses_models_all_algorithms <-best_mse_models_all_algorithms_mse
    best_models_df<-as.data.frame(best_mses_models_all_algorithms) %>% t()
    best_models_df <- as.data.frame(best_models_df)
    rownames(best_models_df) <-(best_models_df$algorithm)
    
    for (i in 1:ncol(best_models_df)) {
      best_models_df[i] = unlist(best_models_df[i]) 
    }
    
    return(best_models_df)
  }
}


best_models_df_mse<-split_up_list_build_df(best_mse_models_all_algorithms_mse)
best_models_df_mse%>% arrange(mean_MSE)

{
  print_Df_mse<-best_models_df_mse%>% select(mean_MSE, mean_R2, algorithm)
  
  sorting_mse<-best_models_df_mse%>% select(algorithm,mean_MSE)%>% arrange(mean_MSE)%>% rownames()
  
  print_Df_mse<-print_Df_mse[sorting_mse,]
  
  
  print_Df_mse$best_mse <- 0
  min_MSE_df <-min(print_Df_mse$mean_MSE)
  print_Df_mse$best_mse[which(print_Df_mse$mean_MSE == min_MSE_df)]<- 1
  
  print_Df_mse_long<-print_Df_mse%>% gather(quant, values, -algorithm, -best_mse)
  print_Df_mse_long<- as.data.frame(print_Df_mse_long)
  print_Df_mse_long$algorithm<-unlist(print_Df_mse_long$algorithm)
  print_Df_mse_long$quant<-unlist(print_Df_mse_long$quant)
  print_Df_mse_long$values<-unlist(print_Df_mse_long$values)
  
  print_Df_mse_long<-print_Df_mse_long[order(print_Df_mse_long$algorithm),]
  
  
  
  min_y<-round(min(print_Df_mse_long$values), digits = 3)
  max_y<-round(max(print_Df_mse_long$values), digits = 3)
  
  
  
  ggplot(data = print_Df_mse_long, mapping =  aes(x = reorder(algorithm, -values ) , y = values, shape = quant , color = best_mse)) + 
    geom_point()+
    scale_x_discrete()+
    scale_y_continuous(name = "mean_MSE/mean_R2",breaks=seq(from = round(min_y, digits = 2), to = round(max_y, digits = 2)+ round((max_y-min_y)/20, digits = 3),by = round((max_y-min_y)/20, digits = 3)))#+
  #scale_shape_identity()
  
}




best_models_df_R2<-split_up_list_build_df(best_mse_models_all_algorithms_r2) %>% arrange(desc(mean_R2))

#####print(smalles r2 per algo)
{
  print_Df_R2<-best_models_df_R2%>% select(mean_MSE, mean_R2, algorithm)
  
  sorting_R2<-best_models_df_R2%>% select(algorithm,mean_R2)%>% arrange(desc(mean_R2))%>% rownames()
  
  print_Df_R2<-print_Df_R2[sorting_R2,]
  
  
  print_Df_R2$best_R2 <- 0
  max_R2_df <-max(print_Df_R2$mean_R2)
  print_Df_R2$best_R2[which(print_Df_R2$mean_R2 == max_R2_df)]<- 1
  
  print_Df_R2_long<-print_Df_R2%>% gather(quant, values, -algorithm, -best_R2)
  print_Df_R2_long<- as.data.frame(print_Df_R2_long)
  print_Df_R2_long$algorithm<-unlist(print_Df_R2_long$algorithm)
  print_Df_R2_long$quant<-unlist(print_Df_R2_long$quant)
  print_Df_R2_long$values<-unlist(print_Df_R2_long$values)
  
  print_Df_R2_long<-print_Df_R2_long[order(print_Df_R2_long$algorithm),]
  
  
  
  min_y<-round(min(print_Df_R2_long$values), digits = 3)
  max_y<-round(max(print_Df_R2_long$values), digits = 3)
  
  
  
  ggplot(data = print_Df_R2_long, mapping =  aes(x = reorder(algorithm, -values ) , y = values, shape = quant , color = best_R2)) + 
    geom_point()+
    scale_x_discrete()+
    scale_y_continuous(name = "mean_MSE/mean_R2",breaks=seq(from = round(min_y, digits = 2), to = round(max_y, digits = 2)+ round((max_y-min_y)/20, digits = 3),by = round((max_y-min_y)/20, digits = 3)))#+
  #scale_shape_identity()
  
}

view(best_models_df_mse)
write_csv(x = best_models_df_mse, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/Ruhr_best_mse_models.csv", )
write_csv(x = best_models_df_R2, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/Ruhr_best_r2_models.csv")


best_models_df_mse$Formula[1]
#mse
best_models_df_mse_algo_kpis<-best_models_df_mse%>% select(algorithm,mean_MSE, mean_R2) %>% arrange(mean_MSE)

#r2
#in results
best_models_df_r2_algo_kpis<-best_models_df_R2%>% select(algorithm,mean_MSE, mean_R2) %>% arrange(desc(mean_R2))
write_csv(x = best_models_df_mse_algo_kpis, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/best_models_df_mse_algo_kpis.csv")
write_csv(x = best_models_df_r2_algo_kpis, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/best_models_df_R2_algo_kpis.csv")


best_models_df_R2$Formula[1]

# check which were the most selected features!
{
  get_unique_formulas_from_best_model_df <- function(best_models_df){
    #best_models_df<-best_models_df_R2
    all_formulas_list<-lapply(best_models_df$Formula, function (x) str_split(string = x, pattern = " \\~ ")[[1]][2])
    all_formulas_list<-all_formulas_list[!is.na(all_formulas_list)]
    all_formulas_list<-unlist(all_formulas_list)
    all_features_per_formula_list<-(lapply(all_formulas_list, function(x) unlist(str_split(string = x, pattern = " \\+ "))))
    unique_found_formulas_here<- unique(unlist(all_features_per_formula_list))
    return(list(unique_found_formulas_here, all_formulas_list))
  }
}
unique_found_features_and_all_formulas_mse<-get_unique_formulas_from_best_model_df(best_models_df_mse)
unique_found_features_and_all_formulas_r2<-get_unique_formulas_from_best_model_df(best_models_df_R2)

build_featureoccurence_frame <- function(unique_found_features_and_all_formulas){
  #unique_found_features_and_all_formulas<- unique_found_features_and_all_formulas_mse
  unique_found_formulas<-unique_found_features_and_all_formulas[[1]]
  all_formulas_list<-unique_found_features_and_all_formulas[[2]]
  #all_Formulas_in_a_list <- all_formulas_list
  dummy_df<-as.data.frame((1:length(unique_found_formulas))%>% t())
  names(dummy_df) <-unique_found_formulas
  
  
  
  new_rowl<-rep(0, length(unique_found_formulas))  
  for(i in 1:length(all_formulas_list)){
    dummy_df<-rbind(dummy_df,new_rowl)
  }
  dummy_df<- dummy_df[-1,]
  dummy_df$Formula<-all_formulas_list
  
  
  for (row_index in 1: nrow(dummy_df)) {
    #row_index<-1
    list_features<-unlist(str_split(string = dummy_df$Formula[row_index], pattern = " \\+ "))
    
    for (feature in list_features) {
      #feature<-list_features[1]
      col_indx<-which(names(dummy_df)==feature)
      dummy_df[row_index,col_indx]<- 1
      
      
    }
  }
  #models with smallest mses or highest r2
  colsum_dummy_df<-dummy_df%>% select(-Formula) %>%colSums()
  colsum_dummy_df$Formula <- "-"
  dummy_df<-rbind(dummy_df,colsum_dummy_df)
  
  sorted_colnames_for_resorting<- names(sort(decreasing = T,dummy_df[nrow(dummy_df),]))
  
  dummy_df<-dummy_df[nrow(dummy_df),]%>% select(sorted_colnames_for_resorting)
  dummy_df<-dummy_df[,-ncol(dummy_df)]
  dummy_df<-(dummy_df/length(all_formulas_list))
  dummy_df<-round(dummy_df, digits = 3)%>% t()
  dummy_df<- as.data.frame(dummy_df)
  dummy_df<-cbind(rownames(dummy_df), dummy_df) 
  return(dummy_df)
}
#in results

features_occurence_mse<-build_featureoccurence_frame(unique_found_features_and_all_formulas_mse)
features_occurence_r2<-build_featureoccurence_frame(unique_found_features_and_all_formulas_r2)

#in results
write_csv(x = features_occurence_mse, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/Ruhr_features_best_mse_models.csv", )

write_csv(x = features_occurence_r2, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/Ruhr_features_best_r2_models.csv", )



view(dummy_df)

#retrained best model_mse


#get formula of smalles mse
best_formula_mse<-best_models_df_mse %>% select(algorithm, Formula, mean_MSE)%>% arrange(mean_MSE) %>% filter(mean_MSE == min(best_models_df_mse$mean_MSE))
best_formula_R2<-best_models_df_R2 %>% select(algorithm, Formula, mean_R2)%>% arrange(desc(mean_R2)) %>% filter(mean_R2 == max(best_models_df_R2$mean_R2))


ruhr_all_data_scaled<-scale(ruhr_all_data)


ruhr_all_data_scaled_df<-as.data.frame(ruhr_all_data_scaled)

best_linear_model_mse<-lm(formula(best_formula_mse$Formula), ruhr_all_data_scaled_df )

best_linear_model_R2<-lm(formula(best_formula_R2$Formula), ruhr_all_data_scaled_df)

coefficients_retrained_best_linear_model_mse<- sort(coef(best_linear_model_mse), decreasing = T) 
coefficients_retrained_best_linear_model_R2<- sort(coef(best_linear_model_R2), decreasing = T) 

coefficients_retrained_best_linear_model_mse<-as.data.frame(coefficients_retrained_best_linear_model_mse)%>% arrange(desc(coefficients_retrained_best_linear_model_mse))
coefficients_retrained_best_linear_model_mse<-cbind(rownames(coefficients_retrained_best_linear_model_mse),coefficients_retrained_best_linear_model_mse)
names(coefficients_retrained_best_linear_model_mse)<- c("feature","coef")

coefficients_retrained_best_linear_model_mse$coef <- round(coefficients_retrained_best_linear_model_mse$coef,digits = 3)

coefficients_retrained_best_linear_model_R2<-as.data.frame(coefficients_retrained_best_linear_model_R2)%>% arrange(desc(coefficients_retrained_best_linear_model_R2))
coefficients_retrained_best_linear_model_R2<-cbind(rownames(coefficients_retrained_best_linear_model_R2),coefficients_retrained_best_linear_model_R2)
names(coefficients_retrained_best_linear_model_R2)<- c("feature","coef")
coefficients_retrained_best_linear_model_R2$coef <- round(coefficients_retrained_best_linear_model_R2$coef,digits = 3)


#in results
write_csv(x = coefficients_retrained_best_linear_model_mse, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/Ruhr_coefficients_retrained_best_linear_model_mse.csv", )
write_csv(x = coefficients_retrained_best_linear_model_R2, path = "/Users/heiko.langer/Dropbox/1_TUB/Masterarbeit_dropbox/03_Steps/Ergebnisssammlung/Ruhr_coefficients_retrained_best_linear_model_R2.csv", )




library(interactions)
library(jtools)
library(sandwich)
summ(best_linear_model_mse)
{
  get_features_that_are_bothj_in_interaction_and_solo<-function(coefficients_retrained_best_linear_model){
    #coefficients_retrained_best_linear_model<-coefficients_retrained_best_linear_model_R2
    interaction_terms<-coefficients_retrained_best_linear_model$feature[unlist(lapply(coefficients_retrained_best_linear_model$feature,FUN = grepl, pattern=":"))]
    non_interaction_terms<-coefficients_retrained_best_linear_model$feature[!unlist(lapply(coefficients_retrained_best_linear_model$feature,FUN = grepl, pattern=":"))]
    feature_solo_and_in_interaction <- list()
    for( i in non_interaction_terms[-1]){
      
      if(any(unlist(lapply(interaction_terms, FUN = grepl, pattern= i)))){
        feature_solo_and_in_interaction <- append(feature_solo_and_in_interaction,i)  
      }
      
    }
    
    
    return(unlist(feature_solo_and_in_interaction))  
  }  
}

#interpret interactions!!
interaction_features_and_solo<-get_features_that_are_bothj_in_interaction_and_solo(coefficients_retrained_best_linear_model_mse)

min(ruhr_all_data_scaled_df$ka_mean_mean_12)

hist(ruhr_all_data_scaled_df$ka_mean_mean_12)

seq(min(ruhr_all_data_scaled_df$ka_mean_mean_12),max(ruhr_all_data_scaled_df$ka_mean_mean_12),length.out = 6)
seq(min(ruhr_all_data$ka_mean_mean_12),max(ruhr_all_data$ka_mean_mean_12), length.out = 6)
max(ruhr_all_data_scaled_df$ka_mean_mean_12)
print(sim_slopes(best_linear_model_mse,r_mean_mean_23,ka_mean_mean_12  ))
interact_plot(best_linear_model_mse, r_mean_mean_23,ka_mean_mean_12, 
              modx.values = seq(min(ruhr_all_data_scaled_df$ka_mean_mean_12),max(ruhr_all_data_scaled_df$ka_mean_mean_12)) ,modx.labels =  seq(min(ruhr_all_data$ka_mean_mean_12),max(ruhr_all_data$ka_mean_mean_12), length.out = 6))
min(ruhr_all_data$ka_mean_mean_12)
print(sim_slopes(best_linear_model_mse,ka_mean_mean_12,r_mean_mean_23  ))
interact_plot(best_linear_model_mse, ka_mean_mean_12,r_mean_mean_23 )



coefficients_retrained_best_linear_model_mse


print(sim_slopes(best_linear_model_mse,r_mean_mean_12, r_mean_mean_345 ))
interact_plot(best_linear_model_mse,r_mean_mean_12, r_mean_mean_345 )
interact_plot(best_linear_model_mse,r_mean_mean_12, r_mean_mean_345 , plot.points = T)
interact_plot(best_linear_model_mse,r_mean_mean_12, r_mean_mean_345 , plot.points = T, interval = T)







colsum_dummy_df
unlist(str_split(string = dummy_df$Formula, pattern = " \\+ "))













#print_Df_mse_long$algorithm <- as.character(print_Df_mse_long$algorithm )


ggplot(print_Df_mse_long, aes(x = value, y =  value)) 









#scale_y_continuous(breaks=seq(from = 0, to = max_y,by = round(max_y/20,2)))







