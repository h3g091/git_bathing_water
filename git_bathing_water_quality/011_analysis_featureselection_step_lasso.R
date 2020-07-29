


{#instancing a dataframe with all possible variable names as colnames 
  build_rename_cols_to_variable_names <- function(df_with_all_vars){
    df<-data.frame(matrix(0, ncol = length((colnames(df_with_all_vars)))+2, nrow = 1))
    colnames(df)<- colnames(df_with_all_vars)
    names(df)[length(names(df))-1]<-"formel" 
    names(df)[length(names(df))]<-"iteration_number" 
    
    
    return(df)
  }
}

{
  build_correlation_table_e.coli_all_features <- function(iteration_river_list_data1, river_number){
    iteration_river_list_data1<- iteration_river_list_data
    df_all_data_with_interactions<-as.data.frame(scale(cbind(iteration_river_list_data1[[1]][[river_number]],interact.data(iteration_river_list_data1[[1]][[river_number]][-1]))))  
    correlation_df_log_e.coli<-as.data.frame(cor(df_all_data_with_interactions))[1]%>%arrange(desc(log_e.coli))
    return(correlation_df_log_e.coli)
  }
}

#occurence table river_havel 
correlation_df_log_e.coli_all_features_havel<-build_correlation_table_e.coli_all_features(iteration_river_list_data, 1)
correlation_df_log_e.coli_all_features_havel<-build_correlation_table_e.coli_all_features(iteration_river_list_data, 2)

#correlation table for all features with e.coli
correlation_df_log_e.coli<-cor(df_all_data_with_interactions)[1]%>%arrange(desc(log_e.coli))

barplot(correlation_df_log_e.coli$log_e.coli)

{#checks which variables were selected for step 
  get_new_iteration_row<-function(df_save,var_list, iteration_numb){
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
    #df_save<-havel_lasso_model_selection_first_iteration_first_model_coef
    #var_list<- unique_coef_lambda_min_havel_lasso_model_selection_first_iteration_first_model
    #iteration_numb<-1
    
    o<-var_list #adds the formula
    
    if(all(sapply(var_list, is.character))==F){
      o<- o[[2]][1]
    }
    
    new_row <- data.frame(matrix(0, ncol = length((colnames(df_with_all_variable_names)))+2, nrow = 1))
    colnames(new_row)<- colnames(df_with_all_variable_names)
    new_row[ncol(new_row)-1] <- o
    names(new_row)[length(names(new_row))-1]<-"formel"
    new_row[ncol(new_row)] <- iteration_numb
    names(new_row)[length(names(new_row))]<-"iteration_number"
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

{
  get_occurences_glmnet_feature_selection_all_columns<-function(df_x_coef,unique_glmnet_formulas_coef_x, iteration_numb){
    #df_x_coef<-havel_lasso_model_selection_first_iteration_first_model_coef
    #unique_glmnet_formulas_coef_x<- unique_coef_lambda_min_havel_lasso_model_selection_first_iteration_first_model
    if (length(unique_glmnet_formulas_coef_x)==0) {
      u<- data.frame()
    }else{
      for (idx in 1: length(unique_glmnet_formulas_coef_x)) {
        #idx<-1
        new_row_glmnet_df_x_coef<-get_new_iteration_row(df_x_coef,unique_glmnet_formulas_coef_x[idx], iteration_numb)
        df_x_coef<-rbind(df_x_coef,new_row_glmnet_df_x_coef)
      }
      u<-df_x_coef[-1,]# %>% select_if(~ !is.numeric(.) || sum(.) != 0) 
    }
    
    u$n_selected_variables <- str_count(unique_glmnet_formulas_coef_x, pattern = " \\+ ")+1
    
    return(u)
  }
}

{
  paste_together_coef_as_string <- function(list_of_variable_names){
  #  list_of_variable_names <- coef_lambda_min_havel_lasso_model_selection_first_iteration_first_model$Coefficient[-1]
  paste(list_of_variable_names
        ,collapse=" + ")
  }
}

havel_data_first_iteration <- saved_iteration_river_list_data[[1]][[1]]



full_1 <- lm(log_e.coli ~ .^2, data = havel_data_first_iteration) #only for df_with_all_variable_names
df_with_all_variable_names <- model.matrix(full_1, havel_data_first_iteration)


##########
#rbind(havel_lasso_model_selection_first_iteration_first_model_coef_save, havel_lasso_model_selection_first_iteration_second_model_coef_save)%>% select_if(~ !is.numeric(.) || sum(.) != 0) 
#occurence function glmnet
{
build_occurence_table_glmnet_all_5_folds_same_iteration <- function(saved_iteration_river_list_glmnet_Model, river_numb, iteration_numb, lambda_value){
  #river_numb<- 1
  #iteration_numb<-1
  #saved_iteration_river_list_glmnet_Model<-saved_iteration_river_list_lasso_Model
  #lambda_value <- "lambda.1se"
 
  model_list_all_folds<-saved_iteration_river_list_glmnet_Model[[iteration_numb]][[river_numb]]
  dummy_df<-data.frame(matrix(0, ncol = length((colnames(df_with_all_variable_names)))+3, nrow = 1))
  colnames(dummy_df)<- colnames(df_with_all_variable_names)
  names(dummy_df)[length(names(dummy_df))-2]<-"formel" 
  names(dummy_df)[length(names(dummy_df))-1]<-"iteration_number" 
  names(dummy_df)[length(names(dummy_df))]<-"n_selected_variables"   
    
  for (fold_idx in 1:length(model_list_all_folds)) {
    #fold_idx <-3
    extracted_coefficients<- extract.coef(model_list_all_folds[[fold_idx]], lambda = lambda_value)
    if(nrow(extracted_coefficients)==1){
      new_row<-build_rename_cols_to_variable_names(df_with_all_variable_names)
      new_row$n_selected_variables <- 0
      new_row$iteration_number <- iteration_numb
    }else{
    coef_pasted_together<-paste_together_coef_as_string(extracted_coefficients$Coefficient[-1])
    
    new_row<- get_occurences_glmnet_feature_selection_all_columns(build_rename_cols_to_variable_names(df_with_all_variable_names), coef_pasted_together,iteration_numb = iteration_numb)
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
{ #ocurence formula step
  build_occurence_table_step_all_5_folds_same_iteration <- function(saved_iteration_river_list_step_Model, river_numb, iteration_numb){
    #river_numb<- 1
    #iteration_numb<-1
    #saved_iteration_river_list_step_Model<-saved_iteration_river_list_selection_bic
    
    #lambda_value <- "lambda.1se"
    
    model_list_all_folds<-saved_iteration_river_list_step_Model[[iteration_numb]][[river_numb]]
    #model_list_all_folds1<-saved_iteration_river_list_glmnet_Model1[[iteration_numb]][[river_numb]]
    dummy_df<-data.frame(matrix(0, ncol = length((colnames(df_with_all_variable_names)))+3, nrow = 1))
    colnames(dummy_df)<- colnames(df_with_all_variable_names)
    names(dummy_df)[length(names(dummy_df))-2]<-"formel" 
    names(dummy_df)[length(names(dummy_df))-1]<-"iteration_number" 
    names(dummy_df)[length(names(dummy_df))]<-"n_selected_variables"   
    
    for (fold_idx in 1:length(model_list_all_folds)) {
      #fold_idx <-1
      extracted_coefficients<-coef(model_list_all_folds[[fold_idx]])
      #extracted_coefficients1<- extract.coef(model_list_all_folds1[[fold_idx]], lambda = lambda_value)
      if(length(extracted_coefficients)==1){
        new_row<-build_rename_cols_to_variable_names(df_with_all_variable_names)
        new_row$n_selected_variables <- 0
        new_row$iteration_number <- iteration_numb
      }else{
        coef_pasted_together<-paste(names(extracted_coefficients[-1]), collapse = " + ")
        
        new_row<- get_occurences_glmnet_feature_selection_all_columns(build_rename_cols_to_variable_names(df_with_all_variable_names), coef_pasted_together,iteration_numb = iteration_numb)
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


build_occurence_table_glmnet_all_5_folds_same_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1 , iteration_numb = 1, lambda_value = "lambda.min")[[1]]
build_occurence_table_glmnet_all_5_folds_same_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1 , iteration_numb = 1, lambda_value = "lambda.1se")[[1]]



#occurence table all iterations
{
build_occurence_table_glmnet_all_iteration <- function(saved_iteration_river_list_glmnet_Model, river_numb,  lambda_value, iterations, whole_table){
  #iterations <- 15
  #saved_iteration_river_list_glmnet_Model<-saved_iteration_river_list_lasso_Model 
  #river_numb <- 1
  #lambda_value = "lambda.1se"  
  #whole_table <- F
  
  
  first_iteration_occurences <- build_occurence_table_glmnet_all_5_folds_same_iteration(saved_iteration_river_list_glmnet_Model, river_numb , iteration_numb=1, lambda_value)[[2]]
  for (iteration_index in 2:iterations) {
    #iteration_index <- 2
    first_iteration_occurences<-rbind(first_iteration_occurences,build_occurence_table_glmnet_all_5_folds_same_iteration(saved_iteration_river_list_glmnet_Model, river_numb , iteration_numb = iteration_index, lambda_value)[[2]])
  }
  
  if(whole_table==T){
    final_df<-first_iteration_occurences
  }else if(whole_table==F){
    final_df<-first_iteration_occurences%>% select_if(~ !is.numeric(.) || sum(.) != 0) %>%arrange(n_selected_variables)
  }else{print("want to have table with all variable?")}
  
  
  return(final_df)
}
}


{
  build_occurence_table_step_all_iteration <- function(saved_iteration_river_list_glmnet_Model, river_numb,   iterations, whole_table){
    #iterations <- 15
    
    #saved_iteration_river_list_glmnet_Model<-saved_iteration_river_list_selection_bic
    #river_numb <- 1
    #lambda_value = "lambda.1se"  
    #whole_table <- F
    
    
    first_iteration_occurences <- build_occurence_table_step_all_5_folds_same_iteration(saved_iteration_river_list_glmnet_Model, river_numb , iteration_numb=1)[[2]]
    for (iteration_index in 2:iterations) {
      #iteration_index <- 2
      first_iteration_occurences<-rbind(first_iteration_occurences,build_occurence_table_step_all_5_folds_same_iteration(saved_iteration_river_list_glmnet_Model, river_numb , iteration_numb = iteration_index)[[2]])
    }
    
    if(whole_table==T){
      final_df<-first_iteration_occurences
    }else if(whole_table==F){
      final_df<-first_iteration_occurences%>% select_if(~ !is.numeric(.) || sum(.) != 0) %>%arrange(n_selected_variables)
    }else{print("want to have table with all variable?")}
    
    
    return(final_df)
  }
}


#####


analysis_feature_selection_havel_lasso_lambda_1se<-build_occurence_table_glmnet_all_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1, lambda_value = "lambda.1se", iterations = 15, whole_table = F)
analysis_feature_selection_havel_lasso_lambda_min<-build_occurence_table_glmnet_all_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1, lambda_value = "lambda.min", iterations = 15, whole_table = F)

analysis_feature_selection_havel_step_aic<-build_occurence_table_step_all_iteration(saved_iteration_river_list_selection_aic, river_numb = 1, iterations = 15, whole_table = F)
analysis_feature_selection_havel_step_bic<-build_occurence_table_step_all_iteration(saved_iteration_river_list_selection_bic, river_numb = 1, iterations = 15, whole_table = F)

#comparison -- check if they found an identical formula
unique_havel_lasso_lambda_min<-(unique(analysis_feature_selection_havel_lasso_lambda_min$formel))
unique_havel_lasso_lambda_1se<-(unique(analysis_feature_selection_havel_lasso_lambda_1se$formel))
unique_havel_step_aic<-(unique(analysis_feature_selection_havel_step_aic$formel))
unique_havel_step_bic<-(unique(analysis_feature_selection_havel_step_bic$formel))


analysis_feature_selection_havel_lasso_lambda_1se%>%select(formel, n_selected_variables)%>% filter(n_selected_variables <5)

intersect(unique_havel_step_bic,unique_havel_step_aic)
intersect(unique_havel_lasso_lambda_min,unique_havel_lasso_lambda_1se)

intersect(unique_havel_step_aic,unique_havel_lasso_lambda_min)
intersect(unique_havel_step_aic,unique_havel_lasso_lambda_1se)
intersect(unique_havel_step_bic,unique_havel_lasso_lambda_min)
intersect(unique_havel_step_bic,unique_havel_lasso_lambda_1se)



n_data_points_havel<- nrow(iteration_river_list_data[[1]][[1]])
        
#plotting histogram- number of features selected --> now which are the most selected features and formulas


par(mfrow=c(1,1))

{
get_algoname_for_plot_title<-function(analysis_feature_selection_algo_temp){
  algo_name<-deparse(substitute(analysis_feature_selection_algo_temp))
  if(grepl("step_aic",algo_name) ){
    algo_name<- "step_aic"
  }else if(grepl("step_bic",algo_name) ){
    algo_name<- "step_bic"
  }else if(grepl("lasso_lambda_min",algo_name) ){
    algo_name<- "lasso_lambda_min"
  }else if(grepl("lasso_lambda_1se",algo_name) ){
    algo_name<- "lasso_lambda_1se"
  }else if(grepl("rf",algo_name) ){
    algo_name<- "rf"
  }
  return(algo_name)
}
}


#ggplot(analysis_feature_selection_lambda_1se, aes(x=n_selected_variables)) + geom_histogram(binwidth = 1,colour="black", fill="white") + stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1)+scale_x_continuous(breaks= round(seq(0, max(analysis_feature_selection_lambda_1se$n_selected_variables), by = 1),1) )

{
plot_analysis_table_n_feature<- function(analysis_feature_selection_algo, n_data_points, title_name){
  #analysis_feature_selection_algo<-analysis_feature_selection_lambda_1se
  #title_name<- algo_name1
  #ggplot(analysis_feature_selection_algo, aes(x=n_selected_variables)) + geom_histogram(binwidth = 1,colour="black", fill="white") + stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1) + scale_x_continuous(breaks= round(seq(0, max(analysis_feature_selection_algo$n_selected_variables), by = 1),1) )
  algo_name<- title_name
  annotation_label<-paste("Number of datapoints:", n_data_points, sep = " ")
  max_features_formula<-max(analysis_feature_selection_algo$n_selected_variables)
  x_axis_labels <- 0:max_features_formula
  ymax<-max(table(analysis_feature_selection_algo$n_selected_variables))
  
  plot_temp<-
    ggplot(analysis_feature_selection_algo, aes(x=n_selected_variables)) + 
    geom_histogram(binwidth = 0.5,colour="black", fill="white") + 
    stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1) + 
    scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+#, limits = x_axis_labels) + 
    annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label) + 
    ggtitle(algo_name)
    
  
  return(plot_temp)
}
}
{
plot_horizontal_boxplot<- function(analysis_feature_selection_river_algo, title_name){
  #analysis_feature_selection_river_algo<-analysis_feature_selection_havel_lasso_lambda_1se
  #title_name <- algo_name1
  max_features_formula<-max(analysis_feature_selection_river_algo$n_selected_variables)
  y_axis_labels <- 0:max_features_formula
  plot_temp<-ggplot(analysis_feature_selection_river_algo, aes( y=n_selected_variables)) + 
    geom_boxplot()+
    scale_y_continuous(labels = y_axis_labels, breaks = y_axis_labels) + 
    #annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label) + 
    ggtitle(title_name)+
    coord_flip()
  
  return(plot_temp)
}
}
  plot_horizontal_boxplot(analysis_feature_selection_havel_lasso_lambda_1se,algo_name1)
  

  
  
  
{
plot_analysis_table_n_feature_2_plots<- function(analysis_feature_selection_algo1,analysis_feature_selection_algo2,  n_data_points, title_name_up, title_name_down){
  #analysis_feature_selection_algo1<-analysis_feature_selection_lambda_min
  #analysis_feature_selection_algo2<-analysis_feature_selection_lambda_1se
  #ggplot(analysis_feature_selection_algo, aes(x=n_selected_variables)) + geom_histogram(binwidth = 1,colour="black", fill="white") + stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1) + scale_x_continuous(breaks= round(seq(0, max(analysis_feature_selection_algo$n_selected_variables), by = 1),1) )
  #n_data_points<- n_data_points_havel
 
  
  
  annotation_label<-paste("Number of datapoints:", n_data_points, sep = " ")
  
  max_features_formula<-max(analysis_feature_selection_algo1$n_selected_variables, analysis_feature_selection_algo2$n_selected_variables)
  ymax<-max(max(table(analysis_feature_selection_algo1$n_selected_variables)),max(table(analysis_feature_selection_algo2$n_selected_variables)))
  x_axis_labels <- 0:max_features_formula
  
  a<-ggplot(analysis_feature_selection_algo1, aes(x=n_selected_variables)) + 
    geom_histogram(binwidth = 1,colour="black", fill="white") + 
    stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1) + 
    scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) + 
    expand_limits(x=c(0,max_features_formula))+ 
    annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label)+ #, limits =c(0,max_features_formula) )# + xlim(c(0,max_features_formula))
    ggtitle(title_name_up)
  b<-ggplot(analysis_feature_selection_algo2, aes(x=n_selected_variables)) + 
    geom_histogram(binwidth = 1,colour="black", fill="white") + 
    stat_bin(aes(y=..count.. + 0.5, label=..count..), geom="text", binwidth=1) + 
    scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+expand_limits(x=c(0,max_features_formula))+ 
    annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label)+
    ggtitle(title_name_down)
  
  ggarrange(a,b, ncol = 1, nrow = 2)
}
}

{ #bar und boxplot length features
        algo_name_lasso_lambda_1se<-get_algoname_for_plot_title(analysis_feature_selection_havel_lasso_lambda_1se)
        lambda_1se_barplot_havel<-plot_analysis_table_n_feature(analysis_feature_selection_havel_lasso_lambda_1se, n_data_points = n_data_points_havel, title_name = algo_name_lasso_lambda_1se)
        lambda_1se_boxplot_havel<-plot_horizontal_boxplot(analysis_feature_selection_havel_lasso_lambda_1se,algo_name_lasso_lambda_1se)
        #mean calculation
        mean(analysis_feature_selection_havel_lasso_lambda_1se$n_selected_variables)
        #lasso lambda 1se
        ggarrange(plot_horizontal_boxplot(analysis_feature_selection_havel_lasso_lambda_1se,algo_name_lasso_lambda_1se),lambda_1se_barplot_havel, ncol = 1, nrow = 2)
        
        
        
        #lasso lambda min
        algo_name_lasso_lambda_min<-get_algoname_for_plot_title(analysis_feature_selection_havel_lasso_lambda_min)
        lasso_lambda_min_barplot_havel<-plot_analysis_table_n_feature(analysis_feature_selection_havel_lasso_lambda_min, n_data_points = n_data_points_havel, title_name = algo_name_lasso_lambda_min)
        lasso_lambda_min_boxplot_havel<-plot_horizontal_boxplot(analysis_feature_selection_havel_lasso_lambda_min,algo_name_lasso_lambda_min)
        #mean calculation
        mean(analysis_feature_selection_havel_lasso_lambda_min$n_selected_variables)
        
        
        ggarrange(plot_horizontal_boxplot(analysis_feature_selection_havel_lasso_lambda_min,algo_name_lasso_lambda_min),lasso_lambda_min_barplot_havel, ncol = 1, nrow = 2)
        
        #step_aic
        algo_name_step_aic<-get_algoname_for_plot_title(analysis_feature_selection_havel_step_aic)
        step_aic_barplot_havel<-plot_analysis_table_n_feature(analysis_feature_selection_havel_step_aic, n_data_points = n_data_points_havel, title_name = algo_name_step_aic)
        step_aic__boxplot_havel<-plot_horizontal_boxplot(analysis_feature_selection_havel_step_aic,algo_name_step_aic)
        #mean calculation
        mean(analysis_feature_selection_havel_step_aic$n_selected_variables)
        
        
        ggarrange(plot_horizontal_boxplot(analysis_feature_selection_havel_step_aic,algo_name_step_aic),step_aic_barplot_havel, ncol = 1, nrow = 2)
        
        #step_bic
        algo_name_step_bic<-get_algoname_for_plot_title(analysis_feature_selection_havel_step_bic)
        step_bic_barplot_havel<-plot_analysis_table_n_feature(analysis_feature_selection_havel_step_bic, n_data_points = n_data_points_havel, title_name = algo_name_step_bic)
        step_bic__boxplot_havel<-plot_horizontal_boxplot(analysis_feature_selection_havel_step_bic,algo_name_step_bic)
        #mean calculation
        mean(analysis_feature_selection_havel_step_bic$n_selected_variables)
        
        ggarrange(plot_horizontal_boxplot(analysis_feature_selection_havel_step_bic,algo_name_step_bic),step_bic_barplot_havel, ncol = 1, nrow = 2)
        
        
}
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
algo_names <- as.data.frame(c("Step_AIC","Step_BIC","Lasso_min","Lasso_1se"))


build_dataframe_row_comparison_box   <- function(analysis_feature_selection_river_algo){
    #analysis_feature_selection_river_algo<-analysis_feature_selection_havel_step_bic
    
    b <-  mean(analysis_feature_selection_river_algo$n_selected_variables)
    c <- median(analysis_feature_selection_river_algo$n_selected_variables)
    d <- min(analysis_feature_selection_river_algo$n_selected_variables)
    e <- max(analysis_feature_selection_river_algo$n_selected_variables)
    f <- getmode(analysis_feature_selection_river_algo$n_selected_variables)
    g<-quantile(analysis_feature_selection_river_algo$n_selected_variables)[2]
    h<-quantile(analysis_feature_selection_river_algo$n_selected_variables)[4]
  t<- as.data.frame(list(b,c,d,e,f,g,h))    
  names(t) <- c("mean", "median", "min", "max", "modal", "25%", "75%")
  return(t)
}

a<-build_dataframe_row_comparison_box(analysis_feature_selection_havel_lambda_min)
b<-build_dataframe_row_comparison_box(analysis_feature_selection_havel_lambda_1se)
c<-build_dataframe_row_comparison_box(analysis_feature_selection_havel_step_aic)
d<-build_dataframe_row_comparison_box(analysis_feature_selection_havel_step_bic)

comparison_df<-rbind(a,b,c,d)
rownames(comparison_df) <- c("lasso_min","lasso_1se", "step_aic", "step_bic")
comparison_df<-comparison_df%>% arrange(mean)
{  #standard boxplot not combineable with ggplot
plot_analysis_table_n_features_boxplot <- function(analysis_feature_selection_algo,title_name){
  #analysis_feature_selection_algo<-analysis_feature_selection_havel_lasso_lambda_1se
  #title_name<- algo_name2
  
  boxplot(analysis_feature_selection_algo$n_selected_variables,yaxt = "n", ylab = "n_features/formula", main= title_name)
  axis(2, at = seq(0, max(analysis_feature_selection_algo$n_selected_variables), 1), las = 2)
  
    
}
}

plot_analysis_table_n_features_boxplot(analysis_feature_selection_havel_lasso_lambda_1se, algo_name1)
plot_analysis_table_n_features_boxplot(analysis_feature_selection_havel_lasso_lambda_min, algo_name_lasso_lambda_min)


#boxplot(analysis_feature_selection_havel_lasso_lambda_1se$n_selected_variables,yaxt = "n", ylab = "n_features/formula", main= algo_name1)
#axis(2, at = seq(0, max(analysis_feature_selection_havel_lasso_lambda_1se$n_selected_variables), 1), las = 2)

{
  build_most_selected_formula_table<- function(analysis_feature_selection_river_algo){
    #analysis_feature_selection_river_algo<-analysis_feature_selection_havel_lasso_lambda_1se
    occurence_table<-as.data.frame(table(analysis_feature_selection_river_algo$formel))
    occurence_table<-cbind(occurence_table,as.data.frame(sapply(occurence_table$Var1, str_count, "\\+")+1))
    colnames(occurence_table) <- c("Formula", "Freq", "n_features")
    occurence_table[occurence_table$Formula==0,3]<-0
    occurence_table<-occurence_table%>% arrange(desc(Freq),n_features)
    
    return(occurence_table)
  }
}






most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se<-build_most_selected_formula_table(analysis_feature_selection_havel_lasso_lambda_1se)
most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min<-build_most_selected_formula_table(analysis_feature_selection_havel_lasso_lambda_min)

most_selected_formulas_analysis_feature_selection_havel_step_aic<-build_most_selected_formula_table(analysis_feature_selection_havel_step_aic)
most_selected_formulas_analysis_feature_selection_havel_step_bic<-build_most_selected_formula_table(analysis_feature_selection_havel_step_bic)



{# gives a feeling how important/present an formula is in the data
get_number_of_different_iteration_formula_was_found_in <- function(most_selected_formulas_analysis_feature_selection_river_algo,analysis_feature_selection_river_algo){
  #most_selected_formulas_analysis_feature_selection_river_algo<- most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se
  #analysis_feature_selection_river_algo<- analysis_feature_selection_havel_lasso_lambda_1se
  
  formulas_occured_more_than_once<- most_selected_formulas_analysis_feature_selection_river_algo%>% filter(Freq>1) %>% select(Formula)
  
  formulas_occured_more_than_once <-as.character(unlist(as.list(formulas_occured_more_than_once)))
  df<- as.data.frame(formulas_occured_more_than_once)
  list_n_different_iterations<- list()
  for (indx in 1:length(formulas_occured_more_than_once)) {
    #indx <- 9
    a<-formulas_occured_more_than_once[indx]
    b<-analysis_feature_selection_river_algo$formel==a
    c<-length(unique(analysis_feature_selection_river_algo[b,]$iteration_number))
    list_n_different_iterations<-append(list_n_different_iterations, list(c))
  }
  df$n_different_iterations <- list_n_different_iterations
    
  return(df)
}
}  
    
#lasso
formulas_found_more_than_once_and_in_different_train_test_splits_lasso_lambda_1se<-get_number_of_different_iteration_formula_was_found_in(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se,analysis_feature_selection_havel_lambda_1se)
formulas_found_more_than_once_and_in_different_train_test_splits_lasso_lambda_min<-get_number_of_different_iteration_formula_was_found_in(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min,analysis_feature_selection_havel_lambda_min)
#step
formulas_found_more_than_once_and_in_different_train_test_splits_step_aic<-get_number_of_different_iteration_formula_was_found_in(most_selected_formulas_analysis_feature_selection_havel_step_aic,analysis_feature_selection_havel_step_aic)
formulas_found_more_than_once_and_in_different_train_test_splits_step_bic<-get_number_of_different_iteration_formula_was_found_in(most_selected_formulas_analysis_feature_selection_havel_step_bic,analysis_feature_selection_havel_step_bic)




#maybe cool plot out of this
ggplot(formulas_found_more_than_once_and_in_different_train_test_splits_lasso_lambda_1se, aes(x = formulas_occured_more_than_once)) +
  geom_bar(aes(y=n_different_iterations),stat="identity",alpha=.8,fill='lightblue',color='lightblue4', width = 0.5)+
  scale_x_discrete(label=function(x) stringr::str_trunc(x, 60))+
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(formulas_found_more_than_once_and_in_different_train_test_splits_lasso_lambda_min, aes(x = formulas_occured_more_than_once)) +
  geom_bar(aes(y=n_different_iterations),stat="identity",alpha=.8,fill='lightblue',color='lightblue4', width = 0.5)+
  scale_x_discrete(label=function(x) stringr::str_trunc(x, 60))+
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

{
  plot_full_formula_occurence <- function(most_selected_formulas_analysis_feature_selection_river_algo, filter_for_freq_bigger_1=F){
    #most_selected_formulas_analysis_feature_selection_river_algo<-most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se
    most_selected_formulas_analysis_feature_selection_river_algo$Formula <- gsub("_mean", "", most_selected_formulas_analysis_feature_selection_river_algo$Formula)
    
    
    
    most_selected_formulas_analysis_feature_selection_river_algo$Formula <- factor(most_selected_formulas_analysis_feature_selection_river_algo$Formula, levels = unique(most_selected_formulas_analysis_feature_selection_river_algo$Formula))
    #ggplot(r, aes(x = reorder(Formula,n_features))) +
    ggplot(most_selected_formulas_analysis_feature_selection_river_algo, aes(x = Formula)) +
      geom_bar(aes(y=Freq),stat="identity",alpha=.8,fill='lightblue',color='lightblue4', width = 0.5) +
      geom_point(aes(y=n_features),stat="identity",alpha=0.8,fill='pink',color='black', )+
      
      #scale_x_discrete(labels=addline_format(r$Formula)) +
      #scale_x_discrete(label=abbreviate)+
      scale_x_discrete(label=function(x) stringr::str_trunc(x, 60))+
      theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      ggtitle(paste("Different formulas: ", nrow(most_selected_formulas_analysis_feature_selection_river_algo)))+
      xlab("Unique formulas")+
      ylab("Frequency found formula absolute")+
      scale_y_continuous(breaks=c(1:(max(most_selected_formulas_analysis_feature_selection_river_algo$Freq, most_selected_formulas_analysis_feature_selection_river_algo$n_features)+1)))
    
    
    
    
    
    
    
  }
}





plot_full_formula_occurence(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se)
plot_full_formula_occurence(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min)

plot_full_formula_occurence(most_selected_formulas_analysis_feature_selection_havel_step_aic)
plot_full_formula_occurence(most_selected_formulas_analysis_feature_selection_havel_step_bic)


{
  plot_formula_occurence_bigger_1 <- function(most_selected_formulas_analysis_feature_selection_river_algo, filter_for_freq_bigger_1=T){
    #most_selected_formulas_analysis_feature_selection_river_algo<-most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min
    most_selected_formulas_analysis_feature_selection_river_algo$Formula <- gsub("_mean", "", most_selected_formulas_analysis_feature_selection_river_algo$Formula)
    
    
    most_selected_formulas_analysis_feature_selection_river_algo<-most_selected_formulas_analysis_feature_selection_river_algo%>% filter(Freq>1) 
    
    
    
    most_selected_formulas_analysis_feature_selection_river_algo$Formula <- factor(most_selected_formulas_analysis_feature_selection_river_algo$Formula, levels = unique(most_selected_formulas_analysis_feature_selection_river_algo$Formula))
    ggplot(most_selected_formulas_analysis_feature_selection_river_algo, aes(x =reorder(Formula,-Freq))) +
      geom_bar(aes(y=Freq),stat="identity",alpha=.8,fill='lightblue',color='lightblue4', width = 0.5) +
      geom_point(aes(y=n_features),stat="identity",alpha=0.8,fill='pink',color='black', )+
      
      scale_x_discrete(labels=addline_format(most_selected_formulas_analysis_feature_selection_river_algo$Formula)) +
      #scale_x_discrete(label=abbreviate)+
      theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      xlab("Unique formulas")+
      ylab("Frequency found formula absolute")+
      scale_y_continuous(breaks=c(1:(max(most_selected_formulas_analysis_feature_selection_river_algo$Freq, most_selected_formulas_analysis_feature_selection_river_algo$n_features)+1)) )
    
  }
}


plot_formula_occurence_bigger_1(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se)
plot_formula_occurence_bigger_1(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min)

plot_formula_occurence_bigger_1(most_selected_formulas_analysis_feature_selection_havel_step_aic)
plot_formula_occurence_bigger_1(most_selected_formulas_analysis_feature_selection_havel_step_bic)



{
build_correlation_table_e.coli_all_features <- function(iteration_river_list_data1, river_number){
  iteration_river_list_data1<- iteration_river_list_data
  df_all_data_with_interactions<-as.data.frame(scale(cbind(iteration_river_list_data1[[1]][[river_number]],interact.data(iteration_river_list_data1[[1]][[river_number]][-1]))))  
  correlation_df_log_e.coli<-as.data.frame(cor(df_all_data_with_interactions))[1]%>%arrange(desc(log_e.coli))
  return(correlation_df_log_e.coli)
}
}

#occurence table river_havel 
#build_correlation_table_e.coli_all_features(iteration_river_list_data, 1)

#correlation table for all features with e.coli
#correlation_df_log_e.coli<-cor(df_all_data_with_interactions)[1]%>%arrange(desc(log_e.coli))

#barplot(correlation_df_log_e.coli$log_e.coli)

#view(correlation_df_log_e.coli)
#match(x = "q_mean_mean_12", rownames(correlation_df_log_e.coli))
#match(x = "q_mean_mean_12:q_mean_mean_123", rownames(correlation_df_log_e.coli))
#correlation_df_log_e.coli[match(x = "q_mean_mean_12:q_mean_mean_123", rownames(correlation_df_log_e.coli)),]
{
build_correlation_tables_for_features_in_found_formulas <- function(most_selected_formulas_analysis_feature_selection_river_algo,river_idx){
  #most_selected_formulas_analysis_feature_selection_river_algo<-most_selected_formulas_analysis_feature_selection_havel_step_aic
  #river_idx<-1
  
    list_features_unique_found_formulas<-sapply(most_selected_formulas_analysis_feature_selection_river_algo$Formula, function(x) unlist(str_split(x, pattern = " \\+ ")))
    river_number <- river_idx
    df_all_data_with_interactions<-as.data.frame(scale(cbind(iteration_river_list_data[[1]][[river_number]][-1],interact.data(iteration_river_list_data[[1]][[river_number]][-1]))))
    #for every formula
    list_correlation_matrizes_features_selected_formulas <- list()
    list_correlation_matrix_chosen_interaction_variables <- list()
    list_correlations_selected_features_and_interaction_features <- list()
    for (formula_idx  in 1: length(list_features_unique_found_formulas)) {
      #formula_idx<- 2
      features_formula<-list_features_unique_found_formulas[[formula_idx]]
      
      if (length(features_formula)>1 ){
        if(any(grepl(':',features_formula ))){ #sometimes, step changes order of variables with interaction
          
          
          string1<-features_formula[grepl(':',features_formula )]
          new_string_list<-str_split(string1,":")
          for( i in new_string_list){
            #i<-new_string_list[[1]]
            string2 <- paste(i[2],i[1], sep = ":")
            features_formula <- append(features_formula, string2)
          }

        }
        idx <- match(features_formula, names(df_all_data_with_interactions))
        idx <- idx[!is.na(idx)]
        
        M<-cor(df_all_data_with_interactions[,idx])
        
        #colnames(M) <- gsub("_mean", "", colnames(M))
        #rownames(M) <- gsub("_mean", "", rownames(M))
        
        list_correlation_matrizes_features_selected_formulas <-append(list_correlation_matrizes_features_selected_formulas ,list(M))
       #checks if interaction is even in formula
        if (any(grepl(":", features_formula)==T)) {
         
        
          chosen_interaction_features<-features_formula[grepl(":", features_formula)]
          list_chosen_interaction_features_splitted<-str_split(chosen_interaction_features, pattern = ":")
          
          
          
          chosen_interaction_features_unlisted<-unique(unlist(list_chosen_interaction_features_splitted))
          idx2 <- match(chosen_interaction_features_unlisted, names(df_all_data_with_interactions))
          
          correlation_matrix_chosen_interaction_variables<-cor(df_all_data_with_interactions[,idx2])
          
          
          #check correlations feature interactions
          true_false_df_chosen_interaction_formulas<- correlation_matrix_chosen_interaction_variables==F #build dataframe with only false for selection
          correlation_matrix_chosen_interaction_variables<-as.data.frame(correlation_matrix_chosen_interaction_variables)
         
          for(entry_indx in 1:length(list_chosen_interaction_features_splitted)){
            #entry_indx<-1
            chosen_formula <- list_chosen_interaction_features_splitted[[entry_indx]]
            chosen_formula[1] <- paste("^",chosen_formula[1], "$", sep = "")
            chosen_formula[2] <- paste("^",chosen_formula[2], "$", sep = "")
            true_false_df_chosen_interaction_formulas[grepl(chosen_formula[1], rownames(correlation_matrix_chosen_interaction_variables)),grepl(chosen_formula[2], colnames(correlation_matrix_chosen_interaction_variables))]=T
            #true_false_df_chosen_interaction_formulas[grepl(chosen_formula[1], colnames(correlation_matrix_chosen_interaction_variables)),grepl(chosen_formula[2], rownames(correlation_matrix_chosen_interaction_variables))]=T
            
            
            
                  
          }
          
          correlation_matrix_chosen_interaction_variables<-correlation_matrix_chosen_interaction_variables * true_false_df_chosen_interaction_formulas
          #correlation_matrix_chosen_interaction_variables[correlation_matrix_chosen_interaction_variables==0]<- NA
       } 
        
        colnames(M) <- gsub("_mean", "", colnames(M))
        rownames(M) <- gsub("_mean", "", rownames(M))
        colnames(correlation_matrix_chosen_interaction_variables) <- gsub("_mean", "", colnames(correlation_matrix_chosen_interaction_variables))
        rownames(correlation_matrix_chosen_interaction_variables) <- gsub("_mean", "", rownames(correlation_matrix_chosen_interaction_variables))
        
        both_correlation_matrizes<- list(M,as.matrix(correlation_matrix_chosen_interaction_variables))
        list_correlations_selected_features_and_interaction_features <- append(list_correlations_selected_features_and_interaction_features,list(both_correlation_matrizes) )
        #list_correlation_matrix_chosen_interaction_variables<-append(list_correlation_matrix_chosen_interaction_variables,list(correlation_matrix_chosen_interaction_variables))      
      }else if (length(features_formula)==1 && features_formula!=0 &&any(grepl(":", features_formula))) {
        
          chosen_interaction_features<-features_formula[grepl(":", features_formula)]
          list_chosen_interaction_features_splitted<-str_split(chosen_interaction_features, pattern = ":")
          
          
          
          chosen_interaction_features_unlisted<-unique(unlist(list_chosen_interaction_features_splitted))
          idx2 <- match(chosen_interaction_features_unlisted, names(df_all_data_with_interactions))
          
          correlation_matrix_chosen_interaction_variables<-cor(df_all_data_with_interactions[,idx2])
          
          
          #check correlations feature interactions
          true_false_df_chosen_interaction_formulas<- correlation_matrix_chosen_interaction_variables==F #build dataframe with only false for selection
          correlation_matrix_chosen_interaction_variables<-as.data.frame(correlation_matrix_chosen_interaction_variables)
          for(entry_indx in 1:length(list_chosen_interaction_features_splitted)){
            #entry_indx<-1
            chosen_formula <- list_chosen_interaction_features_splitted[[entry_indx]]
            chosen_formula[1] <- paste("^",chosen_formula[1], "$", sep = "")
            chosen_formula[2] <- paste("^",chosen_formula[2], "$", sep = "")
            true_false_df_chosen_interaction_formulas[grepl(chosen_formula[1], rownames(correlation_matrix_chosen_interaction_variables)),grepl(chosen_formula[2], colnames(correlation_matrix_chosen_interaction_variables))]=T
            #true_false_df_chosen_interaction_formulas[grepl(chosen_formula[1], colnames(correlation_matrix_chosen_interaction_variables)),grepl(chosen_formula[2], rownames(correlation_matrix_chosen_interaction_variables))]=T
            
            
            
            
          }
          
          correlation_matrix_chosen_interaction_variables<-correlation_matrix_chosen_interaction_variables * true_false_df_chosen_interaction_formulas
          #correlation_matrix_chosen_interaction_variables[correlation_matrix_chosen_interaction_variables==0]<- NA
          
          
          colnames(M) <- gsub("_mean", "", colnames(M))
          rownames(M) <- gsub("_mean", "", rownames(M))
          colnames(correlation_matrix_chosen_interaction_variables) <- gsub("_mean", "", colnames(correlation_matrix_chosen_interaction_variables))
          rownames(correlation_matrix_chosen_interaction_variables) <- gsub("_mean", "", rownames(correlation_matrix_chosen_interaction_variables))
          
          both_correlation_matrizes<- list("only 1 feature selected",as.matrix(correlation_matrix_chosen_interaction_variables))
          list_correlations_selected_features_and_interaction_features <- append(list_correlations_selected_features_and_interaction_features,list(both_correlation_matrizes) )
          
          
        
      }else{
        both_correlation_matrizes<- list("only 1 feature selected","only 1 feature selected")
        list_correlations_selected_features_and_interaction_features <- append(list_correlations_selected_features_and_interaction_features,list(both_correlation_matrizes) )
        #list_correlation_matrizes_features_selected_formulas <-append(list_correlation_matrizes_features_selected_formulas ,list("only 1 feature selected"))
        #list_correlation_matrix_chosen_interaction_variables <-append(list_correlation_matrix_chosen_interaction_variables ,list("only 1 feature selected"))
      }
      
      
      
        
      
   
    }
    
   
    return(list_correlations_selected_features_and_interaction_features)    
  
}
}

#table is sorted like the unique_formulas table --> formulas that are more selected than once are at the beginning

correlation_tables_features_in_formula_havel_lasso_min<-build_correlation_tables_for_features_in_found_formulas(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min, 1)

correlation_tables_features_in_formula_havel_lasso_1se<-build_correlation_tables_for_features_in_found_formulas(most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se, 1)

correlation_tables_features_in_formula_havel_step_aic<-build_correlation_tables_for_features_in_found_formulas(most_selected_formulas_analysis_feature_selection_havel_step_aic,1)
correlation_tables_features_in_formula_havel_step_bic<-build_correlation_tables_for_features_in_found_formulas(most_selected_formulas_analysis_feature_selection_havel_step_bic,1)
################
#plot the correlation tables for analysis
#most selected formulas 


{#change "interaction_corr"to True if you want interaction correlation matrix
plot_both_correlation_plots <- function(correlation_tables_features_in_formula_river_algo, formula_idx, interaction_corr =F){
  #correlation_tables_features_in_formula_river_algo<-correlation_tables_features_in_formula_havel_lasso_1se
  #formula_idx<- 2
  #interaction_corr=T
  if(interaction_corr ==F){
    normal_or_interaction_corr  <-1
  }else{
    normal_or_interaction_corr  <-2
  }
  
  corrplot_formula_components<-correlation_tables_features_in_formula_river_algo[[formula_idx]][[normal_or_interaction_corr]]
  #corrplot_interaction_feature_components<-correlation_tables_features_in_formula_river_algo[[formula_idx]][[normal_or_interaction_corr]]
  #par(mfrow = c(1,1))
  #layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))
  if(any(class(corrplot_formula_components)=="matrix")){
    corrplot(corrplot_formula_components, method = "number",order = "hclust",number.digits = 3)#, type = "upper")
  }else{
    print("only 1 formula --> no corr plot possible")
  
  }
  #corrplot(corrplot_interaction_feature_components, method = "number", number.digits = 3)#, type = "upper")
} 
}


#change formula_idx
most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se$Formula[1]
plot_both_correlation_plots(correlation_tables_features_in_formula_havel_lasso_1se, formula_idx = 1)

most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se$Formula[2]
plot_both_correlation_plots(correlation_tables_features_in_formula_havel_lasso_1se, formula_idx = 3, interaction_corr = T)

#lambda_min formula indx
most_selected_formulas_analysis_feature_selection_havel_step_aic$Formula[2]
plot_both_correlation_plots(correlation_tables_features_in_formula_havel_step_aic, formula_idx = 3, interaction_corr = T)



#prints most selected formula lasso lambda min
most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min$Formula[1]
plot_both_correlation_plots(correlation_tables_features_in_formula_havel_lasso_min, formula_idx = 1)

most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min$Formula[2]
plot_both_correlation_plots(correlation_tables_features_in_formula_havel_lasso_min, formula_idx = 2)


#prints most selected formula lasso lambdamin
most_selected_formulas_analysis_feature_selection_havel_step_aic$Formula[1]
plot_both_correlation_plots(correlation_tables_features_in_formula_havel_step_aic, formula_idx = 1)

most_selected_formulas_analysis_feature_selection_havel_step_aic$Formula[2]
plot_both_correlation_plots(correlation_tables_features_in_formula_havel_step_aic, formula_idx = 2)




#till here analysis formula

#analysis features
{
build_most_selected_feature_table <- function(analysis_feature_selection_river_algo){
  #analysis_feature_selection_river_algo<-analysis_feature_selection_havel_lasso_lambda_1se
  most_selected_features_analysis_feature_selection_river_algo<-analysis_feature_selection_river_algo[,-c((ncol(analysis_feature_selection_river_algo)-1),ncol(analysis_feature_selection_river_algo))] %>% colMeans()
  most_selected_features_analysis_feature_selection_river_algo<- as.data.frame(most_selected_features_analysis_feature_selection_river_algo)

  t<-cbind(most_selected_features_analysis_feature_selection_river_algo,row.names(most_selected_features_analysis_feature_selection_river_algo))
  colnames(t)[1] <- "percentage"
  colnames(t)[2] <- "Rownames"
  e<- t%>%arrange(desc(percentage))
  
  return(e)
}
}

most_selected_features_analysis_feature_selection_havel_lasso_lambda_1se<- build_most_selected_feature_table(analysis_feature_selection_havel_lasso_lambda_1se)
most_selected_features_analysis_feature_selection_havel_lasso_lambda_min<- build_most_selected_feature_table(analysis_feature_selection_havel_lasso_lambda_min)

most_selected_features_analysis_feature_selection_havel_step_aic<- build_most_selected_feature_table(analysis_feature_selection_havel_step_aic)
most_selected_features_analysis_feature_selection_havel_step_bic<- build_most_selected_feature_table(analysis_feature_selection_havel_step_bic)

write_csv(most_selected_features_analysis_feature_selection_havel_lasso_lambda_1se, path =  "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/tables/ak.txt")

{
plot_most_selected_features <- function(most_selected_features_analysis_feature_selection_river_algo, only_features_occured_more_than_once=F){
  #most_selected_features_analysis_feature_selection_river_algo<-most_selected_features_analysis_feature_selection_havel_lasso_lambda_min
  #only_features_occured_more_than_once =F
  
  most_selected_features_analysis_feature_selection_river_algo<-most_selected_features_analysis_feature_selection_river_algo[!(row.names(most_selected_features_analysis_feature_selection_river_algo) %in% 'n_selected_variables'),] # drop row n_selected_variables
  most_selected_features_analysis_feature_selection_river_algo$Rownames<-gsub("_mean", "", most_selected_features_analysis_feature_selection_river_algo$Rownames)
  
  if (only_features_occured_more_than_once ==T) {
    most_selected_features_analysis_feature_selection_river_algo_2<-most_selected_features_analysis_feature_selection_river_algo%>% filter(most_selected_features_analysis_feature_selection_river_algo$percentage> min(most_selected_features_analysis_feature_selection_river_algo$percentage) )  
  }else{
    most_selected_features_analysis_feature_selection_river_algo_2<-most_selected_features_analysis_feature_selection_river_algo
  } 
  max_features_formula<-nrow(most_selected_features_analysis_feature_selection_river_algo_2)
  #nrow(most_selected_features_analysis_feature_selection_river_algo)
  
  #max_features_formula<-nrow(most_selected_features_analysis_feature_selection_river_algo_more_than_once_found)
  
  
  
  ymax<-1
  
  
  annotation_label <- paste("Different features selected: ", nrow(most_selected_features_analysis_feature_selection_river_algo))
  ggplot(most_selected_features_analysis_feature_selection_river_algo_2, aes(x =reorder(Rownames,-percentage), y = percentage)) +
    #geom_bar(stat="identity", colour="black", fill="white", width = 0.5) +
    geom_bar(stat="identity", alpha=.8,fill='lightblue',color='lightblue4', width = 0.5) +
    
    theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    #ylim(0, 1)+
    xlab("Variables")+
    ylab("Frequency percentage")+
    scale_y_continuous(limits = c(0,1),breaks = seq(0,1, by= 0.05))+
    annotate("text",x=max_features_formula,y=ymax,hjust=1,vjust=0,label=annotation_label)
}
}


plot_most_selected_features(most_selected_features_analysis_feature_selection_havel_lasso_lambda_1se)
plot_most_selected_features(most_selected_features_analysis_feature_selection_havel_lasso_lambda_min)

#for plotting only the most important
plot_most_selected_features(most_selected_features_analysis_feature_selection_havel_lasso_lambda_min, only_features_occured_more_than_once = T)


write_csv(most_selected_features_analysis_feature_selection_havel_lasso_lambda_1se, path =  "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/tables/ak.txt")
write_csv(most_selected_features_analysis_feature_selection_havel_lasso_lambda_min, path =  "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/tables/lambda_min.txt")

plot_most_selected_features(most_selected_features_analysis_feature_selection_havel_step_aic, only_features_occured_more_than_once = T)
plot_most_selected_features(most_selected_features_analysis_feature_selection_havel_step_bic)






