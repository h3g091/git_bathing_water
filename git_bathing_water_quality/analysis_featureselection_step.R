
{#instancing a dataframe with all possible variable names as colnames 
  build_rename_cols_to_variable_names <- function(df_with_all_vars){
    df<-data.frame(matrix(0, ncol = length((colnames(df_with_all_vars)))+2, nrow = 1))
    colnames(df)<- colnames(df_with_all_vars)
    names(df)[length(names(df))-1]<-"formel" 
    names(df)[length(names(df))]<-"iteration_number" 
    
    
    return(df)
  }
}


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
iteration_river_list_data
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

build_occurence_table_glmnet_all_5_folds_same_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1 , iteration_numb = 1, lambda_value = "lambda.min")[[1]]
build_occurence_table_glmnet_all_5_folds_same_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1 , iteration_numb = 1, lambda_value = "lambda.1se")[[1]]
iterations

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

analysis_feature_selection_havel_lasso_lambda_1se<-build_occurence_table_glmnet_all_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1, lambda_value = "lambda.1se", iterations = 15, whole_table = F)
analysis_feature_selection_havel_lasso_lambda_min<-build_occurence_table_glmnet_all_iteration(saved_iteration_river_list_lasso_Model, river_numb = 1, lambda_value = "lambda.min", iterations = 15, whole_table = F)
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
{
        algo_name1<-get_algoname_for_plot_title(analysis_feature_selection_havel_lasso_lambda_1se)
        lambda_se_barplot_havel<-plot_analysis_table_n_feature(analysis_feature_selection_havel_lasso_lambda_1se, n_data_points = n_data_points_havel, title_name = algo_name1)
        lambda_se_boxplot_havel<-plot_horizontal_boxplot(analysis_feature_selection_havel_lasso_lambda_1se,algo_name1)
        
        ggarrange(plot_horizontal_boxplot(analysis_feature_selection_havel_lasso_lambda_1se,algo_name1),lambda_se_barplot_havel, ncol = 1, nrow = 2)
        
        
        
        algo_name2<-get_algoname_for_plot_title(analysis_feature_selection_havel_lasso_lambda_min)
        plot_analysis_table_n_feature(analysis_feature_selection_havel_lasso_lambda_min, n_data_points_havel, algo_name2)
        
        algo_name1<-get_algoname_for_plot_title(analysis_feature_selection_havel_lasso_lambda_min)
        algo_name2<-get_algoname_for_plot_title(analysis_feature_selection_havel_lasso_lambda_1se)
        plot_analysis_table_n_feature_2_plots(analysis_feature_selection_lambda_min, analysis_feature_selection_lambda_1se, n_data_points_havel,algo_name1 ,algo_name2)
        
}



{  #standard boxplot not combineable with ggplot
plot_analysis_table_n_features_boxplot <- function(analysis_feature_selection_algo,title_name){
  #analysis_feature_selection_algo<-analysis_feature_selection_havel_lasso_lambda_1se
  #title_name<- algo_name2
  
  boxplot(analysis_feature_selection_algo$n_selected_variables,yaxt = "n", ylab = "n_features/formula", main= title_name)
  axis(2, at = seq(0, max(analysis_feature_selection_algo$n_selected_variables), 1), las = 2)
  
    
}
}

plot_analysis_table_n_features_boxplot(analysis_feature_selection_havel_lasso_lambda_1se, algo_name1)



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


most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min<-build_most_selected_formula_table(analysis_feature_selection_havel_lasso_lambda_min)
most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_1se<-build_most_selected_formula_table(analysis_feature_selection_havel_lasso_lambda_1se)


{
  plot_full_formula_occurence <- function(most_selected_formulas_analysis_feature_selection_river_algo, filter_for_freq_bigger_1=F){
    #most_selected_formulas_analysis_feature_selection_river_algo<-most_selected_formulas_analysis_feature_selection_havel_lasso_lambda_min
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
write_csv(most_selected_features_analysis_feature_selection_havel_lasso_lambda_1se, path =  "/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/git_bathing_water/tables/ak.txt")

{
plot_most_selected_features <- function(most_selected_features_analysis_feature_selection_river_algo){
  #most_selected_features_analysis_feature_selection_river_algo<-most_selected_features_analysis_feature_selection_havel_lasso_lambda_1se
  most_selected_features_analysis_feature_selection_river_algo<-most_selected_features_analysis_feature_selection_river_algo[!(row.names(most_selected_features_analysis_feature_selection_river_algo) %in% 'n_selected_variables'),] # drop row n_selected_variables
  most_selected_features_analysis_feature_selection_river_algo$Rownames<-gsub("_mean", "", most_selected_features_analysis_feature_selection_river_algo$Rownames)
  ggplot(most_selected_features_analysis_feature_selection_river_algo, aes(x =reorder(Rownames,-percentage), y = percentage)) +
    #geom_bar(stat="identity", colour="black", fill="white", width = 0.5) +
    geom_bar(stat="identity", alpha=.8,fill='lightblue',color='lightblue4', width = 0.5) +
    
    theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    #ylim(0, 1)+
    xlab("Variables")+
    ylab("Frequency percentage")+
    scale_y_continuous(limits = c(0,1),breaks = seq(0,1, by= 0.05))
}
}


plot_most_selected_features(most_selected_features_analysis_feature_selection_havel_lasso_lambda_1se)
plot_most_selected_features(most_selected_features_analysis_feature_selection_havel_lasso_lambda_min)









