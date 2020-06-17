{library(magrittr)
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

library(kwb.flusshygiene)



#if (FALSE)
  

  
  #### Laden von Testdaten ###################
  
  rivers <- c("havel")
  river <- "havel"
  #river_paths <- kwb.flusshygiene::get_paths()[paste0(rivers, "data")]
  
  river_paths <- list(havel = "Y:/SUW_Department/Projects/FLUSSHYGIENE/Data-Work packages/Daten/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv")
  
  river_paths <- list(havel = "/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv")
  
  river_data <- lapply(river_paths, kwb.flusshygiene::import_riverdata)
  
  river <- "havel"
  
  names(river_data) <- rivers

  
#
  calc_t <- function (datalist=river_data$havel) {
    #heiko
    #datalist<- river_data1$havel
    phy_data <- datalist[-1] # Entfernung der Hygienedaten
    
    hyg_df <- subset(datalist[[1]],
                     
                     subset = lubridate::month(datum) %in% 5:9) # Filtern nach Sommer
    
    data_summer <- lapply(phy_data, function(df){
      
      df <- subset(df, subset = lubridate::month(datum) %in% 4:9)
      
    })
    
    
    
    # z_standardize <- function (x) {
    
    #   y = (x - mean(x, na.rm=T))/sd(x, na.rm=T)
    
    # }
    
    transform_z <- function(df) {
      
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
    
    data_t <- lapply(data_summer, transform_z)
    
    result <- append(list(hyg_df), data_t)
    
    names(result) <- names(datalist)
    
    return(result)
    
  }
  ### Anwenden von calc_t auf Inputliste
  
  river_data_ts <- lapply(river_data, function(river_list){
    
    river_ts <- calc_t(river_list) # use function
    
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
  
  
  
  rm(river_data,calc_t)
  
  river = "havel"
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
  
  data <- data %>% filter(log_e.coli > log10(15)) #why-heiko?
  
  
  #Definition of models
  
  # Definition of null and full models
  #stepwise models
  null <- lm(log_e.coli ~ 1, data = data) #model with only 1 variable
  
  full <- lm(log_e.coli ~ .^2, data = data)
  
  #heiko models
  
  
  
  {
    #heiko
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
      
      get_coef_fixed_lambda <- function(df,lambda){
        tmp_coeffs <- coef(df, s = lambda)
        a <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
        return(a)
      }
      
      
    }  
    #lasso
    #build/integrate here into folds to train with same cross validation
    #fold1<-train_rows[[1]]
    
    #training_heiko<-data[fold1,]
    training_heiko <- data
    #training_heiko_features <- (training_heiko%>% select(-log_e.coli))
    
    
    #training_heiko_features_matrix <- (data.frame.2.sparseMatrix(training_heiko_features))
    train_sparse <- sparse.model.matrix(training_heiko$log_e.coli~., training_heiko[,3:ncol(training_heiko)]) #data must be dataframe
    
    #testing_heiko<-data[-fold1,]
    
   # test_sparse <- sparse.model.matrix(testing_heiko$log_e.coli~., testing_heiko[,3:ncol(testing_heiko)]) #data must be dataframe
    
    fit_lasso_base <- glmnet(train_sparse, training_heiko$log_e.coli , na.rm =T, standardize = F, alpha = 1,relax = F)
    fit_lasso_base_cross <- cv.glmnet(train_sparse, training_heiko$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3,standardize = F,relax = F)#--> alpha =1:  lasso regressio
    
    fit_lasso_base_stand <- glmnet(train_sparse, training_heiko$log_e.coli , na.rm =T, standardize = T, alpha = 1,relax = F)
    fit_lasso_base_cross_stand <- cv.glmnet(train_sparse, training_heiko$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3,standardize = T,relax = F)#--> alpha =1:  lasso regressio
    
    
    
    coef_1se<-get_coef_1se_cv(fit_lasso_base_cross_stand)
    coef_min_lambda<-get_coef_min_cv(fit_lasso_base_cross_stand)
    coef_1se
    coef_min_lambda
    
    coef_name_lambda_min<-coef_min_lambda$name[-1]
    a<-str("")
    
    l<-paste(coef_name_lambda_min, collapse = " + " )
    
    k<-paste("log_e.coli ~ ", l)
    
    
    formula_heiko_1<-formula(k)
    
  }
  
  
  
  heiko_lm<- lm(formula_heiko_1, data = data)   
  
#nicht mehr benötigt  
  #### Anwenden der Hauptfunktion ###################
  {  
  #results <- build_and_validate_model(river_data = river_data, river = river)
  






#### Definition der Hauptfunktion #################





#build_and_validate_model <- function(river_data, river) {
  
  
  
#  calc_t <- function (datalist=river_data$havel) {
    
 #   phy_data <- datalist[-1] # Entfernung der Hygienedaten
    
#    hyg_df <- subset(datalist[[1]],
                     
 #                   subset = lubridate::month(datum) %in% 5:9) # Filtern nach Sommer
    
#    data_summer <- lapply(phy_data, function(df){
      
#      df <- subset(df, subset = lubridate::month(datum) %in% 4:9)
      
 #   })
    
    
    
    # z_standardize <- function (x) {
    
    #   y = (x - mean(x, na.rm=T))/sd(x, na.rm=T)
    
    # }
    
  #  transform_z <- function(df) {
      
  #    for (site in names(df)[-1]) { # every col gets treatment
        
  #      df2 <- subset(df, select = c("datum", site))
        
   #     if (grepl("^r_.*",site)) { # rain gets log-transformed and 1/sigma2
          
    #      df2[[site]] <- log(df2[[site]]+1)
          
          # df2[[site]] <- df2[[site]]/sd(df2[[site]], na.rm=T)
          
     #   } #else {
        
        #   df[[site]] <- z_standardize(df2[[site]]) # standardize
        
        # }
        
      #  df[[site]] <- df2[[site]]
        
    #  }
      
    #  return(df)
      
  #  }
    
   # data_t <- lapply(data_summer, transform_z)
    
  #  result <- append(list(hyg_df), data_t)
    
  #  names(result) <- names(datalist)
    
  #  return(result)
    
#  }
  
  
  
  
  
  ### Anwenden von calc_t auf Inputliste
  
  #river_data_ts <- lapply(river_data, function(river_list){
    
#    river_ts <- calc_t(river_list) # use function
    
 #   add_meancol <- function (df) { # for rain and i #edit: + ka #2ndedit: + q
      
   #   prefix <- unique(sub("([a-z])_.*","\\1",names(df)[-1]))
      
  #    for (pre in prefix) {
        
   #     df2 <- dplyr::select(df, dplyr::starts_with(pre))
        
    #    df[,paste0(pre,"_mean")] <- rowMeans(df2, na.rm=T)
        
    #  }
      
      
      
  #    return(df)
      
  #  }
    
  #  add_sumcol <- function (df) { # originally for ka, but not used
      
 #     prefix <- unique(sub("([a-z])_.*","\\1",names(df)[-1]))
      
  #    if (length(df) > 2)
        
   #     df[,paste0(prefix,"_sum")] <- rowSums(df[,-1], na.rm=T)
      
  #    return(df)
      
  #  }
    
    
    
  #  q_pos <- grep("^q", names(river_ts)[-1])+1
    
    
   # if (length(q_pos) == 1)
      
    #  river_ts[[q_pos]] <- add_meancol(river_ts[[q_pos]])
    
#    ka_pos <- grep("^ka", names(river_ts)[-1])+1
    
 #   if (length(ka_pos) == 1)
      
  #    river_ts[[ka_pos]] <- add_meancol(river_ts[[ka_pos]])
    
  #  i_pos <- grep("^i", names(river_ts)[-1])+1
    
  #  if (length(i_pos) == 1)
      
   #   river_ts[[i_pos]] <- add_meancol(river_ts[[i_pos]])
    
#    r_pos <- grep("^r", names(river_ts)[-1])+1
    
 #   river_ts[[r_pos]] <- add_meancol(river_ts[[r_pos]])
    
  #  return(river_ts)
    
#  })
  
  
  
 # rm(river_data,calc_t)
  
  
  
  
  
  
  
  # step through, forward and backward selection
  
  #fb <- stepwise(river = river, pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)")#,
}  
  stepwise <- function (river, pattern, data, null, full ){
 {#   river = river
  #  pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)"
  #  riverdata <- river_data_ts[[river]]
    
    # prepare variables out of all cominations (given by pattern)
    
    # variables for interaction get replaced by q_new (remove q_old)
    
  #  vars1 <- (riverdata[-1] %>% unroll_physical_data() %>%
                
   #             lapply(names) %>% unlist() %>% unique())[-1]
    
  #  vars2 <- vars1[stringr::str_detect(vars1, pattern)]
    
    
    
    # prepare formulas
    
#    data <- process_model_riverdata(riverdata, c("log_e.coli", vars2)) %>%
      
#      dplyr::select(-datum)
    
    
    
    
    
#    data <- na.omit(data)
    
#    data <- data %>% filter(log_e.coli > log10(15)) #why-heiko?
    
    # Definition of null and full models
    
#    null <- lm(log_e.coli ~ 1, data = data) #model with only 1 variable
    
#    full <- lm(log_e.coli ~ .^2, data = data)
    
    #heiko
    
 #   heiko_lm<- lm(formula_heiko_1, data = data)
    #heiko test if all coefficients are not NA
    
 } 
    # Definition maximum number of steps
    
    nsteps <- 5 #ifelse(round(nrow(data)/10) < 10, round(nrow(data)/10), 5 )
    
    selection <- list()
    
    fmla <- list()
    
    
    
    # Creating list of candidate models with 1 ...n predictors 
    #split up this piece in stpe and new algorithms/formulars
    
    for(i in 1: nsteps){
      
      
      
      selection[[i]] <- step(null, data = data,
                             
                             direction = "forward",
                             
                             list(lower=null, upper=full), steps = i)   
      
      
      fmla[[i]] <- as.list(selection[[i]]$call)$formula
      
      
      
    }
    
    #heiko_add_formular to fmla list function function
    #selection[[6]] <- heiko_lm
    #fmla[[6]] <- as.list(selection[[6]]$call)$formula
    step_returns <- list(fmla, selection)
    return(step_returns)
    
  }
  
  
  
  
  
  # order of pattern, q_old and q_new is important!
  
  #fb <- stepwise(river = river, pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)", data,null, full)#,
  step_returns <- stepwise(river = river, pattern = "(i_mean|q_mean_mean|r_mean_mean|ka_mean_mean)", data,null, full)
  fmla <- step_returns[[1]]
  selection <- step_returns[[2]]
  
  
  
 
  
  
  
selection<-append(selection, list(heiko_lm))
fb<- selection  
#fb[6] <- list(heiko_lm)
  
  #selection[6] <- list(heiko_lm)
  selection
  fb
  
  fmla_heiko <-eval(heiko_lm$call$formula)
 
  # as.list(selection[[6]]$call)$formula
  
  fmla<-append(fmla, fmla_heiko)
  fmla
  if(class(fmla[[length(fmla)]]) !="formula"){
    print("new element is no formula!!")
  }
 
  
  #add my models here
  
  
  #q_old = "q_cochem",
  
  #q_new = "q_cochem_abs_1")
  


  
  
  
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
  
  unique_index <- length(unique(fb))
  
  fb <- fb[1:unique_index]
  
  
  
  # testing for classical statistical model assumtions, normality of residuals and
  
  # heteroskelasdicity
  
  river_stat_tests <- sapply(fb, get_stat_tests)%>%
    
    t() %>%
    
    dplyr::as_tibble(rownames = "model")  %>%
    
    
    
    dplyr::bind_rows(.id = "river") %>%
    
    
    
    dplyr::mutate(stat_correct = N > .05 & BP > .05)
  
  
  
  # creating list of independent training rows
  #-test/train split
  
  #weirde zeile
  river_stat_tests$in95 <- river_stat_tests$below95 <-
    
    river_stat_tests$below90 <- river_stat_tests$in50 <- 0
  
  
  train_rows <- caret::createFolds(1:nrow(fb[[paste0(river, "model_01")]]$model),
                                   
                                   k = 5, list = T, returnTrain = T)
  
  #train here the lasso-algo
  
  
  
  
  
 # names(fb)
  
  
  
  #fmla <- list()

  
  #for(i in names(fb))
   
   # fmla[[i]] <- as.list(eval(fb[[i]])$call)$formula
  
  if(class(fmla[[length(fmla)]]) !="formula"){
    print("new element is no formula!!")
  }
  
  test_beta <- function(true, false, percentile)
    
  { if( pbeta(q = percentile, shape1 = true + 1, shape2 = false + 1) > 0.05)
    
  {TRUE}
    
    else{FALSE}
    
  }
  
  
  names(fmla) <- sprintf(paste0(river,"model_%02d"), seq_along(1:length(fb)))
  

  
  
  counter<-0
  
  #fb<-fb[-6]
  names(fb)
}

  for(i in names(fb)){
  counter<- counter+1
       #i="havelmodel_01"
    for(j in 1:5){
      counter <- counter+1
       #   j=1
    
      
      
        
        training <- as.data.frame(fb[[i]]$model)[c(train_rows[[j]]),]
        #training <- as.data.frame(fb[[6]]$model)[c(train_rows[[1]]),]
        test <- as.data.frame(fb[[i]]$model)[-c(train_rows[[j]]),]
        #test <- as.data.frame(fb[[6]]$model)[-c(train_rows[[1]]),]
        #formel<-formula(formula_heiko_1)
        
        
        #fmla[6]<- list(formel)
        
        
        
        fit <- rstanarm::stan_glm(fmla[[i]], data = training) #fitting
        #fit <- rstanarm::stan_glm(fmla[[1]], data = training) #fitting
       
        
        df <- apply(rstanarm::posterior_predict(fit, newdata = test), 2, quantile, #predicting
                    
                    probs = c(0.025, 0.25, 0.75, 0.9, 0.95, 0.975)) %>% t() %>% as.data.frame() %>%
          
          dplyr::mutate(log_e.coli = test$log_e.coli, #evaluating ther model has to be classified correctly with every single test train split
                        #--> here 5 different splits, if all validations correct than everywhere ==5
                        
                        below95 = log_e.coli < `95%`,
                        
                        below90 = log_e.coli < `90%`,
                        
                        within95 = log_e.coli < `97.5%`& log_e.coli > `2.5%`,
                        
                        within50 = log_e.coli < `75%`& log_e.coli > `25%`,
                        
          )
        
        #validation step if allpercentile categories are set to 1
        
        river_stat_tests$in95[river_stat_tests$model == i] <-
          
          river_stat_tests$in95[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$within95), false = sum(!df$within95), percentile = .95 )
        
        river_stat_tests$below95[river_stat_tests$model == i] <-
          
          river_stat_tests$below95[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$below95), false = sum(!df$below95), percentile = .95 )
        
        river_stat_tests$below90[river_stat_tests$model == i] <-
          
          river_stat_tests$below90[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$below90), false = sum(!df$below90), percentile = .90 )
        
        river_stat_tests$in50[river_stat_tests$model == i] <-
          
          river_stat_tests$in50[river_stat_tests$model == i] +
          
          test_beta(true = sum(df$within50), false = sum(!df$within50), .5)
        
        
        
      } 
      
  } 
  
fmla

fmla_heiko
  
  
  
  sorted_modellist <- river_stat_tests %>%
    
    filter( below95 == 5 & below90 == 5& in95) %>%
    
    dplyr::arrange(desc(in50), desc(R2))
  
  river_stat_tests
  sorted_modellist
  
  best_valid_model_stats <- sorted_modellist[1,]
  
  best_valid_model <- fb[[best_valid_model_stats$model]]
  
  
  
  
  #refit best model
  stanfit <- rstanarm::stan_glm(fmla[[best_valid_model_stats$model]],
                                
                                data = best_valid_model$model)
  
  brmsfit <- brms::brm(fmla[[best_valid_model_stats$model]],
                       
                       data = best_valid_model$model, iter = 10000)
  
  
  
  return(list(sorted_modellist = sorted_modellist,
              
              best_model = best_valid_model,
              
              stanfit = stanfit,
              
              brmsfit = brmsfit))
  
  
  


