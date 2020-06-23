{
  {
library(magrittr)
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
  
  data <- data %>% filter(log_e.coli > log10(15)) #why-heiko? genauigkeit test?
  
  
  #Definition of models
  
  # Definition of null and full models
  #stepwise models
  null <- lm(log_e.coli ~ 1, data = data) #model with only 1 variable
  
  full <- lm(log_e.coli ~ .^2, data = data)
  
  #heiko models
  
  
} 
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
    get_formula_variable_names <- function(formula_a,df){ 
      mf <- model.frame(formula_a, data=df)
      mt <- attr(mf, "terms")
      predvarnames <- attr(mt, "term.labels")
      predvarnames
    }
    
    #lasso
    #build/integrate here into folds to train with same cross validation
    #fold1<-train_rows[[1]]
    
    #training_heiko<-data[fold1,]
    
    part1<-names(data)[1]
    form<-formula(paste(part1," ~ (.)^2"))
    get_formula_variable_names(form,data)
        #training_heiko_features <- (training_heiko%>% select(-log_e.coli))
    #sparse.model.matrix(form, training_heiko)
    
    #form <- log_e.coli ~ (.)ˆ2
    #training_heiko_features_matrix <- (data.frame.2.sparseMatrix(training_heiko_features))
    train_sparse <- sparse.model.matrix(form, data) #data must be dataframe
    #train_sparse <- sparse.model.matrix(training_heiko$log_e.coli~(.)ˆ2, training_heiko[,3:ncol(training_heiko)]) #data must be dataframe
    #form <- Y ~ (x + y + z)^2
    #testing_heiko<-data[-fold1,]
    
   # test_sparse <- sparse.model.matrix(testing_heiko$log_e.coli~., testing_heiko[,3:ncol(testing_heiko)]) #data must be dataframe
    set.seed(4)

    {        
    fit_lasso_base <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = F, alpha = 1,relax = F)
    
    fit_lasso_base_cross <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3,standardize = F,relax = F)#--> alpha =1:  lasso regressio
    
    
    fit_lasso_base_stand <- glmnet(train_sparse, data$log_e.coli , na.rm =T, standardize = T, alpha = 1,relax = F)
    fit_lasso_base_cross_stand <- cv.glmnet(train_sparse, data$log_e.coli,type.measure="mse", alpha=1, family="gaussian",  nfolds = 3,standardize = T,relax = F)#--> alpha =1:  lasso regressio
    
    par(mfrow=c(2,2))
    plot(fit_lasso_base, xvar="lambda", label = T, main = "lasso_base")
    plot(fit_lasso_base_cross,main="LASSO")
    
    plot(fit_lasso_base_stand, xvar="lambda", label = T, main = "lasso_base_stand")
    plot(fit_lasso_base_cross_stand,main="LASSO")
    
    #plot(fit_elnet_base, xvar="lambda", label = T, main = "elnet_base")
    #plot(fit_elnet_base_cross,main="elnet")
    
    #plot(fit_elnet_base_stand, xvar="lambda", label = T, main = "elnet_base_stand")
    #plot(fit_elnet_base_cross_stand,main="elnet")
    
    
    
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
   
    coef_1se_fit_lasso_base_cross<-get_coef_1se_cv (fit_lasso_base_cross)
    coef_1se_fit_lasso_base_cross_stand<-get_coef_1se_cv (fit_lasso_base_cross_stand)
    coef_lambda_min_fit_lasso_base_cross<-get_coef_min_cv (fit_lasso_base_cross)
    coef_lambda_min_fit_lasso_base_cross_stand<-get_coef_min_cv (fit_lasso_base_cross_stand)
    
 # add_new_formulas_to_list_if_exists <- function(coef_list){
    
#    if(exists("coef_1se_fit_lasso_base_cross")== TRUE){
 #     idx <- length(list_lasso)
  #    idx <- idx+1
  #    list_lasso[[idx]] <-coef_1se_fit_lasso_base_cross
  #  }
  #}
        
    list_lasso <- list()
    
    coef_1se_fit_lasso_base_cross               <-get_feature_selection_coeficient_names_as_formular_1se(fit_lasso_base_cross)
    
    if(exists("coef_1se_fit_lasso_base_cross")== TRUE){
      idx <- length(list_lasso)
      idx <- idx+1
      list_lasso[[idx]] <-coef_1se_fit_lasso_base_cross
    }
    coef_1se_fit_lasso_base_cross_stand         <-get_feature_selection_coeficient_names_as_formular_1se(fit_lasso_base_cross_stand)
    if(exists("coef_1se_fit_lasso_base_cross_stand")== TRUE){
      idx <- length(list_lasso)
      idx <- idx+1
      list_lasso[[idx]] <-coef_1se_fit_lasso_base_cross_stand
    }
    
    coef_lambda_min_fit_lasso_base_cross        <-get_feature_selection_coeficient_names_as_formular_lambda_min(fit_lasso_base_cross)
    if(exists("coef_lambda_min_fit_lasso_base_cross")== TRUE){
      idx <- length(list_lasso)
      idx <- idx+1
      list_lasso[[idx]] <-coef_lambda_min_fit_lasso_base_cross
    }
    
    coef_lambda_min_fit_lasso_base_cross_stand  <-get_feature_selection_coeficient_names_as_formular_lambda_min(fit_lasso_base_cross_stand)
    if(exists("coef_lambda_min_fit_lasso_base_cross_stand")== TRUE){
      idx <- length(list_lasso)
      idx <- idx+1
      list_lasso[[idx]] <-coef_lambda_min_fit_lasso_base_cross_stand
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
    list_lasso
    print(paste(length(list_lasso)," new models added")) 
    model_lsit<-list()
    list_lasso
    #builded linear model
    heiko_lm_1<-lm(list_lasso[[1]], data = data)
    heiko_lm_2<-lm(list_lasso[[2]],data=data)
    heiko_lm_3<-lm(list_lasso[[3]],data=data)
    heiko_lm_4<-lm(list_lasso[[4]],data=data)
    
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
  
  stepwise <- function (river, pattern, data, null, full ){
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
  
  fmla_heiko <- list()
  fmla_heiko[[1]]<- fmla_heiko_1
  fmla_heiko[[2]]<- fmla_heiko_2
  fmla_heiko[[3]]<- fmla_heiko_3
  fmla_heiko[[4]]<- fmla_heiko_4
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
  fb
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
  
  #weirde zeile, setze alle stat tests auf 0
  river_stat_tests$in95 <- river_stat_tests$below95 <-river_stat_tests$below90 <- river_stat_tests$in50 <- 0
  
  
  train_rows <- caret::createFolds(1:nrow(fb[[paste0(river, "model_01")]]$model),
                                   
                                   k = 5, list = T, returnTrain = T)
  

  
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
        
        
        
        fit <- rstanarm::stan_glm(fmla[[i]], data = training ) #fitting
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
  
#fmla

}  
  
}  
  sorted_modellist <- river_stat_tests %>%
    
    filter( below95 == 5 & below90 == 5& in95) %>%
    
    dplyr::arrange(desc(in50), desc(R2))
  
  river_stat_tests
  sorted_modellist
  
  best_valid_model_stats <- sorted_modellist[1,]
  
  best_valid_model <- fb[[best_valid_model_stats$model]]
  
  coef(best_valid_model)
  
  
  #refit best model
  stanfit <- rstanarm::stan_glm(fmla[[best_valid_model_stats$model]],
                                
                                data = best_valid_model$model)
  
  brmsfit <- brms::brm(fmla[[best_valid_model_stats$model]],
                       
                       data = best_valid_model$model, iter = 10000)
  
  
  
  
  best_valid_model
  par(mfrow = c(1,1))
  plot(predict(best_valid_model),data$log_e.coli,
       xlab="predicted",ylab="actual")
  abline(a=0,b=1)
  par(mfrow = c(2,2))
  plot((best_valid_model))
  
 
  
 
  
  #return(list(sorted_modellist = sorted_modellist,
              
   #           best_model = best_valid_model,
              
    #          stanfit = stanfit,
              
     #         brmsfit = brmsfit))
  
  
  


