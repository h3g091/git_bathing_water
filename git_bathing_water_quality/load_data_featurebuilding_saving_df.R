{
library(dplyr)
library(glmnet)
library(purrr)
library(tidyverse)
#?glmnet
library(coefplot) #for extracing non 0 coef
#install.packages("tidyverse")
library(tidyverse)
library(pROC)

save_preprocessed_df <- function(df, name){
  savepath <-  paste("/Users/heiko.langer/Masterarbeit_lokal/Masterprojekt/Masterarbeit/Dataframes_processed_heiko/", name,".csv" ,sep = "") 
  write.csv(df, savepath)  
}

all_days_df<- function(date1,date2){ #gives back a df with all days in a datafram as "date"object
  dates <- as.Date(as.Date(date1) : as.Date(date2), origin="1970-01-01")
  dates <- data.frame("datum" = dates)
  return(dates)
}

set.seed(2)

date1 <- "2008-01-01"
date2 <- "2017-12-31"
dates <- all_days_df(date1,date2)


dates <- as.Date(as.Date("2008-01-01") : as.Date("2017-12-31"), origin="1970-01-01")
test_dates <- data.frame("datum" = dates )
id<-c(1:nrow(test_dates))
test_dates <- cbind(test_dates, id)


split_up_dataframe <- function(df){
  col_names <- names(df)
  list_new_dfs <- list()
  for (i in col_names) {
    new_dfs <- df %>%
      select(datum, i)
    
    list_new_dfs<-c(list_new_dfs, list(new_dfs))
  }
  list_new_dfs<- list_new_dfs[-c(1:2)]
  return(list_new_dfs)
}

temporal_system_boudary <- function(df){
  
  boundary_1 <- df %>%
    select(e.coli)%>%
    summarise(sum(!is.na(.)))
  boundary_2 <- df %>%
    select(entero)%>%
    summarise(sum(!is.na(.)))
  
  b<-max(boundary_1, boundary_2)
  #spearman_rank correlation as system boundary
  c <- 2/sqrt(b)
  return(c)
}

# new lag function that also names new columns
lag_variable_and_bind_to_df <- function(df, lags=1){ #needs a tibble with 2 cols datum and another col
  temp_df <- df
  idx <-1
  while (idx<=lags) {
    temp_df2 <- tibble(lead(temp_df[[2]],idx)) #builds lagged df
    name_column <- names(temp_df[2]) #
    names(temp_df2) <- paste(name_column,"lag", idx, sep = "_") #builds names for new cols
    temp_df<-cbind(temp_df,temp_df2) #cbinds new cols and original df
    idx <- idx +1
  }
  return(temp_df[,2:ncol(temp_df)])
}


#average function
averaging_over_lags_and_bind_to_df <- function(df, idx1 = 2){ #takes lagged df as input and returns averages from lag 1 till last
  temp_df<- df
  idx2 <- idx1+1
  n_cols <- ncol(temp_df)
  #lags <- ncol(temp_df)-2
  cols_name <- names(temp_df[2])
  while (idx2 <=n_cols) {
    new_name <- paste(cols_name,"avg","lag", idx1-1, idx2-1, sep =  "_")
    new_df<-(mutate(temp_df, !!new_name := rowMeans(temp_df[c(idx1:idx2)], na.rm = TRUE)))
    temp_df <-(cbind(temp_df,new_df[ncol(new_df)]))
    idx2<- idx2 +1
  }
  return(temp_df)
}

feature_building<- function(df, lags=6){
 #df<-rain
  a<-df
  b <- split_up_dataframe(df)
  
  e<-select(df,c(datum,id))
  
  for (eintrag in b) {
    c<- lag_variable_and_bind_to_df(eintrag,lags)
    d<-averaging_over_lags_and_bind_to_df(c)
    e <- cbind(e,d)
    
  }
  f<-e[,!duplicated(colnames(e),fromLast = T)]
  #f <- rename(f, "datum" = "e") #For renaming tibble column
  return(f)
}

#average functions over multiple measurement spots
average_rain<- function(df,lags){
  #lags=6
  #df<-rain_feature
  avg_dataframe_build <- df
  #ncol(avg_dataframe_build)
  
  if(ncol(avg_dataframe_build%>%select(contains("r_"))%>% select(!contains("lag")))>1){   #check if multiple rain gauges
    today<-df%>%
      select(!contains("lag"))
    today_avg<-today%>%
      mutate(avg_rain = rowMeans(.[,-c(1:2)],na.rm = T))%>%#do not take id and datum for rowMean
      select(datum,id,avg_rain)
    
    avg_dataframe_build<- avg_dataframe_build%>%
      left_join(today_avg)
    
    for (i in 1:lags) {
      #i=1
      name<-(paste("lag",i, sep = "_"))
      name_avg <- paste("avg_rain",name,sep = "_")
      a<-df%>%
        select(datum,id,contains(name))%>%
        select(!contains("avg"))
      b<-a%>%
        mutate(!!name_avg := rowMeans(.[,-c(1:2)],na.rm = T))%>% #do not take id and datum for rowMean
        select(datum,id,name_avg)
      
      avg_dataframe_build<- avg_dataframe_build%>%
        left_join(b)
    }
  }else{
    print("Only 1 rain_gauge , nothing to average")
  }
  return(avg_dataframe_build) 
}
average_WWTP<- function(df,lags){
  avg_dataframe_build <- df
  if(ncol(avg_dataframe_build%>%select(contains("ka_"))%>% select(!contains("lag")))>1){   
    today<-df%>%
      select(!contains("lag"))
    today_avg<-today%>%
      mutate(avg_WWTP = rowMeans(.[,-c(1:2)],na.rm = T))%>%
      select(datum,avg_WWTP)
    
    avg_dataframe_build<- avg_dataframe_build%>%
      left_join(today_avg)
    
    for (i in 1:lags) {
      name<-(paste("lag",i, sep = "_"))
      name_avg <- paste("avg_WWTP",name,sep = "_")
      a<-df%>%
        select(datum,contains(name))%>%
        select(!contains("avg"))
      b<-a%>%
        mutate(!!name_avg := rowMeans(.[,-c(1:2)],na.rm = T))%>%
        select(datum,name_avg)
      
      avg_dataframe_build<- avg_dataframe_build%>%
        left_join(b)
    }
  }else{
    print("Only 1 WWTP, nothing to average")
  }
  return(avg_dataframe_build) 
}

#log10 +1  function from vick for rain
log_10_plus_1 <- function(x){
  return(log10(x+1))
}

ln_plus_1 <- function(x){
  return(log(x+1))
}

#combine avg_rain and different q-plants from same day to new variables
preprocess_data <-function(hygiene, irradiance, flow_rate,WWTPs_Flow_rate,rain, number_of_lags){    
 
  #hygiene<- hygiene1 
  #irradiance<-irradiance1
  #flow_rate<-flow_rate1
  #WWTPs_Flow_rate<-WWTPs_Flow_rate1
  #rain<-rain1
  #number_of_lags=6
  
  hygiene[-1]<-log10(hygiene[-1]) #log10 e.coli
  #hygiene[-1] <- scale(hygiene[-1], scale = T, center = T)
  #colMeans(hygiene[-1], na.rm = T)
  #apply(hygiene[-1], 2, sd, na.rm = T)
 
  
  #irradiance[-1] <- scale(irradiance[-1], scale = T, center = T)
  #colMeans(irradiance[-1], na.rm = T)
  #apply(irradiance[-1], 2, sd, na.rm = T)
  #flow_rate[-1] <- scale(flow_rate[-1], scale = T, center = T)
  
  #WWTPs_Flow_rate[-1] <- scale(WWTPs_Flow_rate[-1], scale = T, center = T)
  
  rain[-1]<-log_10_plus_1(rain[-1])
  #rain[-1] <- scale(rain[-1], scale = T, center = T)
  
  {#be careful multiple dates can happen
    #hygiene$datum[duplicated(hygiene$datum)]
    hygiene$datum = as.Date(hygiene$datum)
    irradiance$datum = as.Date(irradiance$datum)
    flow_rate$datum = as.Date(flow_rate$datum)
    WWTPs_Flow_rate$datum = as.Date(WWTPs_Flow_rate$datum)
    rain$datum = as.Date(rain$datum)
  }
  
  hygiene<-hygiene%>% group_by(datum)%>%
    summarize_all(funs(mean))
  #any(duplicated(hygiene$datum))
  
  #a<-duplicated(hygiene$datum) 
  #hygiene<-hygiene[-a,]
  #fill up empty rows of date in ilz
  {
    hygiene <-test_dates%>%left_join(hygiene, by= "datum")
    irradiance <- test_dates%>%left_join(irradiance, by= "datum")
    flow_rate <- test_dates%>%left_join(flow_rate, by= "datum")
    WWTPs_Flow_rate <- test_dates%>%left_join(WWTPs_Flow_rate, by= "datum")
    rain <- test_dates%>%left_join(rain, by= "datum")
  }
  

  
  
  train_test_split_df <- hygiene
    
  e_col <- train_test_split_df%>%
    rename(log10_bacteria = e.coli)%>%
    select(-c(entero))%>%
    drop_na(log10_bacteria)
  
  #drop all empty cols
  e_col<-  e_col[, colSums(is.na(e_col)) != nrow(e_col)]
  
  entero <- train_test_split_df%>%
    rename(log10_bacteria=entero)%>%
    select(-c(e.coli))%>%
    drop_na(log10_bacteria)
  
  entero<-  entero[, colSums(is.na(entero)) != nrow(entero)]
  #glimpse(e_col)
  
  #change here to train either entero or e_col bacteria ==F --> entero
  
  #change here for enterokokken
  some_df <- e_col
    
  #be careful with set_seed
  split <- sample(nrow(some_df), floor(0.7*nrow(some_df)))
  
  
  train <- as.data.frame(some_df[split,])
  train_mean<-mean(train$log10_bacteria)
  train_sd<-sd(train$log10_bacteria)
  
  #train$log10_bacteria <- (train$log10_bacteria - train_mean)/train_sd
  
  test <- as.data.frame(some_df[-split,])
  test_mean<-mean(test$log10_bacteria)
  test_sd<-sd(test$log10_bacteria)
  
  #test$log10_bacteria <- (test$log10_bacteria - test_mean)/test_sd
  
  
  #number_of_lags=6
  
  lags <- number_of_lags
  #change here to preprocess different dataset
  
  flow_feature <- feature_building(flow_rate,lags)
  rain_feature <- feature_building(rain,lags)
  
  #optional
  rain_feature<-average_rain(rain_feature,lags)
  
  #log10+1 rain features why?
  rain_feature[-c(1:2)]<- lapply(rain_feature[-c(1:2)],log_10_plus_1)
  
  irradiance_features <- feature_building(irradiance,lags)
  
  WWTPs_Flow_rate_features <- feature_building(WWTPs_Flow_rate,lags)
  #optional
  WWTPs_Flow_rate_features <- average_WWTP(WWTPs_Flow_rate_features,lags)
  
  
  
  all_features <-test_dates%>%
    left_join(hygiene, by=c("datum", "id"))%>%
    left_join(flow_feature, by=c("datum", "id") ) %>%
    left_join(rain_feature,by=c("datum", "id"))%>%
    left_join(irradiance_features,by=c("datum", "id"))%>%
    left_join(WWTPs_Flow_rate_features,by=c("datum", "id"))
  
  
  #bis hier hin alles fresh
  #remove duplicates
 
  
  
  all_features1<-combine_q_rain(all_features,number_of_lags)
  
  #head(all_features1%>% select(q_kalteneck, avg_rain , r_avg_q_kalteneck, q_kalteneck_lag_1, avg_rain_lag_1, r_avg_q_kalteneck_lag_1))
 return(list(all_features1,split, train_mean,train_sd,test_mean,test_sd))
 # return(all_features1)
}


combine_q_rain <-function(df,lags){
  #df <- all_features
  df2<-df
  all_features_r_q<-df %>%
    select(datum, id ,contains("q"), contains("_rain"))%>%
    select(-contains("lag"))
  
  qs<-ncol(all_features_r_q)-2
  for (i in 1:qs) {
    #i=1
    name <-(names(all_features_r_q[i+2]))
    name_new_col <- paste("r_avg",name, sep = "_")
    
    s<-mutate(all_features_r_q,!!name_new_col :=  !!sym(name) * avg_rain)
    
    b<-s%>%
      select(datum,contains(name_new_col))
    df2<- df2 %>% 
      left_join(b)
    
    #lags = 6
    if(lags !=0){
      for(j in 1:lags){
        lagpos<-j
        
        q_lagpos_name<-paste(name,"lag", lagpos, sep = "_")
        r_lagpos_name <- paste("avg_rain_lag",lagpos,sep = "_")
        
        #r_q_lagpos<-all_features %>% select(datum,q_lagpos_name,r_lagpos_name)
        
        name_new_col2 <- paste(name_new_col,"lag", lagpos, sep = "_")
         #head(all_features)%>%
          #select(!!sym(q_lagpos_name) ,!!sym(r_lagpos_name))
        df2 <- df2%>%
          mutate(!!name_new_col2 := !!sym(q_lagpos_name) *!!sym(r_lagpos_name))
        
        #head(all_features)%>% select(ncol(all_features))
        #select(-contains("lag_1_avg"))
      }
    }
    
    

  }
  
  #  all_features<-%>%
  #   left_join(all_features_r_q, by="datum")
  return(df2)
}


}

#read havel files
{
  hygiene1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv/hygiene_havel.csv')
  irradiance1 <- data.frame()
  flow_rate1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv/q_havel.csv', col_types = cols())
  WWTPs_Flow_rate1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv/ka_havel.csv')
  rain1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_TestPackage_Berlin/Havel/DATA_preprocessed_csv/r_havel.csv')
  #change isar column datum to date format
}  


#read isar files
{
  hygiene1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Isar/DATA_preprocessed_csv/hygiene_marzling.csv')
  irradiance1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Isar/DATA_preprocessed_csv/i_isar.csv')
  flow_rate1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Isar/DATA_preprocessed_csv/q_isar.csv', col_types = cols())
  WWTPs_Flow_rate1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Isar/DATA_preprocessed_csv/ka_isar.csv')
  rain1 <- read_csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Isar/DATA_preprocessed_csv/r_isar.csv')
  #change isar column datum to date format
}  

  


  #read ilz files
{
    hygiene1 <- read.csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Ilz/DATA_preprocessed_csv/hygiene_fischhaus.csv')
    irradiance1 <- read.csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Ilz/DATA_preprocessed_csv/i_ilz.csv')
    flow_rate1 <- read.csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Ilz/DATA_preprocessed_csv/q_ilz.csv')
    WWTPs_Flow_rate1 <- read.csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Ilz/DATA_preprocessed_csv/ka_ilz.csv')
    rain1 <- read.csv('/Users/heiko.langer/Masterarbeit_lokal/Data_preprocess/Daten_Bayern/Ilz/DATA_preprocessed_csv/r_ilz.csv')
}
  
  #change ilz column datum to date format
temp_sys_boundary <- temporal_system_boudary(hygiene1)
#abc <- hygiene1%>% right_join(rain1)

#lag_rain<-lag_variable_and_bind_to_df(abc, 10)


#cor(lag_rain, use = "pairwise.complete.obs",method = c("spearman"))

lags = 6
pearson = F


processed_data_list <- preprocess_data(hygiene1, irradiance1, flow_rate1,WWTPs_Flow_rate1,rain1, lags)
all_features_removed_duplicates <- processed_data_list[[1]]
split <- processed_data_list[[2]]
train_mean <- processed_data_list[[3]]
train_sd <- processed_data_list[[4]]
test_mean <- processed_data_list[[5]]
test_sd <- processed_data_list[[6]]

#check with id
#train_dataset <-all_features_removed_duplicates[split,]
#test_dataset <-all_features_removed_duplicates[-split,]



#remove duplicates
#all_features_removed_duplicates<-all_features%>% group_by(datum)%>%
#  summarize_all(funs(mean))

#optional pearson
if (pearson==T) {
  
  #do correlation
  b<- as.matrix(all_features_removed_duplicates[-1])
  correlation_matrix<-cor(b, use = "pairwise.complete.obs",method = c("spearman"))
  
  sys_boundary<-temporal_system_boudary(all_features_removed_duplicates)
  correlation_matrix<-as.data.frame(correlation_matrix)
  
  pearson_correlation_true<-correlation_matrix$e.coli == T
  positive_correlation_true<-correlation_matrix[pearson_correlation_true]
  
  names_positive_correlation<-names(positive_correlation_true)
  
  all_features_removed_duplicates<-all_features_removed_duplicates%>%select(!!names_positive_correlation)


}
#all_features1<-combine_q_rain(all_features)
#standardize everything




save_preprocessed_df(all_features_removed_duplicates,'Isar_stand_new')

