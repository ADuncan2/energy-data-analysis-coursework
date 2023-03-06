library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(tidyverse)
library(plotly)
library(data.table)
library(arrow)
library(readr)
library(gganimate)
library(ggridges)
library(lubridate)
library(goft)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

data_c <- read_csv("~/Energy data analysis coursework/data/Matrix/MatrixC.csv")
data_t <- read_csv("~/Energy data analysis coursework/data/Matrix/MatrixT.csv")



data_t <- data_t%>%
  dplyr::select(-...1)

### slimmed down version of functions for quicker repeats

rep_for_param <- function(data_type_rep,num_homes_rep){
  data_rep <- data_generation(num_homes_rep,data_type_rep)
  
  data_rep1 <- data_rep%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")
  
  data_max_ind <- data_rep1%>%
    group_by(house)%>%
    summarise(max_kwh = max(kwh))%>%
    summarise(sum_max_kwh = sum(max_kwh))
  
  data_max_cum <- data_rep1%>%
    group_by(DateTime)%>%
    summarise(sum_kwh = sum(kwh))%>%
    summarise(max_sum_kwh = max(sum_kwh))%>%
    bind_cols(data_max_ind)%>%
    mutate(coinc = max_sum_kwh/sum_max_kwh,
           admd = 1/coinc,
           div_max = max_sum_kwh/num_homes_rep)%>%
    dplyr::select(coinc,admd,div_max)
  return(data_max_cum)
}

num_homes_rep <- 20
data_type_rep <- "control"

tic("overall")
for (i in 1:10){
  data <- rep_for_param(data_type_rep,num_homes_rep)

  if(i==1){
    data_store_rep <- data
  }
  else{
    data_store_rep <- rbind(data_store_rep,data)
  }
}
toc()





### setting the sample size


data_generation <- function(num_of_homes,data_type){
  num <- num_of_homes

  if (data_type == "test") {
    num_cols <- ncol(data_t)
    ran_sample <- sample(2:num_cols,num,replace = FALSE)
    data_time <- data_t[,1]
    data_sample <- data_t[,ran_sample]
    data_end <- cbind(data_time,data_sample)
    data_end[is.na(data_end)]<-0 #note this, it's potentially a big assumption
  } else {
    num_cols <- ncol(data_c)
    ran_sample <- sample(2:num_cols,num,replace = FALSE)
    data_time <- data_c[,1]
    data_sample <- data_c[,ran_sample]
    data_end <- cbind(data_time,data_sample)
    data_end[is.na(data_end)]<-0 #note this, it's potentially a big assumption
  }
  
  ## partition based on period
  return(data_end)}

metrics_calc <- function(data,period){
  data_period <- data%>%
    mutate(period = floor_date(DateTime,unit = period))%>%
    group_split(period)
  
  test_list<-lapply(data_period,coinc_admd_calc)
  
  return(test_list)
}


coinc_admd_calc <- function(data){
  data <- data%>%
    dplyr::select(-period)
  
  data_names <- as.data.frame(colnames(data)[2:(num_of_homes+1)])%>%
    mutate(house = 1:num_of_homes)
  
  #changing names to numbers to ensure order of houses is kept consistent when cumuliative sum is applied
  colnames(data)<- c("DateTime",data_names$house)

  data_max_ind <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    mutate(house = as.numeric(house))%>%
    group_by(house)%>%
    summarise(max_kwh = max(kwh))%>%
    mutate(max_indi = cumsum(max_kwh))%>%
    dplyr::select(-max_kwh)

  
  data_max_cum <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    mutate(house = as.numeric(house))%>%
    group_by(DateTime)%>%
    mutate(cumsum = cumsum(kwh))%>%
    dplyr::select(-kwh)%>%
    ungroup()%>%
    group_by(house)%>%
    summarise(max_cum = max(cumsum))%>%
    right_join(data_max_ind,by="house")%>%
    mutate(admd=max_indi/max_cum, coinc = 1/admd,div_max = max_cum/house)%>%
    dplyr::select(admd,coinc,div_max)%>%
    mutate(house = 1:num_of_homes)%>%
    dplyr::arrange(house,admd,coinc,div_max)%>%
    pivot_longer(!house)%>%
    filter(house %in% list(1,3,10,50,100,150))
  
  return(data_max_cum)
}


###variables ###
num_of_homes<-200
data_type <- "control"
period <- "month"



repeat_calc <- function(num_of_homes,data_type,period){
  tic("sample gen")
  data_sample <- data_generation(num_of_homes,data_type)
  toc()
  
  data_period <- data_sample%>%
    mutate(period = floor_date(DateTime,unit = period))%>%
    dplyr::select(period)%>%
    summarise(period = unique(period))
  
  tic("metrics")
  metrics <- metrics_calc(data_sample,period)
  toc()
  
  ##adding periods back into each list of metrics
  for (i in 1:nrow(data_period)){
    metrics[[i]]<- metrics[[i]]%>%
      mutate(date = data_period$period[i])
  }
  
  metrics_joined <- metrics%>%
    rbindlist()
  return(metrics_joined)
}

tic("overall")
test_data <- repeat_calc(num_of_homes,data_type,period)
toc()

tic("overall")
for (i in 1:40){
  data <- repeat_calc(num_of_homes,data_type,period)
  if(i==1){
    data_store <- data
  }
  else{
    data_store <- rbind(data_store,data)
  }
}
toc()


####test plots####
data_store%>%
  filter(name=="div_max",house == 100,date>as.POSIXct("2013-01-01"))%>%
  group_by(date)%>%
  summarise(avg_admd = mean(value))%>%
  ggplot(aes(x=date,y=avg_admd))+
  geom_point()


data_store%>%
  filter(name=="div_max",house == 100,date==as.POSIXct("2013-12-01"))%>%
  mutate(date = as.factor(date))%>%
  ggplot(aes(x=value, fill = date))+
  geom_histogram(bins=5)
  
