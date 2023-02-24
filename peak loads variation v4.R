library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
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

### setting the sample size

reps <- 500
num_of_homes <- 100

metrics_calc <- function(num_of_homes){
  num <- num_of_homes
  num_cols <- ncol(data_t)
  ran_sample <- sample(2:num_cols,num_of_homes,replace = FALSE)
  data_time <- data_t[,1]
  data_sample <- data_t[,ran_sample]
  data <- cbind(data_time,data_sample)
  data[is.na(data)]<-0 #note this, it's potentially a big assumption
  
  ## partition based on period
  period <- "month"
  
  data_period <- data%>%
    mutate(period = floor_date(DateTime,unit = period))%>%
    group_split(period)
  
  
  
  test<-lapply(data_period,coinc_admd_calc)
  
  return(coinc)
}


coinc_admd_calc <- function(data){
  data_names <- as.data.frame(colnames(data)[2:101])%>%
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
    dplyr::select(admd,coinc,div_max)
    
  return(data_max_cum)
}

