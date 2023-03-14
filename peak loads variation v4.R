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
library(fitdistrplus)
library(nortest)
library(EnvStats)

set.seed(1)
setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

data_c <- read_csv("~/Energy data analysis coursework/data/Matrix/control_clean.csv")
data_t <- read_csv("~/Energy data analysis coursework/data/Matrix/ToU_clean.csv")

data_c_kw <- data_c%>%
  pivot_longer(!DateTime)%>%
  mutate(kw = value/0.5)%>%
  dplyr::select(-value)%>%
  pivot_wider(names_from = names,values_from = kw)

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
    summarise(max_kwh = max(kwh,rm.na=TRUE))%>%
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
    filter(house %in% list(5,10,20,50))
  
  return(data_max_cum)
}



repeat_calc <- function(num_of_homes,data_type,period){
  
  data_sample <- data_generation(num_of_homes,data_type)
  
  data_period <- data_sample%>%
    mutate(period = floor_date(DateTime,unit = period))%>%
    dplyr::select(period)%>%
    summarise(period = unique(period))
  
  metrics <- metrics_calc(data_sample,period)
  
  ##adding periods back into each list of metrics
  for (i in 1:nrow(data_period)){
    metrics[[i]]<- metrics[[i]]%>%
      mutate(date = data_period$period[i])
  }
  
  metrics_joined <- metrics%>%
    rbindlist()
  return(metrics_joined)
}



###variables ###
num_of_homes<-3
data_type <- "control"
period <- "season"

tic("overall")
test_data1 <- repeat_calc(num_of_homes,data_type,period)
toc()

test_data1%>%
  mutate(house = as.factor(house))%>%
  filter(name=="div_max",
         date < as.POSIXct("2014-01-01"))%>%
  ggplot(aes(date,value, colour = house))+
  geom_point()+
  labs(x="",y="Diversified maximum demand [kWh]")

tic("overall")
for (i in 1:40){
  data <- repeat_calc(num_of_homes,data_type,period)
  if(i==1){
    data_store_daily <- data
  }
  else{
    data_store_daily <- rbind(data_store_daily,data)
  }
}
toc()

#### testing 3 homes

data_3_homes <- data_generation(3,"control")

data_3_homes_1<- data_3_homes%>%
  pivot_longer(!DateTime)%>%
  mutate(period = floor_date(DateTime,"season"))

p<- ggplot(data_3_homes_1%>%filter(name=="HH1965"),aes(DateTime,value,colour=period))+
  geom_line()

ggplotly(p)

data_3_homes_metric<- metrics_calc(data_3_homes,"season")

####test plots####
data_store_daily%>%
  filter(name=="coinc",house==50)%>%
  group_by(date)%>%
  summarise(avg_coinc = mean(value))%>%
  ggplot(aes(date,avg_coinc))+
  geom_point()

data_store_daily%>%
  filter(name=="coinc",house==50)%>%
  mutate(date = as.factor(date))%>%
  ggplot(aes(date,value))+
  geom_boxplot()

data_store%>%
  filter(name=="coinc",house == 50)%>%
  group_by(date)%>%
  summarise(avg_admd = mean(value))%>%
  ggplot(aes(x=date,y=avg_admd))+
  geom_point()

data_store%>%
  filter(name=="coinc",house == 50)%>%
  mutate(date=as.factor(date))%>%
  ggplot(aes(x=date,y=value))+
  geom_boxplot()+
  coord_flip()+
  labs(x="",y="Coincidence factor")

data_store%>%
  filter(name=="div_max",house == 50,date==as.POSIXct("2013-12-01"))%>%
  mutate(date = as.factor(date))%>%
  ggplot(aes(x=value, fill = date))+
  geom_histogram(bins=30)+
  labs(x="Coincidence factor")
  
data_store%>%
  filter(name=="coinc")%>%
  mutate(date = as.factor(date))%>%
  ggplot(aes(x=value, fill = date))+
  geom_density()+
  facet_wrap(~date)+
  labs(x="Diversified maximum demand [kWh]")
