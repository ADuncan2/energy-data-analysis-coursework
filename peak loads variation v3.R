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

####log normal testing experiment####
ran <- sample(2:4445,1)
data_norm_h <- data_c[,ran]
colnames(data_norm_h)<- "value"
random_500 <- sample(1:39727,5000)
lnorm <- lnorm_test(data_norm_h$value[random_500])

ggplot(data_norm_h,aes(value))+
  geom_histogram(bins=20)

num_house <- 1000
lnorm <- data.frame("house" = 1:num_house)
for (i in 2:num_house){
  data_norm_h <- data_c[,i]
  colnames(data_norm_h)<- "value"
  random_500 <- sample(1:39727,5000)
  res<-lnorm_test(data_norm_h$value[random_500])

  lnorm[i,2] <- res$p.value
}

ggplot(lnorm,aes(V2))+
  geom_histogram()


#### metric calculation ####
run_test <- function(num){
  num_of_homes <-num
  num_cols <- ncol(data_t)
  ran_sample <- sample(3:num_cols,num_of_homes,replace = FALSE)
  
  #finding sample of data and max kwh in study period
  data_time <- data_t[,2]
  data_sample <- data_t[,ran_sample]
  colnames(data_sample)<-as.numeric(1:num_of_homes)
  data <- cbind(data_time,data_sample)
  data[is.na(data)]<-0
  
  data_max_ind <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    group_by(house)%>%
    summarise(max_kwh = max(kwh))%>%
    mutate(house = as.numeric(house))%>%
    arrange(house,max_kwh)%>%
    mutate(max_indi = cumsum(max_kwh))%>%
    select(-max_kwh)
  
  data_max_cum <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    group_by(DateTime)%>%
    mutate(cumsum = cumsum(kwh))%>%
    select(-kwh)%>%
    ungroup()%>%
    group_by(house)%>%
    summarise(max_cum = max(cumsum))%>%
    mutate(house=as.numeric(house))%>%
    arrange(house,max_cum)%>%
    right_join(data_max_ind,by="house")%>%
    mutate(admd=max_indi/max_cum, coinc = 1/admd)%>%
    select(admd,coinc)
  
  return(data_max_cum)
}


run_control <- function(num){
  num_of_homes <-num
  ran_sample <- sample(2:4445,num_of_homes,replace = FALSE)
  
  #finding sample of data and max kwh in study period
  data_time <- data_c[,1]
  data_sample <- data_c[,ran_sample]
  colnames(data_sample)<-as.numeric(1:num_of_homes)
  data <- cbind(data_time,data_sample)
  data[is.na(data)]<-0
  
  data_max_ind <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    group_by(house)%>%
    summarise(max_kwh = max(kwh))%>%
    mutate(house = as.numeric(house))%>%
    arrange(house,max_kwh)%>%
    mutate(max_indi = cumsum(max_kwh))%>%
    select(-max_kwh)
  
  data_max_cum <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    group_by(DateTime)%>%
    mutate(cumsum = cumsum(kwh))%>%
    select(-kwh)%>%
    ungroup()%>%
    group_by(house)%>%
    summarise(max_cum = max(cumsum))%>%
    mutate(house=as.numeric(house))%>%
    arrange(house,max_cum)%>%
    right_join(data_max_ind,by="house")%>%
    mutate(admd=max_indi/max_cum, coinc = 1/admd)%>%
    select(admd,coinc)
  
  return(data_max_cum)
}

##test group
tic()
num_of_props<- 200 
num_of_runs <- 50
multi_runs_test <- data.frame("house" = 1:num_of_props)

#clearing old data
rm(results)

for (i in 1:num_of_runs){
  results <- run_test(num_of_props)
  coinc_name <- paste("coinc",i)
  admd_name <- paste("admd",i)
  colnames(results)<- c(admd_name,coinc_name)
  multi_runs_test<- cbind(multi_runs_test,results)
}
toc()

multi_runs_test2<- multi_runs_test%>%
  pivot_longer(!house,names_to = "metric",values_to = "values")%>%
  filter(grepl("coinc",metric))%>%
  group_by(metric)


ggplot(multi_runs_test2,aes(x=house,y=values,color = metric))+
  geom_point()

multi_runs_test2%>%
  filter(house%%10==0)%>%
  mutate(house = as.factor(house))%>%
  ggplot(aes(x = values, y = house)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = F) +
  guides(fill = F)+
  labs(x="Demand diversity factor",y="Number house houses in sample",
       title = "Density of demand diveristy factor for 100 runs of 200 randomly sampled homes")



##control group
tic()
num_of_props<- 20 
num_of_runs <- 1000
multi_runs_cont <- data.frame("house" = 1:num_of_props)

#clearing old data
rm(results)

for (i in 1:num_of_runs){
  results <- run_control(num_of_props)
  coinc_name <- paste("coinc",i)
  admd_name <- paste("admd",i)
  colnames(results)<- c(admd_name,coinc_name)
  multi_runs_cont<- cbind(multi_runs_cont,results)
}
toc()

multi_runs_cont2<- multi_runs_cont%>%
  pivot_longer(!house,names_to = "metric",values_to = "values")%>%
  filter(grepl("admd",metric))%>%
  group_by(metric)

multi_runs_cont2 %>% 
  filter(house==20)%>%
  ggplot(aes(x=values))+
  geom_histogram(bins=20)+
  labs(x="Diversity factor",title = "1000 results of 20 randomly selected homes in control group")

ggplot(multi_runs_cont2,aes(x=house,y=values,color = metric))+
  geom_point()


multi_runs_cont2%>%
  ggplot(aes(x=values))+
  geom_density()

multi_runs_cont2%>%
  filter(house%%2==0)%>%
  mutate(house = as.factor(house))%>%
  ggplot(aes(x = values, y = house)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = F) +
  guides(fill = F)+
  labs(x="Demand diversity factor",y="Number house houses in sample",
       title = "Density of demand diveristy factor for 1000 runs of 20 randomly sampled control homes")


#### weekly metric calculation

run_weekly_test <- function(num){
  #finding sample of data and max kwh in study period
  num_of_homes <-  num
  num_cols <- ncol(data_t)
  ran_sample <- sample(3:num_cols,num_of_homes,replace = FALSE)
  
  data_time <- data_t[,2]
  data_sample <- data_t[,ran_sample]
  colnames(data_sample)<-as.numeric(1:num_of_homes)
  data <- cbind(data_time,data_sample)
  
  data[is.na(data)]<-0
  
  data_max_ind <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    mutate(date = floor_date(DateTime,unit = "week"))%>%
    group_by(date)%>%
    summarise(max_kwh = max(kwh))
  
  data_max_cum <- data%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    group_by(DateTime)%>%
    summarise(sum_kwh = sum(kwh))%>%
    mutate(date = floor_date(DateTime,unit = "week"))%>%
    group_by(date)%>%
    summarise(max_cum = max(sum_kwh))%>%
    right_join(data_max_ind,by="date")%>%
    mutate(admd=max_kwh/max_cum, coinc = 1/admd)%>%
    ungroup()%>%
    dplyr::select(admd,coinc)
  
  return(data_max_cum)
}

tic()
num_of_props<- 20 
num_of_runs <- 1
multi_runs_week <- data_t[,2]%>%
  mutate(date = floor_date(DateTime,unit = "week"))%>%
  dplyr::select(-DateTime)%>%
  distinct()

#clearing old data
rm(results)

for (i in 1:num_of_runs){
  results <- run_weekly_test(num_of_props)
  coinc_name <- paste("coinc",i)
  admd_name <- paste("admd",i)
  colnames(results)<- c(admd_name,coinc_name)
  multi_runs_week<- cbind(multi_runs_week,results)
}
toc()

multi_runs_week2<- multi_runs_week%>%
  pivot_longer(!date)%>%
  filter(grepl("admd",name))

ggplot(multi_runs_week2,aes(x=date,y=value,color = name))+
  geom_point()

