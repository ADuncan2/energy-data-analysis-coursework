library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(plotly)
library(data.table)
library(arrow)
library(readr)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

data_c <- read_csv("~/Energy data analysis coursework/data/Matrix/MatrixC.csv")

num_of_homes <-100

ran_sample <- sample(0:4445,num_of_homes,replace = FALSE)

#finding sample of data and max kwh in study period
data_time <- data_c[,1]
data_sample <- data_c[,ran_sample]
data <- cbind(data_time,data_sample)



admd<- data.frame()

tic()
for (i in 1:num_of_homes){
  data_max_tot <- data[,1:(i+1)]%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    group_by(house)%>%
    filter(!is.na(kwh))%>%
    summarise(max_kwh=max(kwh))%>%
    ungroup()%>%
    summarise(max_indi_kwh = sum(max_kwh))

  data_div_tot <- data[,1:(i+1)]%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
    group_by(DateTime)%>%
    filter(!is.na(kwh))%>%
    summarise(sum_kwh=sum(kwh))%>%
    ungroup()%>%
    summarise(div_max_kwh = max(sum_kwh))
  admd[i,1]<-i
  admd[i,2]<-data_max_tot$max_indi_kwh/data_div_tot$div_max_kwh
}
toc()
admd<- admd%>%
  rename("index"=V1,"admd"=V2)%>%
  mutate(coinc = 1/admd)


plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

plot <- ggplot(admd,aes(x=index,y=coinc))+
  geom_point()+
  labs(x="Number of properties (randomly selected)",
       y="After diversity maximum demand")

ggplotly(plot)

#investigating a single homes
#home ID
investigate <- 22

data_sample1 <- data_c[,ran_sample[investigate]]
data1 <- cbind(data_time,data_sample1)

inv<- data1%>%
  pivot_longer(!DateTime,names_to = "house",values_to = "kwh")%>%
  ggplot(aes(DateTime,kwh,color = house))+
  geom_point()

ggplotly(inv)

#distribution of average half hourly measurements in control group
avg_hh_kwh<- data.frame()

sample_of_homes <- 1000
tic()
for (i in 1:sample_of_homes){
  data_avg <- data_c[,i+1]
  data_tot<-cbind(data_time,data_avg)
  name <- colnames(data_tot)[2]
  
  colnames(data_tot)<-c("time","home")
  
  data_avg_tot <- data_tot %>%
    filter(!is.na(home))%>%
    summarise(sum_kwh = sum(home),n = n(),avg_kwh = sum_kwh/n)
  
  avg_hh_kwh[i,1]<-name
  avg_hh_kwh[i,2]<- data_avg_tot$avg_kwh
}
toc()

ggplot(avg_hh_kwh,aes(x=V2))+
  geom_histogram(bins = 50)


#distribution of average half hourly measurements in control group
max_hh_kwh<- data.frame()

sample_of_homes <- 4445
tic()
for (i in 1:sample_of_homes){
  data_max <- data_c[,i+1]
  data_tot_max<-cbind(data_time,data_max)
  name <- colnames(data_tot)[2]
  
  colnames(data_tot_max)<-c("time","home")
  
  data_avg_tot <- data_tot_max %>%
    filter(!is.na(home))%>%
    summarise(max_kwh = max(home))
  
  max_hh_kwh[i,1]<-name
  max_hh_kwh[i,2]<- data_avg_tot$max_kwh
}
toc()

ggplot(max_hh_kwh,aes(x=V2))+
  geom_histogram(bins = 50)
