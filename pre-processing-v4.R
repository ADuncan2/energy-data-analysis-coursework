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
#### Justification of 2013 focus ####
data_control <- read_csv("~/Energy data analysis coursework/data/Matrix/Control.csv")

data_control2<- data_control

data_times <- data_control2$DateTime

data_control2<- data_control2%>%
  dplyr::select(-DateTime)
data_control2[!is.na(data_control2)]<-1
data_control2[is.na(data_control2)]<-0

data_control2$DateTime <- data_times



sum_dat_cont <- data_control2%>%pivot_longer(!DateTime)%>%
  group_by(DateTime)%>%
  summarise(sum = sum(value))

ggplot(sum_dat_cont,aes(DateTime,sum))+
  geom_point()+
  labs(y="Number of properties in Control group with non-NA values",x="")


data_tou <- read_csv("~/Energy data analysis coursework/data/Matrix/ToU.csv")
data_times_tou <- data_tou$DateTime

data_tou_1<- data_tou%>%
  dplyr::select(-DateTime)
data_tou_1[!is.na(data_tou_1)]<-1
data_tou_1[is.na(data_tou_1)]<-0

data_tou_1$DateTime <- data_times_tou



sum_dat_tou <- data_tou_1%>%pivot_longer(!DateTime)%>%
  group_by(DateTime)%>%
  summarise(sum = sum(value))

ggplot(sum_dat_tou,aes(DateTime,sum,colour = "ToU"))+
  geom_point()+
  geom_point(data=sum_dat_cont,aes(DateTime,sum,colour = "Control"))+
  labs(y="Number of properties with non-NA values",x="")

#### Identifying homes that have end dates 
#ToU
tou_test<- data_tou_1%>%
  filter(DateTime == as.POSIXct("2014-01-01"))%>%
  pivot_longer(!DateTime)%>%
  filter(value == 1)%>%
  dplyr::select(name)

sum_dat_tou_2 <- data_tou%>%pivot_longer(!DateTime)%>%
  filter(name %in% tou_test$name,
         DateTime>= as.POSIXct("2013-01-01"))%>%
  group_by(DateTime)%>%
  summarise(sum = sum(value))

sum_dat_tou_3<- sum_dat_tou_2%>%
  mutate(perc = sum/max(sum))
  
ggplot(sum_dat_tou_3,aes(DateTime,sum,colour = "ToU"))+
  geom_point()

#Control
control_test<- data_control2%>%
  filter(DateTime == as.POSIXct("2014-01-01"))%>%
  pivot_longer(!DateTime)%>%
  filter(value == 1)%>%
  dplyr::select(name)

sum_dat_control_2 <- data_control2%>%pivot_longer(!DateTime)%>%
  filter(name %in% control_test$name,
         DateTime>= as.POSIXct("2013-01-01"))%>%
  group_by(DateTime)%>%
  summarise(sum = sum(value))

sum_dat_control_3<- sum_dat_control_2%>%
  mutate(perc = sum/max(sum))


ggplot(sum_dat_control_3,aes(DateTime,perc,colour = "Control"))+
  geom_point()+
  geom_point(data=sum_dat_tou_3,aes(DateTime,perc,colour = "ToU"))

## percentage covarge of homes
#control 
nrow(control_test)/(ncol(data_control)-1)
#91.2% of homes

#ToU
nrow(tou_test)/(ncol(data_tou)-1)
#92.7 # of homes

#### re-saving cleaned data ####
#control
data_control_clean <- data_control%>%
  filter(DateTime>= as.POSIXct("2013-01-01"))%>%
  dplyr::select(DateTime, control_test$name)

write_csv(data_control_clean,"~/Energy data analysis coursework/data/Matrix/control_clean.csv")

data_tou_clean <- data_tou%>%
  filter(DateTime>= as.POSIXct("2013-01-01"))%>%
  dplyr::select(DateTime, tou_test$name)

write_csv(data_tou_clean,"~/Energy data analysis coursework/data/Matrix/ToU_clean.csv")
