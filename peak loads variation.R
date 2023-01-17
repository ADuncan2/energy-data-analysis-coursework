library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(plotly)
library(data.table)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

data <- read.csv("~/Energy data analysis coursework/data/LCL-June2015v2_2.csv")

data1<- data %>%
  rename(KWh = KWH.hh..per.half.hour.)%>%
  mutate(KWh1 = as.numeric(as.character(KWh)),
         datetime = as.POSIXct(DateTime,"%Y-%m-%d %H:%M:%S",tz="GMT"),
         time = format(datetime,"%H:%M:%S"),
         month = months.POSIXt(datetime),
         date = format(datetime,"%y-%m-%d"))%>%
  group_by(LCLid)



max_id <- max(data1$LCLid)
min_id <- min(data1$LCLid)

n = max_id - min_id

datalist = vector("list", length = n)

for(i in min_id:max_id){
data2<- data1%>%
  filter(LCLid %in% min_id:i)%>%
  group_by(date,time)%>%
  summarise(kWh_sum= sum(KWh1))%>%
  filter(!is.na(kWh_sum))%>%
  summarise(number = i-min_id,max = max(kWh_sum),max_norm = max/number)
datalist[[i+1-min_id]]<-data2
}

big_data = do.call(rbind, datalist)

big_data%>%
  group_by(number)%>%
  summarise(max = max(max))%>%
  ggplot(aes(number,max))+
  geom_point()
