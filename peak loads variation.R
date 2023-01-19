library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(plotly)
library(data.table)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

tic()
unlink("max_load.csv")
for (j in 1:7){
  x <- "~/Energy data analysis coursework/data/LCL-June2015v2_"
  num <- as.character(j)
  name<- paste(x,num,".csv",sep="")
  data <- read.csv(name)

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
    summarise(number = i+1-min_id,max = max(kWh_sum),max_norm = max/number)
  datalist[[i+1-min_id]]<-data2
  }
  
  big_data = do.call(rbind, datalist)
  
  big_data1<- big_data%>%
    group_by(number)%>%
    summarise(max = max(max_norm))
  
  big_data1$batch <- j
  
  fwrite(big_data1, file = "max_load.csv", sep = ",",
         append = TRUE)
}
toc()

max_load <- read.csv("max_load.csv")

max_load%>%
  mutate(batch = as.factor(batch))%>%
  ggplot(aes(number,max,color = batch))+
  geom_point()+
  labs(x="Number of properties",y="Maximum normalised coincident load [kWh]")
