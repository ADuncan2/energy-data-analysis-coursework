library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(plotly)
library(data.table)
library(arrow)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

tic()
unlink("max_load.csv")
for (j in 1:1){
  #extracting the data in managable portions
  tic()
  x <- "~/Energy data analysis coursework/data/LCL-June2015v2_"
  num <- as.character(j)
  name<- paste(x,num,".csv",sep="")
  data <- read.csv(name)
  toc()
  #approx 2s to run
  
  #testing downloading in parquet format
  tic()
  x <- "~/Energy data analysis coursework/Parquet_Format/Control_gzip/File"
  num <- as.character(j)
  name<- paste(x,num,".gzip",sep="")
  data <- read_parquet(name)
  toc()
  #approx 0.4s to run
  
  #cleaning the data by changing formats of data
  data1<- data %>%
    rename(KWh = KWH.hh..per.half.hour.)%>%
    mutate(KWh1 = as.numeric(as.character(KWh)),
           datetime = as.POSIXct(DateTime,"%Y-%m-%d %H:%M:%S",tz="GMT"),
           time = format(datetime,"%H:%M:%S"),
           month = months.POSIXt(datetime),
           date = format(datetime,"%y-%m-%d"),
           year = format(datetime,"%Y"))%>%
    filter(year == 2013)
  
  
  #creating range of property IDs to iterate over
  max_id <- max(data1$LCLid)
  min_id <- min(data1$LCLid)
  
  n = max_id - min_id
  
  #creating data list to save iterative data generation to
  datalist = vector("list", length = n)
  
  #calculating ADMD for different number of households
  for(i in min_id:max_id){
    #calculating maximum demand of all houses
    data2<- data1%>%
      filter(LCLid %in% min_id:i)%>%
      group_by(date,time)%>%
      summarise(kWh_sum= sum(KWh1))%>%
      filter(!is.na(kWh_sum))%>%
      ungroup()%>%
      summarise(count =  i - min_id +1,kwh_sum_max = max(kWh_sum))
  
    #calculating sum of max demand of each house
    data3<- data1%>%
      filter(LCLid %in% min_id:i)%>%
      group_by(LCLid)%>%
      summarise(kwh_peak = max(KWh1))%>%
      ungroup()%>%
      summarise(count =  i - min_id +1,kwh_peak_sum = sum(kwh_peak))%>%
      left_join(data2,by="count")%>%
      mutate(ADMD = kwh_peak_sum/kwh_sum_max)
  
    datalist[[i+1-min_id]]<-data3
  }
  
  big_data = do.call(rbind, datalist)
  
  big_data$batch <- j
  
  fwrite(big_data, file = "max_load.csv", sep = ",",
         append = TRUE)
}
toc()

max_load <- read.csv("max_load.csv")

max_load%>%
  mutate(batch = as.factor(batch))%>%
  ggplot(aes(count,ADMD,color = batch))+
  geom_point()+
  labs(x="Number of properties",y="Maximum normalised coincident load [kWh]")
