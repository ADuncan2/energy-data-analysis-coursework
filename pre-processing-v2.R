library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(plotly)
library(data.table)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")


#creating a for loop to open each individual file, change the data types
# individual parts of this are explained in more detail below

tic("summary")
for (i in 0:134){
  x <- "data/LCL-June2015v2_"
  num <- as.character(i)
  name<- paste(x,num,".csv",sep="")
  data <- read.csv(name)
  data1<- data %>%
    rename(KWh = KWH.hh..per.half.hour.)%>%
    mutate(KWh1 = as.numeric(as.character(KWh)),
           datetime = as.POSIXct(DateTime,"%Y-%m-%d %H:%M:%S",tz="GMT"),
           time = format(datetime,"%H:%M:%S"),
           month = months.POSIXt(datetime))%>%
    group_by(LCLid)
  
  data_sum <- data1 %>%
    group_by(LCLid, KWh1)%>%
    summarise(obs = n())%>%
    filter(KWh1 %in% c(0,NA))%>%
    pivot_wider(names_from = KWh1,
                values_from = obs)%>%
    rename(zeros = `0`,NAs= `NA`)
  
  data_sum1 <- data1 %>%
    group_by(LCLid)%>%
    summarise(obs = n(),
              start = min(datetime),
              end = max(datetime))%>%
    mutate(lost_time = ((as.numeric(end)-as.numeric(start))/1800)-obs)
  
  data_sum2 <- left_join(data_sum,data_sum1)
  
  fwrite(data_sum2, file = "summary_stats.csv", sep = ",",
         append = TRUE)
}
toc()

tic("total")
data <- read.csv("~/Energy data analysis/data/LCL-June2015v2_17.csv")

#changes kwh data to numeric from factor (comes in from csv as str)
#datetime stored as POSIX
#Reformatting of date to extract particular aspects

tic("data cleaning")
data1<- data %>%
  rename(KWh = KWH.hh..per.half.hour.)%>%
  mutate(KWh1 = as.numeric(as.character(KWh)),
         datetime = as.POSIXct(DateTime,"%Y-%m-%d %H:%M:%S",tz="GMT"),
         time = format(datetime,"%H:%M:%S"),
         month = months.POSIXt(datetime))%>%
  group_by(LCLid)
toc()

data2<- data1%>%
  filter(LCLid == 492)%>%
  mutate(count = 1:n())

data2%>%
  ggplot(aes(count,datetime))+
  geom_line()

plot_ly((data2%>%filter(LCLid==492)), type = 'scatter', mode = 'lines')%>%
  add_trace(y = ~datetime, x = ~count, name = 'count')%>%
  layout(showlegend = F)%>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)



data_sum <- read.csv("summary_stats.csv")

toc()

#data grouped by property ID and 
data_sum <- data1 %>%
  group_by(LCLid, KWh1)%>%
  summarise(obs = n())%>%
  filter(KWh1 %in% c(0,NA))%>%
  pivot_wider(names_from = KWh1,
              values_from = obs)%>%
  rename(zeros = `0`,NAs= `NA`)

data_sum1 <- data1 %>%
  group_by(LCLid)%>%
  summarise(obs = n(),
            start = min(datetime),
            end = max(datetime))%>%
  mutate(lost_time = ((as.numeric(end)-as.numeric(start))/1800)-obs)

data_sum2 <- left_join(data_sum,data_sum1)

data1%>%
  filter(LCLid==3)%>%
  ggplot(aes(datetime,KWh1))+
  geom_line()

plot_ly((data1%>%filter(LCLid==5)), type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~datetime, y = ~KWh1, name = 'KWh')%>%
  layout(showlegend = F)%>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', width = 900)



tic("plotting")
data1%>%
  filter(LCLid == 0)%>%
  mutate(year = format(datetime,"%Y"))%>%
  filter(year == "2013",month=="December")%>%
  group_by(time)%>%
  ggplot(aes(time,KWh1))+
  stat_halfeye()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
toc()
