library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(plotly)
library(data.table)
library(arrow)
library(ggalt)
library(forcats)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

name <- '~/Energy data analysis coursework/Parquet_Format/Control_gzip/File1.gzip'
data <- read_parquet(name) 

data1<- data %>%
  rename(KWh = `KWH/hh (per half hour) `)%>%
  mutate(KWh1 = as.numeric(as.character(KWh)),
         datetime = as.POSIXct(DateTime,"%Y-%m-%d %H:%M:%S",tz="GMT"),
         time = format(datetime,"%H:%M:%S"),
         month = months.POSIXt(datetime),
         date = format(datetime,"%y-%m-%d"),
         year = format(datetime,"%Y"))

#number of half hourly observations
data2<- data1 %>%
  group_by(date)%>%
  summarise(n = n(),test = if_else(n >= 48,TRUE,FALSE))%>%
  mutate(date1 = as.Date(date,format="%y-%m-%d"))

data2%>%
  ggplot(aes(date1,n))+
  geom_point()

#number of houses in the sample
data3 <- data1%>%
  select(LCLid,date)%>%
  mutate(date1 = as.Date(date,format="%y-%m-%d"))%>%
  distinct()%>%
  group_by(date1)%>%
  summarise(n = n())

data3%>%
  ggplot(aes(date1,n))+
  geom_point()

#start and end dates 
data4<- read_csv_arrow("summary_stats.csv")%>%
  mutate(period = as.numeric(end-start)/86400)

#density of period of study
ggplot(data4,aes(period))+
  geom_histogram(binwidth = 10)+
  labs(x="Days between first and last measurements")

#
data5<- data4%>%
  mutate(LCLid = as.factor(LCLid),
         date = format(start,"%y-%m-%d"),
         LCLid = fct_reorder(LCLid,date))
data5%>%
  ggplot(aes(y=LCLid,x=date))+
  geom_point()+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE))

data4%>%
  ggplot(aes(y=LCLid,x=start,xend=end))+
  geom_dumbbell(size=1, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744")+
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))
