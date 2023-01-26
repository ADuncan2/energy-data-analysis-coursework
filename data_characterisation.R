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



data_control <- read_csv_arrow("MatrixC.csv")




### finding null cases for whole control dataset

tic("summary")
for (i in 1:135){
  name <- '~/Energy data analysis coursework/Parquet_Format/Control_gzip/File'
  num <- i
  name<- paste(name,num,".gzip",sep="")
  data <- read_parquet(name)
  data1<- data %>%
    rename(KWh1 = `KWH/hh (per half hour) `)%>%
    mutate(datetime = as.POSIXct(DateTime,"%Y-%m-%d %H:%M:%S",tz="GMT"))
  data_sum <- data1 %>%
    select(LCLid,datetime,KWh1)%>%
    filter(is.na(KWh1))
  
  fwrite(data_sum, file = "summary_NAs.csv", sep = ",",
         append = TRUE)
}
toc()

NAs <- read_csv_arrow("summary_NAs.csv")


NAs1 <- NAs%>%
  mutate(hour = format(datetime, "%H"))%>%
  group_by(hour)%>%
  summarise(n=n())

#NAs plot before sorting
NAs %>%
  mutate(hour = format(datetime, "%H"))%>%
  filter(hour == "15")%>%
  ggplot(aes(x=datetime,y=LCLid))+
  geom_point()

NAs2 <- NAs %>%
  mutate(hour = format(datetime, "%H"),
         LCLid = as.factor(LCLid))%>%
  filter(hour == "15")%>%
  select(LCLid,datetime)%>%
  mutate(LCLid = fct_reorder(LCLid,datetime))

ggplot(NAs2,aes(x=datetime,y=LCLid))+
  geom_point()+
  scale_y_discrete(guide = guide_axis(check.overlap = TRUE))
  

ggplot(NAs2,aes(x=datetime))+
  geom_histogram(bins=800)
  

###
name1 <- '~/Energy data analysis coursework/Parquet_Format/Control_gzip/File2.gzip'
data <- read_parquet(name1) 

data1<- data %>%
  rename(KWh = `KWH/hh (per half hour) `)%>%
  mutate(KWh1 = as.numeric(as.character(KWh)),
         datetime = as.POSIXct(DateTime,"%Y-%m-%d %H:%M:%S",tz="GMT"),
         time = format(datetime,"%H:%M:%S"),
         month = months.POSIXt(datetime),
         date = format(datetime,"%y-%m-%d"),
         year = format(datetime,"%Y"))

data_anom <- data1%>%
  filter(date == "12-04-17")

#number of half hourly observations
data2<- data1 %>%
  group_by(date)%>%
  summarise(n = n())%>%
  mutate(date1 = as.Date(date,format="%y-%m-%d"))

plot1<-data2%>%
  ggplot(aes(date1,n))+
  geom_point()+
  labs(y="number of half hourly periods in File 2",x="")

ggplotly(plot1)

#number of houses in the sample
data3 <- data1%>%
  select(LCLid,date)%>%
  mutate(date1 = as.Date(date,format="%y-%m-%d"))%>%
  distinct()%>%
  group_by(date1)%>%
  summarise(n = n())

data3%>%
  ggplot(aes(date1,n))+
  geom_point()+
  labs(x="",y="Number of properties in File 2 with daily data")

data1_start_end <- data1%>%
  group_by(LCLid)%>%
  summarise(start=min(datetime),
            end = max(datetime))

data1_start_end%>%
  ggplot(aes(y=LCLid,x=start,xend=end))+
  geom_dumbbell(size=1, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744")+
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))+
  labs(y="Property ID from File 2")

data8<- data1%>%
  group_by(LCLid,date)%>%
  summarise(n = n(), count = n -48)

### from summary data of whole data set
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
         time_1 = as.numeric(start),
         LCLid = fct_reorder(LCLid,time_1))

#start date density
ggplot(data5,aes(x=start))+
  geom_histogram(binwidth = 864000)
#end dates density
ggplot(data5,aes(x=end))+
  geom_histogram(binwidth = 864000)

data4%>%
  ggplot(aes(y=period,x=start,xend=end))+
  geom_dumbbell(size=1, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744")+
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))+
  labs(y="Days between first and last measurements")

data4%>%
  filter(end == as.POSIXct("2014-02-28 00:00:00"))%>%
  ggplot(aes(y=period,x=start,xend=end))+
  geom_dumbbell(size=1, color="#e3e2e1",
                colour_x = "#5b8124", colour_xend = "#bad744")+
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05))+
  labs(y="Days between first and last measurements")

data4%>%
  filter(end != as.POSIXct("2014-02-28 00:00:00"))%>%
  ggplot(aes(x=end))+
  geom_histogram(binwidth = 864000)


# experiment of super-clean data

data7<- data4%>%
  filter(is.na(NAs))
