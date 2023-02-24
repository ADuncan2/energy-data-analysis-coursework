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


obvs <- data_t %>%
  pivot_longer(!DateTime)%>%
  group_by(DateTime)%>%
  summarise(num_of_obvs = sum(!is.na(value)))

ggplot(obvs,aes(x=DateTime,y=num_of_obvs))+
  geom_point()

obvs_c <- data_c %>%
  pivot_longer(!DateTime)%>%
  group_by(DateTime)%>%
  summarise(num_of_obvs = sum(!is.na(value)))


obvs_2<- obvs%>%
  left_join(obvs_c,by="DateTime")%>%
  mutate(Test = num_of_obvs.x*100/1123,Control = num_of_obvs.y*100/4444)%>%
  pivot_longer(!DateTime,names_to = "Data",values_to = "num_of_obvs")%>%
  filter(Data %in% c("Test","Control"))

p<-ggplot(obvs_2,aes(x=DateTime,y=num_of_obvs,colour=Data))+
  geom_point()+
  labs(x="",y="% of total properties with data")

ggplotly(p)
