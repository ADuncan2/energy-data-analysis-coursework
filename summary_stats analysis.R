library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(plotly)
library(data.table)

setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

data_sum <- read.csv("summary_stats.csv")

data%>%
  select(-start,-end,-obs)%>%
  pivot_longer(!LCLid, names_to = "names",values_to = "values")%>%
  group_by(names)%>%
  ggplot(aes(y=names,x=values))+
