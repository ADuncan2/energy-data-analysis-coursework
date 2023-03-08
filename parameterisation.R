library(dplyr)
library(tictoc)
library(ggplot2)
library(ggdist)
library(tidyr)
library(tidyverse)
library(plotly)
library(data.table)
library(arrow)
library(readr)
library(gganimate)
library(ggridges)
library(lubridate)
library(goft)
library(fitdistrplus)
library(nortest)
library(EnvStats)

set.seed(1)
setwd("~/Energy data analysis coursework/energy-data-analysis-coursework")

data_c <- read_csv("~/Energy data analysis coursework/data/Matrix/control_clean.csv")
data_t <- read_csv("~/Energy data analysis coursework/data/Matrix/ToU_clean.csv")

data_t <- data_t%>%
  dplyr::select(-...1)


### slimmed down version of functions for quicker repeats
data_generation <- function(num_of_homes,data_type){
  num <- num_of_homes
  
  if (data_type == "test") {
    num_cols <- ncol(data_t)
    ran_sample <- sample(2:num_cols,num,replace = FALSE)
    data_time <- data_t[,1]
    data_sample <- data_t[,ran_sample]
    data_end <- cbind(data_time,data_sample)
    data_end[is.na(data_end)]<-0 #note this, it's potentially a big assumption
  } else {
    num_cols <- ncol(data_c)
    ran_sample <- sample(2:num_cols,num,replace = FALSE)
    data_time <- data_c[,1]
    data_sample <- data_c[,ran_sample]
    data_end <- cbind(data_time,data_sample)
    data_end[is.na(data_end)]<-0 #note this, it's potentially a big assumption
  }
  
  ## partition based on period
  return(data_end)}

rep_for_param <- function(data_type_rep,num_homes_rep){
  data_rep <- data_generation(num_homes_rep,data_type_rep)
  
  data_rep1 <- data_rep%>%
    pivot_longer(!DateTime,names_to = "house",values_to = "kwh")
  
  data_max_ind <- data_rep1%>%
    group_by(house)%>%
    summarise(max_kwh = max(kwh))%>%
    summarise(sum_max_kwh = sum(max_kwh))
  
  data_max_cum <- data_rep1%>%
    group_by(DateTime)%>%
    summarise(sum_kwh = sum(kwh))%>%
    summarise(max_sum_kwh = max(sum_kwh))%>%
    bind_cols(data_max_ind)%>%
    mutate(coinc = max_sum_kwh/sum_max_kwh,
           admd = 1/coinc,
           div_max = max_sum_kwh/num_homes_rep)%>%
    dplyr::select(coinc,admd,div_max)
  return(data_max_cum)
}

num_homes_rep <- 20
data_type_rep <- "control"
# DO NOT RUN - only run this if you want to reset everything
data_store_rep<- data.frame(matrix(nrow=0,ncol=3))
colnames(data_store_rep)<- c("coinc","admd","div_max")

time_mins <- 11*60
repeats<- time_mins*60/0.4

tic("overall")
for (i in 1:repeats){
  data <- rep_for_param(data_type_rep,num_homes_rep)
  data_store_rep<-rbind(data_store_rep,data)
}
toc()
##one off
write_csv(data_store_rep,"~/Energy data analysis coursework/data/unclean_bootstrap_results.csv")

ggplot(data_store_rep,aes(coinc))+
  geom_histogram(bins=100)+
  labs(x="Coincidence factor")

## adding lines for different curves

norm<- data.frame(index = 1:136228,val= rnorm(136228))

ggplot(norm,aes(val))+
  geom_histogram(bins=100,aes(y=..density..))

##goodness-of-fit checks
gofTest(data_store_rep$admd,distribution = "lnorm")

##test for normal distribution
FIT_norm <- fitdist(data_store_rep$coinc, "norm")    ## note: it is "norm" not "normal"

plot(FIT_norm)    ## use method `plot.fitdist


FIT_gamma <- fitdist(data_store_rep$coinc, "gamma")    ## note: it is "norm" not "normal"

plot(FIT_gamma)    ## use method `plot.fitdist

FIT_lnorm <- fitdist(data_store_rep$coinc, "lnorm")    ## note: it is "norm" not "normal"

plot(FIT_lnorm)
