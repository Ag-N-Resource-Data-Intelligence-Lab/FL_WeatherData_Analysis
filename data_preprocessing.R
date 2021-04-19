#install.packages('lubridate')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('magrittr')
#install.packages('RcppRoll')
#install.packages('wrapr')
#install.packages('sqldf')

Libs=c('lubridate','dplyr','tidyr','magrittr','RcppRoll','wrapr','data.table')
lapply(Libs,library, character.only = TRUE)

DtF=read.csv('C:/Users/Chi Zhang/Desktop/JAX.csv', sep=',',header=T,stringsAsFactors = F) %>%
  mutate(Time=ymd_hms(Time))

DtF %>% 
  arrange(Time) %>% 
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>% 
  group_by(TimeLag_min) %>% 
  tally %>% 
  rename(Num_lag=n)

DtF %>%
  ggplot()+
  geom_point(aes(Time,Precip_mm))


source('https://raw.githubusercontent.com/ZyuAFD/SWRE_General_R_Functions/master/src/Regulate%205%20min.R')
interval= 60

DtF1=Regular_Time(DtF,interval)

DtF1 %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  tally %>%
  rename(Num_lag=n)












