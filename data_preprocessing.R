#install.packages('lubridate')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('magrittr')
#install.packages('RcppRoll')
#install.packages('wrapr')
Libs=c('lubridate','dplyr','tidyr','magrittr','RcppRoll','wrapr')
lapply(Libs,library, character.only = TRUE)

DtF=read.csv('C:/Users/Chi Zhang/Desktop/Florida_hourly_NCDC.csv',
             sep=',',header=T,stringsAsFactors = F) %>%
  mutate(Time=ymd_hms(Time))

DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) 
