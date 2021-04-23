#install.packages('lubridate')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('magrittr')
#install.packages('RcppRoll')
#install.packages('wrapr')
#install.packages('sqldf')
install.packages('repmis')
library(repmis)
Libs=c('lubridate','dplyr','tidyr','magrittr','RcppRoll','wrapr','data.table')
lapply(Libs,library, character.only = TRUE)

DtF=read.csv('C:/Users/Chi Zhang/Desktop/JAX.csv', sep=',',header=T,stringsAsFactors = F)[,-1] %>%
  mutate(Time=ymd_hms(Time))


# filename = 'Rain.csv'
# mykey = '4cs5he2g7uzh4mu'
# test_dp = source_DropboxData(filename, key = mykey, sep=",", header=TRUE)


DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  tally %>%
  rename(Num_lag=n)%>%
  view()

source('https://raw.githubusercontent.com/ZyuAFD/SWRE_General_R_Functions/master/src/Regulate%205%20min.R')
interval= 60

DtF=Regular_Time(DtF,interval)
#DtF3 = DtF3[rowSums(is.na(DtF3)) == 0,]

DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  tally %>%
  rename(Num_lag=n)%>%
  view()

# DtF2 %>%
#   ggplot()+
#   geom_point(aes(Time,Precip_mm))

Precip_Evt_Sep= function(dt,T_intv,IntE_P)
  #dt:       data of time and Precip_mm
  #T_intv:   Time interval of the time series (mins)
  #IntE_P:   Inter event period 
  #           (time step based your time interval
  #            For example: in a 5 min time interval series
  #            a 4 hour inter event period is corresponding to
  #            48 for IntE_P)
  # output: 
  #   Odd Evt_lab: Precip_mm event
  #   Even Evt_lab: Dry event
{
  #The header of time and Precip_mm should be
  # Time    Precip_mm
  
  dt %<>% arrange(Time)
  
  # print out the gaps with NA Precip_mm
  print('Here are all the gaps with NA Precip_mm.')
  dt %>% 
    filter(is.na(Precip_mm)) %>% 
    arrange(Time) %>% 
    mutate(lag=as.numeric(Time-lag(Time),units='mins')) %>% 
    mutate(Gap_St=ifelse(lag>T_intv | is.na(lag),'Start','NA')) %>% 
    mutate(Gap_End=ifelse(lead(lag)>T_intv | is.na(lead(lag)),'End','NA')) %>% 
    mutate(Gap_Lab=(Gap_St=='Start')+(Gap_End=='End')) %>% 
    mutate(Gap_n=(cumsum(Gap_Lab)+1) %/% 2) %>% 
    group_by(Gap_n) %>% 
    summarise(Start=min(Time),
              End=max(Time)) %>% 
    mutate(Duration_hr=as.numeric(End-Start,units='hours')) %>% 
    print
  
  #generate Precip_mm events
  
  data.frame(
    Time=c(min(dt$Time)-minutes(T_intv),
           max(dt$Time)+minutes(T_intv))
  ) %>% 
    bind_rows(dt) %>% 
    Regular_Time(T_intv) %>% 
    replace_na(list(Precip_mm=0)) %>% 
    mutate(Cum_Precip_4hr_L=roll_sum(Precip_mm,IntE_P+1,align='left',fill=0),
           Cum_Precip_4hr_R=roll_sum(Precip_mm,IntE_P+1,align='right',fill=0)) %>% 
    mutate(St_wet=ifelse(lag(Cum_Precip_4hr_R)==0 & Precip_mm>0,1,0),
           St_dry=ifelse(lag(Cum_Precip_4hr_L)>0 & Cum_Precip_4hr_L==0 & Precip_mm==0,1,0)) %>% 
    replace_na(list(St_wet=0,St_dry=0)) %>% 
    mutate(Evt_lab=St_wet+St_dry) %>% 
    mutate(Evt_lab=cumsum(Evt_lab)) %>% 
    select(Time,Precip_mm,Evt_lab) %>% 
    return
}

# inter event dry period time steps
IntE_P=4  # 4 horus: 4 time steps based on 60 min interval

DtF %>% 
  Precip_Evt_Sep(.,interval,IntE_P) %>% 
  filter(Evt_lab>0) %>% 
  group_by(Evt_lab) %>% 
  summarise(Start=min(Time),
            End=max(Time),
            TotalPrecip_mm=round(sum(Precip_mm),3),
            Max_Intensity=max(Precip_mm), # Maximium Precip_mm intensity based on time interval
            Dur_hr=as.numeric(max(Time+minutes(60))-min(Time),units='hours')) %>% 
  mutate(PreDry_Dur_hr=lag(Dur_hr)) %>% 
  #filter(TotalPrecip_mm>0) %>%
  filter(TotalPrecip_mm == 0) %>%
  write.table(.,'D:\\FAWN_data\\FL_WeatherData_Analysis\\drought.csv', row.names = FALSE, sep = ",")
  









