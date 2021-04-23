Libs=c('lubridate','dplyr','tidyr','magrittr','RcppRoll','wrapr','knitr','repmis')
lapply(Libs,library, character.only = TRUE)
setwd('./')

# read table via dropbox link
# test_dp <- read.csv("https://www.dropbox.com/s/0o8hrb4o4ccwaxz/Florida_hourly_NCDC.csv?dl=1",
#                     sep=',',header=T,stringsAsFactors = F)[c('Time','Location','Precip_mm')] %>% 
#   mutate(Time=ymd_hms(Time)) %>% 
#   rename(Rain = Precip_mm) %>%
#   filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US')


DtF=read.csv('../../Data/NCDC/Florida/Florida_hourly_NCDC.csv',
             sep=',',header=T,stringsAsFactors = F) %>%
  mutate(Time=ymd_hms(Time)) %>% 
  rename(Rain = Precip_mm) %>%
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US')

source('https://raw.githubusercontent.com/ZyuAFD/SWRE_General_R_Functions/master/src/Regulate%205%20min.R')

DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  tally %>%
  rename(Num_lag=n) %>%
  kable

# find the time where the gap is 180 min 
df<-DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  filter(TimeLag_min == 180) %>%
  mutate(Years=year(Time))
# tally %>%
# rename(Num_lag=n)

# find the time where the gap is larger than 60 min (exclude 180 min) 
df_gap<-DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  filter(TimeLag_min > 60 & TimeLag_min != 180) %>%
  mutate(Years=year(Time))

# remove these timestamps from raw data
DtF_clean <- anti_join(DtF,df,by = 'Time')


interval = 60
DtF_clean=Regular_Time(DtF_clean,interval)

# remove 0 time interval
DtF_clean %>% 
  group_by(Time,Location) %>%
  summarise(Rain=max(Rain),Pressure_hPa=mean(Pressure_hPa,na.rm=TRUE),Temp_C=mean(Temp_C,na.rm=TRUE),
            DewPt_C=mean(DewPt_C,na.rm=TRUE),RH=mean(RH,na.rm=TRUE),Wind_SP_m_s=mean(Wind_SP_m_s,na.rm=TRUE)) %>% 
  ungroup() -> DtF_rm_zero

DtF_rm_zero %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  tally %>%
  rename(Num_lag=n) %>%
  kable

Precip_Evt_Sep= function(dt,T_intv,IntE_P)
  #dt:       data of time and rain
  #T_intv:   Time interval of the time series (mins)
  #IntE_P:   Inter event period 
  #           (time step based your time interval
  #            For example: in a 5 min time interval series
  #            a 4 hour inter event period is corresponding to
  #            48 for IntE_P)
  # output: 
  #   Odd Evt_lab: Rain event
  #   Even Evt_lab: Dry event
{
  #The header of time and rain should be
  # Time    Rain
  
  dt %<>% arrange(Time)
  
  # print out the gaps with NA Rain
  print('Here are all the gaps with NA rain.')
  dt %>% 
    filter(is.na(Rain)) %>% 
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
  
  #generate rain events
  
  data.frame(
    Time=c(min(dt$Time)-minutes(T_intv),
           max(dt$Time)+minutes(T_intv))
  ) %>% 
    bind_rows(dt) %>% 
    Regular_Time(T_intv) %>% 
    replace_na(list(Rain=0)) %>% 
    mutate(Cum_Precip_4hr_L=roll_sum(Rain,IntE_P+1,align='left',fill=0),
           Cum_Precip_4hr_R=roll_sum(Rain,IntE_P+1,align='right',fill=0)) %>% 
    mutate(St_wet=ifelse(lag(Cum_Precip_4hr_R)==0 & Rain>0,1,0),
           St_dry=ifelse(lag(Cum_Precip_4hr_L)>0 & Cum_Precip_4hr_L==0 & Rain==0,1,0)) %>% 
    replace_na(list(St_wet=0,St_dry=0)) %>% 
    mutate(Evt_lab=St_wet+St_dry) %>% 
    mutate(Evt_lab=cumsum(Evt_lab)) %>% 
    select(Time,Rain,Evt_lab) %>% 
    return
}

IntE_P = 4

DtF %>% 
  Precip_Evt_Sep(.,interval,IntE_P) %>% 
  filter(Evt_lab>0) %>% 
  group_by(Evt_lab) %>% 
  summarise(Start=min(Time),
            End=max(Time),
            TotalRain=round(sum(Rain),3),
            Max_Intensity=max(Rain), # Maximium rain intensity based on time interval
            Dur_hr=as.numeric(max(Time+minutes(60))-min(Time),units='hours')) %>% 
  #mutate(PreDry_Dur_hr=lag(Dur_hr)) %>% 
  mutate(PreRain_Dur_hr=lag(Dur_hr)) %>% 
  filter(TotalRain==0) %>%  write.table('./Drought_Evt.csv',row.names = FALSE,sep = ',')
#filter(TotalRain>0) %>%  write.table('./Rain_Evt.csv',row.names = FALSE,sep = ',')