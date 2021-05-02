Libs=c('lubridate','dplyr','tidyr','magrittr','RcppRoll','wrapr','knitr','repmis','ggplot2')
lapply(Libs,library, character.only = TRUE)
setwd('./')

Plot_theme=theme_bw()+
  theme(axis.text=element_text(size=12, 
                               color="grey12"),
        axis.title=element_text(size=14),
        plot.title=element_text(size=18,
                                face="bold"),
        legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, 
                                 color="grey12"),
        strip.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))

# read table via dropbox link
# test_dp <- read.csv("https://www.dropbox.com/s/10qbo3spd4r7x1s/Florida_hourly_NCDC.csv?dl=1",
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

# data clean------------------------------------------------------------------------------------------------
DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  tally %>%
  rename(Num_lag=n) %>%
  kable




# find the time where the gap is 180 min 
DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  filter(TimeLag_min == 180) %>%
  mutate(Years=year(Time)) ->df
# tally %>%
# rename(Num_lag=n)

# find the time where the gap is larger than 60 min (exclude 180 min) 
DtF %>%
  arrange(Time) %>%
  mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  group_by(TimeLag_min) %>%
  filter(TimeLag_min > 60 & TimeLag_min != 180) %>%
  mutate(Years=year(Time)) -> df_gap

# remove these timestamps from raw data
DtF_clean <- anti_join(DtF,df,by = 'Time')

# regulate time
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


# fill gap and calculate lag pressure lag of 24-hour
DtF_rm_zero %>% 
  # spline the pressure
  mutate(Pressure_hPa.spl=spline(x=Time,y=Pressure_hPa,xout=Time)$y) %>%
  # moving average
  mutate(Pressure_hPa.av=roll_mean(Pressure_hPa.spl,n=24,align='center',fill=NA)) %>% 
  #Change of Pressure in 24 hours
  mutate(Pressure_chng.av=Pressure_hPa.av-lag(Pressure_hPa.av,24)) %>%  
  ungroup->Climate_Dt


# Try moving average--------------------------------------------------------------------------
# DtF_rm_zero %>% 
#   # spline the pressure
#   mutate(Pressure_hPa.spl=spline(x=Time,y=Pressure_hPa,xout=Time)$y) %>%
#   # moving average
#   mutate(Pressure_hPa.av=roll_mean(Pressure_hPa.spl,n=24,align='center',fill=NA)) %>% 
#   #Change of Pressure in 24 hours
#   mutate(Pressure_chng.av=Pressure_hPa.av-lag(Pressure_hPa.av,24)) %>% 
#   ungroup->Climate_Dt_ma
# 
# # plot and compare the effect of moving average
# dry_start = ymd_hms('1996-2-1 00:00:00')
# dry_end = ymd_hms('1996-2-8 00:00:00')
# wet_start = ymd_hms('1996-7-21 00:00:00')
# wet_end = ymd_hms('1996-7-28 00:00:00')
# 
# # No moving average
# Climate_Dt %>% 
#   filter(Time>=dry_start & Time<dry_end) -> Climate_Dt_dry
# 
# Climate_Dt %>% 
#   filter(Time>=wet_start & Time<wet_end) -> Climate_Dt_wet
# 
# # moving average
# Climate_Dt_ma %>% 
#   filter(Time>=dry_start & Time<dry_end) -> Climate_Dt_dry_ma
# 
# Climate_Dt_ma %>% 
#   filter(Time>=wet_start & Time<wet_end) -> Climate_Dt_wet_ma
# 
# 
# ggplot() + 
#   geom_line(data = Climate_Dt_dry, aes(x = Time, y = Pressure_chng.spl), color = "blue",linetype = 'Without Moving Average') + 
#   geom_line(data = Climate_Dt_dry_ma, aes(x = Time, y = Pressure_chng.av), color = "Red",linetype = 'With Moving Average') +
#   scale_linetype_manual(values = c('solid', 'dashed')) +
#   scale_colour_manual(values = c('blue', 'red'))
# #legend(legend=c("Without Moving Average", "With Moving Average"),col=c("blue", "red")) + 
#   labs(x='Dateime',y='Pressure (hPa)') +
#   Plot_theme
#   
#   
# ggplot() + 
#   geom_line(data = Climate_Dt_wet, aes(x = Time, y = Pressure_chng.spl), color = "blue") + 
#   geom_line(data = Climate_Dt_wet_ma, aes(x = Time, y = Pressure_chng.av), color = "Red") +
#   labs(x='Dateime',y='Pressure (hPa)') +
#   Plot_theme

# separate drought and rain events------------------------------------------------------------------  
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
  
  # dt %<>% arrange(Time)
  # 
  # # print out the gaps with NA Rain
  # print('Here are all the gaps with NA rain.')
  # dt %>% 
  #   filter(is.na(Rain)) %>% 
  #   arrange(Time) %>% 
  #   mutate(lag=as.numeric(Time-lag(Time),units='mins')) %>% 
  #   mutate(Gap_St=ifelse(lag>T_intv | is.na(lag),'Start','NA')) %>% 
  #   mutate(Gap_End=ifelse(lead(lag)>T_intv | is.na(lead(lag)),'End','NA')) %>% 
  #   mutate(Gap_Lab=(Gap_St=='Start')+(Gap_End=='End')) %>% 
  #   mutate(Gap_n=(cumsum(Gap_Lab)+1) %/% 2) %>% 
  #   group_by(Gap_n) %>% 
  #   summarise(Start=min(Time),
  #             End=max(Time)) %>% 
  #   mutate(Duration_hr=as.numeric(End-Start,units='hours')) %>% 
  #   print
  
  #generate rain events
  
  # data.frame(
  #   Time=c(min(dt$Time)-minutes(T_intv),
  #          max(dt$Time)+minutes(T_intv))
  # ) %>% 
  #   bind_rows(dt) %>% 
  #   Regular_Time(T_intv) %>%
  dt %>% 
    replace_na(list(Rain=0)) %>% 
    mutate(Cum_Precip_4hr_L=roll_sum(Rain,IntE_P+1,align='left',fill=0),
           Cum_Precip_4hr_R=roll_sum(Rain,IntE_P+1,align='right',fill=0)) %>% 
    mutate(StR=ifelse(lag(Cum_Precip_4hr_R)==0 & Rain>0,1,0),
           StD=ifelse(lag(Cum_Precip_4hr_L)>0 & Cum_Precip_4hr_L==0 & Rain==0,1,0)) %>% 
    replace_na(list(StR=0,StD=0)) %>% 
    mutate(Evt_lab=StR+StD) %>% 
    mutate(Evt_lab=cumsum(Evt_lab)) %>% 
    select(-Cum_Precip_4hr_L,-Cum_Precip_4hr_R) %>% 
    return
}

IntE_P = 4
# rain events
Climate_Dt %>% 
  Precip_Evt_Sep(.,interval,IntE_P) -> DtF_sep

DtF_sep %>% 
  select(Time,Rain,Evt_lab) %>% 
  filter(Evt_lab>0) %>% 
  group_by(Evt_lab) %>% 
  summarise(Start=min(Time),
            End=max(Time),
            TotalRain=round(sum(Rain),3),
            Max_Intensity=max(Rain), # Maximium rain intensity based on time interval
            Dur_hr=as.numeric(max(Time+minutes(60))-min(Time),units='hours')) %>% 
  mutate(PreDry_Dur_hr=lag(Dur_hr)) %>% 
  filter(TotalRain>0) -> DtF_sep_rain

  write.table(DtF_sep_rain, './Rain_Evt.csv',row.names = FALSE,sep = ',')
  
# drought events 

DtF_sep %>%
  select(Time,Rain,Evt_lab) %>% 
  filter(Evt_lab>0) %>% 
  group_by(Evt_lab) %>% 
  summarise(Start=min(Time),
            End=max(Time),
            TotalRain=round(sum(Rain),3),
            Max_Intensity=max(Rain), # Maximium rain intensity based on time interval
            Dur_hr=as.numeric(max(Time+minutes(60))-min(Time),units='hours')) %>% 
  mutate(PreRain_Dur_hr=lag(Dur_hr)) %>% 
  filter(TotalRain==0) -> DtF_sep_dry

  write.table('./Drought_Evt.csv',row.names = FALSE,sep = ',')
  

save.image("sep_drought_rain_evt.RData")

# pressure events--------------------------------------------------------------------------------------
# convert Climate data into pressure events series
Get_Press_Evt_lab=function(Dt) 
{
  Dt %>% 
    arrange(Time)%>%
    mutate(Mon=month(Time)) %>% 
    # Roll average over 24 hours
    mutate(Press_Evt_lab=ifelse(Pressure_chng.av*lag(Pressure_chng.av)<=0 & lag(Pressure_chng.av)!=0,1,0)) %>% 
    mutate(Press_Evt_lab=ifelse(is.na(Press_Evt_lab),0,Press_Evt_lab)) %>% 
    mutate(Press_Evt_lab=cumsum(Press_Evt_lab)) %>% 
    return
}

Get_Press_Evt=function(Dt)
{
  Dt %>% 
    mutate(Mon=month(Time),
           Yr=year(Time)) %>% 
    group_by(Yr,Mon) %>% 
    summarise(MonT=mean(Temp_C,na.rm=T))->Dt_MonT
  
  Dt %>% 
    filter(Press_Evt_lab>0,
           Press_Evt_lab<max(Press_Evt_lab)) %>% 
    group_by(Press_Evt_lab,Location) %>% 
    summarise(St=min(Time),
              End=max(Time),
              Dur=as.numeric(max(Time)-min(Time),units='hours')+1,
              Sum_Press_Delta=sum(Pressure_chng.av),
              Press_NA=sum(is.na(Pressure_hPa)),
              Temp_NA=sum(is.na(Temp_C)),
              Sum_Precip=sum(Rain,na.rm=T),
              St_Rain=sum(StR),
              St_Dry=sum(StD)) %>% 
    filter(Dur<1000) %>% #take events with more than 10000 hours as gap
    mutate(Yr=year(St),
           Mon=month(St)) %>% 
    left_join(.,Dt_MonT,by=c('Yr'='Yr','Mon'='Mon')) %>% 
    ungroup %>% 
    arrange(St) %>% 
    mutate(Dur_lag1=lag(Dur),
           Press_Delta_lag1=lag(Sum_Press_Delta)) %>% 
    
    return
}

DtF_sep %>% 
  Get_Press_Evt_lab(.) %>% 
  Get_Press_Evt(.) -> Raw_dt
