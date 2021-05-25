#install.packages('lubridate')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('magrittr')
#install.packages('RcppRoll')
#install.packages('wrapr')
#install.packages('sqldf')
#install.packages('repmis')
# install.packages('remotes')
# remotes::install_github("Sibada/scPDSI")
# install.packages("SPEI")
Libs=c('lubridate','dplyr','purrr','tidyr','magrittr','RcppRoll','wrapr','knitr','repmis','data.table','magrittr','ggplot2','scales','scPDSI','SPEI')
lapply(Libs,library, character.only = TRUE)
setwd('./')

# Set theme for plotting---------------------------------------------------------------------------------
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

theme_Result= theme(panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(colour = "light grey"),
                    panel.border = element_rect(colour="black",fill=NA),
                    line = element_line(colour = "black", size = 0.5, linetype = 1),
                    axis.text.x = element_text(size=10,colour='black'),
                    axis.text.y = element_text(size=16,colour='black'),
                    axis.title.y = element_text(size = 18),
                    axis.title.x = element_text(size = 18),
                    plot.title = element_text(size = 20),
                    legend.text=element_text(size=18),
                    legend.title=element_text(size=18,face='plain'),
                    legend.position='bottom')


# All the complementary function-----------------------------------------------------------------------------------------------------

# Function for regulating time interval
source('https://raw.githubusercontent.com/ZyuAFD/SWRE_General_R_Functions/master/src/Regulate%205%20min.R')

# Function for padding location
pad_loc=function(x,df)
{
  loc = unique(df$Location)
  x %>% 
    #pad(interval="hour") %>% 
    replace_na(list(Location=loc)) %>% 
    return
}

# Function for separating dry and wet events
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

# Function for getting pressure change events
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

# data clean------------------------------------------------------------------------------------------------
data_preprosessing = function(DtF)
{
  DtF %>%
    arrange(Time) %>%
    mutate(St=lag(Time)) %>% 
    mutate(TimeLag_min=as.numeric(Time-St,units='mins')) %>% 
    mutate(Years=year(Time))%>% 
    rename(End=Time) %>% 
    relocate(St,.before = End)->DtF_TimeLag 
  
  # DtF_TimeLag %>%
  # group_by(TimeLag_min) %>% 
  # tally %>%
  # rename(Num_lag=n) %>%
  # kable
  
  
  
  
  # find the time where the gap is larger than 4 hours
  DtF_TimeLag %>%
    group_by(TimeLag_min) %>%
    filter(TimeLag_min >= 240) -> df_gap
  
  # find the time where the gap number is larger than 100, get all gaps
  DtF_TimeLag %>%
    group_by(TimeLag_min) %>%
    filter(TimeLag_min > 60 & TimeLag_min < 240) %>%
    count(TimeLag_min)%>% 
    filter(n>100) %>%
    select(-n) %>% 
    left_join(DtF_TimeLag) %>% 
    relocate(TimeLag_min,.before = Years) %>% 
    bind_rows(df_gap) %>% 
    arrange(St)-> DtF_gap
  
  
  # regulate time
  interval = 60
  
  
  DtF %>% 
    Regular_Time(.,interval) %>% 
    do(pad_loc(.,DtF)) -> DtF_clean
  # 
  # DtF_clean %>% 
  #   filter(is.na(Location)== T)
  
  # remove 0 time interval
  DtF_clean %>% 
    group_by(Time,Location) %>%
    summarise(Rain=mean(ifelse(Rain>0 & Rain<999.9,Rain,NA),na.rm=T),Pressure_hPa=mean(Pressure_hPa,na.rm=TRUE),Temp_C=mean(Temp_C,na.rm=TRUE),
              DewPt_C=mean(DewPt_C,na.rm=TRUE),RH=mean(RH,na.rm=TRUE),Wind_SP_m_s=mean(Wind_SP_m_s,na.rm=TRUE)) %>% 
    ungroup() -> DtF_rm_zero
  
  # DtF_rm_zero %>%
  #   arrange(Time) %>%
  #   mutate(TimeLag_min=as.numeric(Time-lag(Time),units='mins')) %>%
  #   group_by(TimeLag_min) %>%
  #   tally %>%
  #   rename(Num_lag=n) %>%
  #   kable
  
  DtF_rm_zero %>% 
    filter(is.na(Location)== T)
  
  # fill gap, do moving average calculate lag pressure lag of 24-hour
  DtF_rm_zero %>% 
    # spline the pressure
    mutate(Pressure_hPa.spl=spline(x=Time,y=Pressure_hPa,xout=Time)$y) %>%
    # moving average
    mutate(Pressure_hPa.av=roll_mean(Pressure_hPa.spl,n=24,align='center',fill=NA)) %>% 
    #Change of Pressure in 24 hours
    mutate(Pressure_chng.av=Pressure_hPa.av-lag(Pressure_hPa.av,24)) %>%  
    ungroup->Climate_Dt
  
  IntE_P = 4
  # rain events
  Climate_Dt %>% 
    Precip_Evt_Sep(.,interval,IntE_P) -> DtF_sep
  
  DtF_sep %>% 
    select(Time,Rain,Evt_lab) %>% 
    filter(Evt_lab>0, Evt_lab<max(Evt_lab)) %>% 
    group_by(Evt_lab) %>% 
    summarise(Start=min(Time),
              End=max(Time),
              TotalRain=round(sum(Rain),3),
              Max_Intensity=max(Rain), # Maximium rain intensity based on time interval
              Dur_hr=as.numeric(max(Time+minutes(60))-min(Time),units='hours')) %>% 
    mutate(PreDry_Dur_hr=lag(Dur_hr)) %>% 
    filter(TotalRain>0) -> DtF_sep_rain
  
  # write.table(DtF_sep_rain, './Rain_Evt.csv',row.names = FALSE,sep = ',')
  
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
  
  # write.table('./Drought_Evt.csv',row.names = FALSE,sep = ',')
  #   
  # 
  # save.image("sep_drought_rain_evt.RData")
  
  # pressure events--------------------------------------------------------------------------------------
  # convert Climate data into pressure events series
  
  DtF_sep %>% 
    Get_Press_Evt_lab(.) -> Raw_dt
  
  Raw_dt %>% 
    Get_Press_Evt(.) -> Raw_dt_evt
  
  # Remove events overlapped with gaps
  
  
  Raw_dt_evt_tb = data.table(Raw_dt_evt)
  DtF_gap_tb = data.table(DtF_gap)
  setkey(Raw_dt_evt_tb,St,End)
  overlap = foverlaps(DtF_gap_tb,Raw_dt_evt_tb,type="any",nomatch=0L)
  
  Raw_dt %>% 
    anti_join(.,overlap,by='Press_Evt_lab') %>% 
    filter(Press_Evt_lab != max(Press_Evt_lab))->Raw_dt_no_gap
  
  # Raw_dt_no_gap %>% 
  #   Get_Press_Evt(.) -> Raw_dt_evt_no_gap
  
  return(Raw_dt_no_gap)
}



# Read data from csv file----------------------------------------------------------------------------------------

# read table via dropbox link
# test_dp <- read.csv("https://www.dropbox.com/s/10qbo3spd4r7x1s/Florida_hourly_NCDC.csv?dl=1",
#                     sep=',',header=T,stringsAsFactors = F)[c('Time','Location','Precip_mm')] %>%
#   mutate(Time=ymd_hms(Time)) %>%
#   rename(Rain = Precip_mm) %>%
#   filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US')



# filename = 'Rain.csv'
# mykey = '4cs5he2g7uzh4mu'
# test_dp = source_DropboxData(filename, key = mykey, sep=",", header=TRUE)



DtF_NCDC=read.csv('D:/FAWN_data/Florida_hourly_NCDC.csv',
                  sep=',',header=T,stringsAsFactors = F) %>%
  mutate(Time=ymd_hms(Time)) %>% 
  rename(Rain = Precip_mm)

# DtF_FAWN=read.csv('D:/FAWN_data/Florida_hourly_FAWN.csv',
#                   sep=',',header=T,stringsAsFactors = F) %>%
#   mutate(Time=ymd_hms(Time)) %>% 
#   rename(Rain = Pecipit_mm)
# all
# DtF_all_loc = bind_rows(DtF_NCDC,DtF_FAWN)



DtF_NCDC %>% 
  mutate(Loc=Location) %>% 
  group_by(Loc) %>% 
  nest()->DtF_nest_EDI

DtF_nest_EDI %>% 
  mutate(data_processed = map(data, ~data_preprosessing(.))) %>% 
  mutate(EP = map(data_processed, ~EP_cal(.))) -> DtF_EDI_all



DtF_NCDC %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  mutate(Mon=month(Time), Day = day(Time), Yr=year(Time)) %>%
  group_by(Yr, Mon, Day) %>%
  summarise(Time, Yr, Mon, Day, sum(Rain,na.rm = T)) %>% 
  arrange(Time) %>% 
  rename(Day_Rain = 'sum(Rain, na.rm = T)') -> Dt_Rain

  DS = 7 #7
  ep = 0
  ep_new = 0
  len <- nrow(Dt_Rain) 

  for (i in rep(8:len)){
    for (j in rep(1:DS)) {
      ep = sum(Dt_Rain$Day_Rain[i-j]:Dt_Rain$Day_Rain[i])/j
      ep_new = ep_new + ep
    }
    Dt_Rain[i,6] = ep_new
    ep_new = 0
  }
  Dt_Rain %>% 
    rename(EP = '...6') -> Dt_EP

# EP function
EP_cal = function(dt){
  DS = 7
  ep = 0
  ep_new = 0
  len <- nrow(Dt_Rain) 
  dt %>% 
    mutate(Mon=month(Time), Day = day(Time), Yr=year(Time)) %>%
    group_by(Yr, Mon, Day) %>%
    summarise(Time, Yr, Mon, Day, sum(Rain,na.rm = T)) %>% 
    arrange(Time) %>% 
    rename(Day_Rain = 'sum(Rain, na.rm = T)') -> Dt_Rain_fu
    
    for (i in rep(8:len)){
      for (j in rep(1:DS)) {
          ep = sum(Dt_mat[i-j,4]:Dt_mat[i,4])/j
          ep_new = ep_new + ep
      }
      Dt_Rain_fu[i,6] = ep_new
      ep_new = 0
    }
    return
}





# -> Data_map_EDI
  # mutate(Raw_dt_evt_all = map(Raw_dt_all, ~Get_Press_Evt(.)))->DtF_map_fawn


DtF_map_fawn %>% 
  select(Loc,Raw_dt_all) %>% 
  unnest(cols = c(Raw_dt_all)) %>%
  ungroup() %>% 
  select(-Loc)->Raw_dt_all_loc_fawn

DtF_map_fawn %>% 
  select(Loc,Raw_dt_evt_all) %>% 
  unnest(cols = c(Raw_dt_evt_all)) %>% 
  ungroup() %>%
  select(-Loc)->Raw_dt_evt_all_loc_fawn

















#get Monthly meantemperature 
DtF_clean_loc %>% 
  mutate(Mon=month(Time),
         Yr=year(Time)) %>% 
  group_by(Yr,Mon) %>% 
  summarise(Yr,Mon, Location, sum(Rain), MonT=mean(Temp_C,na.rm=T) ) -> Dtf_pdsi