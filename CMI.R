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
              DewPt_C=mean(DewPt_C,na.rm=TRUE),RH=mean(RH,na.rm=TRUE),Wind_SP_m_s=mean(Wind_SP_m_s,na.rm=TRUE), CMI = mean(CMI.x, na.rm = TRUE),CMI_pre = mean(CMI.y, na.rm = TRUE)) %>% 
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


# file_path_local="~/Dropbox (UFL)/Weather Data Analysis/2021/FL extreme weather events analysis/FL_CMI_73_18.csv"
# file_path_dropbox="https://www.dropbox.com/s/dx85e6p9eh1qzj8/FL_CMI_73_18.csv?dl=1"
# 
# FL_CMI = read.csv(file_path_local, header = T,stringsAsFactors = F)
# 
# file_path_local="~/Dropbox (UFL)/Weather Data Analysis/2021/FL extreme weather events analysis/Florida_hourly_NCDC.csv"
# file_path_dropbox="https://www.dropbox.com/s/h81j6kh50bq69ap/Florida_hourly_NCDC.csv?dl=1"


FL_CMI = read.csv('D:/FAWN_data/FL_CMI_73_18.csv', header = T,stringsAsFactors = F)

DtF_NCDC=read.csv('D:/FAWN_data/Florida_hourly_NCDC.csv',
                  sep=',',header=T,stringsAsFactors = F) %>%
  mutate(Time=ymd_hms(Time)) %>% 
  rename(Rain = Precip_mm)


# DtF_NCDC=read.csv(file_path_local,
#                   sep=',',header=T,stringsAsFactors = F) %>%
#   mutate(Time=ymd_hms(Time)) %>% 
#   rename(Rain = Precip_mm)

# DtF_FAWN=read.csv('D:/FAWN_data/Florida_hourly_FAWN.csv',
#                   sep=',',header=T,stringsAsFactors = F) %>%
#   mutate(Time=ymd_hms(Time)) %>% 
#   rename(Rain = Pecipit_mm)

FL_CMI %>% 
  mutate(Yr = year(ymd_hms),
         Mon = month(ymd_hms),
         Day = day(ymd_hms),
         h = hour(ymd_hms)) -> Dt_CMI

DtF_NCDC %>% 
  mutate(Yr = year(Time),
         Mon = month(Time),
         Wk = week(Time),
         Day = day(Time),
         h = hour(Time)
         ) %>% 
  left_join(., Dt_CMI, by = c('Yr' = 'Yr', 'Mon' = 'Mon', 'Day' = 'Day', 'h' = 'h', 'Location' = 'location')) %>% 
  left_join(., Dt_CMI %>% mutate(Day_pre=Day-7), by = c('Yr' = 'Yr', 'Mon' = 'Mon', 'Day' = 'Day_pre', 'h' = 'h', 'Location' = 'location')) -> Dt_NCDC_CMI 

#NCDC
Dt_NCDC_CMI %>% 
  mutate(Loc=Location) %>% 
  group_by(Loc) %>% 
  nest()->Dt_NCDC_CMIc

Dt_NCDC_CMIc %>% 
  mutate(Raw_dt_all = map(data, ~data_preprosessing(.))) %>% 
  mutate(Raw_dt_evt_all = map(Raw_dt_all, ~Get_Press_Evt(.)))->DtF_map_ncdc

DtF_map_ncdc %>% 
  select(Loc,Raw_dt_all) %>% 
  unnest(cols = c(Raw_dt_all)) %>%
  ungroup() %>% 
  select(-Loc)->Raw_dt_all_loc_ncdc

# DtF_map_ncdc %>% 
#   select(Loc,Raw_dt_evt_all) %>% 
#   unnest(cols = c(Raw_dt_evt_all)) %>% 
#   ungroup() %>%
#   select(-Loc)->Raw_dt_evt_all_loc_ncdc



# cumulative 
Raw_dt_all_loc_ncdc %>% 
  filter(data.table::between(Time,ymd('1973-01-01'),ymd('2018-12-24'))) %>%
  replace_na(list(CMI = 0,CMI_pre=0)) %>% 
  mutate(Yr = year(Time),
         Wk = week(Time)) %>% 
  select(Time, Yr, Mon, Wk, Location, CMI,CMI_pre, Temp_C, RH, DewPt_C, Press_Evt_lab, Pressure_chng.av,Rain) %>% 
  group_by(Yr, Mon, Wk, Location) %>%
  summarise(
            CMI = sum(CMI),
            CMI_pre=sum(CMI_pre),
            PCE_num = max(Press_Evt_lab) - min(Press_Evt_lab),
            wk_Rain=sum(Rain),
            cum_Temp = sum(Temp_C)/PCE_num,
            cum_RH = sum(RH)/PCE_num,
            Temp_dew_diff = sum(Temp_C - DewPt_C)/PCE_num,
            cum_Pressure_chng = sum(Pressure_chng.av)) %>% 
            replace_na(list(cum_Temp = 0, cum_RH = 0, Temp_dew_diff = 0,wk_Rain=0)) -> T_RH_Diff_cum
            # mutate(CMI_stand = (CMI - mean(CMI,na.rm = TRUE))/sd(CMI, na.rm = TRUE))

T_RH_Diff_cum$CMI_standarize = scale(T_RH_Diff_cum$CMI)
# T_RH_Diff_cum$CMI_pre_standarize = scale(T_RH_Diff_cum$CMI_pre)

T_RH_Diff_cum %>% 
  filter(CMI_standarize < -1) -> T_RH_Diff_cum_exclude



#hide -------
#mean
Raw_dt_all_loc_ncdc %>% 
  filter(data.table::between(Time,ymd('1973-01-01'),ymd('2018-12-24'))) %>%
  replace_na(list(CMI = 0)) %>% 
  mutate(Yr = year(Time),
         Wk = week(Time)) %>% 
  select(Time, Yr, Mon, Wk, Location, CMI, Temp_C, RH, DewPt_C, Press_Evt_lab) %>% 
  group_by(Yr, Mon, Wk, Location) %>%
  summarise(Yr,
            Mon,
            Wk,
            Location,
            CMI = sum(CMI),
            Time,
            PCE_num = max(Press_Evt_lab) - min(Press_Evt_lab),
            mean_Temp = mean(Temp_C)/PCE_num,
            mean_RH = mean(RH)/PCE_num,
            Temp_dew_diff = mean(Temp_C - DewPt_C)/PCE_num) %>% 
  replace_na(list(mean_Temp = 0, mean_RH = 0, Temp_dew_diff = 0))-> T_RH_Diff_mean









#plot T_mean
T_RH_Diff_mean %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  # mutate(wet_dry = ifelse(between(Mon,4,10), 'wet', 'dry')) %>% 
  # filter(CMI >= 0.5 | CMI <= -0.5) %>%
  select(-Time) %>% 
  unique() %>% 
  ggplot(aes(x=mean_Temp))+
  geom_jitter(aes(y=CMI), size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  # geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("CMI")+
  xlab("Mean of week Temps / # PCEs")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))

#plot T_cumulative
T_RH_Diff_cum %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  mutate(wet_dry = ifelse(between(Mon,4,10), 'wet', 'dry')) %>%
  # filter(CMI >= 0.5 | CMI <= -0.5) %>%
  select(-Time) %>% 
  unique() %>% 
  ggplot(aes(x= cum_Temp, colour = wet_dry))+
  geom_jitter(aes(y=CMI), size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  # geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("CMI")+
  xlab("Cumulative of week Temps / # PCEs")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))

# plot RH_mean
T_RH_Diff_mean %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  mutate(wet_dry = ifelse(between(Mon,4,10), 'wet', 'dry')) %>%
  # filter(CMI >= 0.5 | CMI <= -0.5) %>%
  select(-Time) %>% 
  unique() %>% 
  ggplot(aes(x=mean_RH, colour = wet_dry))+
  geom_jitter(aes(y=CMI), size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  # geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("CMI")+
  xlab("Mean of week RHs / # PCEs")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))


# plot RH_cum
T_RH_Diff_cum %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  mutate(wet_dry = ifelse(between(Mon,4,10), 'wet', 'dry')) %>%
  filter(Mon >= 5 | Mon <= 7, PCE_num <= 5) %>% 
  # filter(CMI >= 0.5 | CMI <= -0.5) %>%
  select(-Time) %>% 
  unique() %>% 
  ggplot(aes(x=cum_RH, colour = wet_dry))+
  geom_jitter(aes(y=CMI), size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  # geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("CMI")+
  xlab("Cum of week RHs / # PCEs")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))

# plot diff between temp and dew point_mean

T_RH_Diff_mean %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  # mutate(wet_dry = ifelse(between(Mon,4,10), 'wet', 'dry')) %>% 
  # filter(CMI >= 0.5 | CMI <= -0.5) %>%
  select(-Time) %>% 
  unique() %>% 
  ggplot(aes(x=Temp_dew_diff))+
  geom_jitter(aes(y=CMI), size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  # geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("CMI")+
  xlab("Mean of week (Temp - dew point) / # PCEs")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))

# plot diff between temp and dew point_cum

T_RH_Diff_cum %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  mutate(wet_dry = ifelse(PCE_num > 2, '>2', '<2')) %>%
  filter(CMI >= 1 | CMI <= -1) %>%
  filter(Mon >= 2 | Mon <= 4, PCE_num <= 5) %>% 
  select(-Time) %>% 
  unique() %>% 
  ggplot(aes(x=Temp_dew_diff, colour = wet_dry))+
  geom_jitter(aes(y=CMI), size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("CMI")+
  xlab("Mean of week (Temp - dew point) / # PCEs")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))



# plot diff VS cum_pressure_chng with CMI in color--------

T_RH_Diff_cum_exclude %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  mutate(wet_dry = ifelse(between(Mon,6,10), 'wet', 'dry')) %>%
  # filter(CMI >= 1 | CMI <= -1) %>%
  #filter(Mon >= 11 | Mon <= 4) %>%
  # filter(wk_Rain<10) %>% 
  ggplot(aes(x=Temp_dew_diff, colour = wet_dry, y=CMI_standarize))+
  geom_jitter(size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  geom_smooth(method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("cum_Pressure_chng")+
  xlab("Mean of week (Temp - dew point) / # PCEs")+
  xlim(200,800)+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))


T_RH_Diff_cum_exclude %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>%
  filter(log(Temp_dew_diff)>5,
         CMI_pre<=0) %>% 
  ggplot(aes(x=CMI_standarize,y=cum_Pressure_chng,color=cum_Pressure_chng>0))+
  geom_jitter(aes(size=CMI_pre))+
  geom_smooth(method = "loess", size = 1.5, linetype = 5, se = FALSE)
  

  
T_RH_Diff_cum_exclude %>% 
    filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>%
    filter(Temp_dew_diff>20) %>% 
  ggplot(aes(x=factor(Mon),y=Temp_dew_diff))+
    geom_boxplot()
  
T_RH_Diff_cum_exclude %>% 
    filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>%
    #filter(Temp_dew_diff>20) %>% 
    ggplot(aes(x=factor(Mon),y=CMI_standarize))+
    geom_boxplot()+
    labs(title="CMI")









#CMI_1
Raw_dt_all_loc_ncdc %>% 
  filter(data.table::between(Time,ymd('1973-01-01'),ymd('2018-12-24'))) %>%
  replace_na(list(CMI = 0)) %>% 
  mutate(Yr = year(Time),
         Wk = week(Time)) %>% 
  # select(Yr, Mon, Wk, Location, CMI) %>% 
  group_by(Yr, Mon, Wk, Location) %>%
  summarise(PCE_num = max(Press_Evt_lab) - min(Press_Evt_lab),
            Yr,
            Mon,
            Wk,
            Location,
            CMI = sum(CMI),Time,
            Rain = sum(Rain)) %>% 
  mutate(year_wk = paste(Yr, Wk)) -> Raw_all_loc_CMI


#CMI VS #pce'
Raw_all_loc_CMI %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  mutate(wet_dry = ifelse(between(Mon,4,10), 'wet', 'dry')) %>% 
  filter(CMI >= 0.5 | CMI <= -0.5) %>%
  select(-Time) %>% 
  unique() %>% 
  ggplot(aes(x=PCE_num, colour = wet_dry))+
  geom_jitter(aes(y=CMI), size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("CMI")+
  xlab("Number of PCEs within One Week")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))


#Rain vs # PCE 
# Raw_all_loc_CMI %>% 
#   filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
#   # filter(CMI >= 3 | CMI <= -1) %>% 
#   select(-Time) %>% 
#   unique() %>% 
#   ggplot(aes(x=PCE_num))+
#   geom_point(aes(y=Rain))+
#   #scale_colour_gradientn(colours=rainbow(4))
#   # scale_colour_gradientn(colours=rainbow(4))         
#   # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
#   # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
#   # guides(fill = guide_colorbar(barwidth = 10))+
#   # geom_smooth(aes(y=CMI,group = 1, color = "Local Fit Line"), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
#   ylab("Rain")+
#   xlab("Number of PCEs within One Week")+
#   #scale_x_datetime(breaks = date_breaks(breaks)) +
#   #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
#   #scale_colour_manual("",values=c("black"))+
#   #guides(colour = guide_legend(override.aes = list(fill=NA)))+
#   #ylab("Value")+
#   theme_Result+
#   theme(legend.text =element_text(size=12),legend.position = "bottom")+
#   ggtitle('JACKSONVILLE 1973 - 2018')+
#   theme(plot.title = element_text(hjust = 0.5))






  
  
  
  
breaks='1 days'
Raw_dt_all_loc_ncdc %>% 
  filter(data.table::between(Time,ymd('1979-01-01'),ymd('2018-12-24')))%>% 
  mutate(Pressure_chng.av=as.numeric(Pressure_chng.av),
         Rain=as.numeric(Rain),
         Temp_C=as.numeric(Temp_C),
         RH=as.numeric(RH),
         DewPt_C=as.numeric(DewPt_C)) %>% 
  ggplot(aes(x=Time))+
  geom_area(aes(y=Pressure_chng.av,fill="Air pressure change (hPa)"),alpha=0.3)+
  geom_bar(aes(y=Rain,colour="Precipitation (mm)",width=2),stat="identity")+
  geom_line(aes(y=Temp_C,color='Temperature (\u00B0C)'),size=1)+
  geom_line(aes(y=DewPt_C,color='Dew point'),size=0.5)+ 
  geom_line(aes(y=RH,color='RH'),size=2)+ 
  scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  scale_colour_manual("",values=c("blue","black","#729e6f","red"))+
  scale_fill_manual(values=c("grey10"))+
  guides(colour = guide_legend(override.aes = list(fill=NA)))+
  ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=16),legend.position = "bottom")








