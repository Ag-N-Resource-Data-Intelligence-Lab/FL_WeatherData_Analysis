# install.packages('lubridate')
# install.packages('dplyr')
# install.packages('tidyr')
# install.packages('magrittr')
# install.packages('RcppRoll')
# install.packages('wrapr')
# install.packages('sqldf')
# install.packages('repmis')
# install.packages('remotes')
# remotes::install_github("Sibada/scPDSI")
# install.packages("SPEI")
Libs=c('lubridate','dplyr','purrr','tidyr','magrittr','RcppRoll','wrapr','knitr','repmis','data.table','magrittr','ggplot2','scales','scPDSI','SPEI')
lapply(Libs,library, character.only = TRUE)
setwd('./')
# install.packages("devtools")
# library(devtools)
# devtools::install_github("WillemMaetens/standaRdized")
library(standaRdized)

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


# file_path_local="~/Dropbox (UFL)/Weather Data Analysis/2021/FL extreme weather events analysis/FL_CMI_73_18.csv"
# file_path_dropbox="https://www.dropbox.com/s/dx85e6p9eh1qzj8/FL_CMI_73_18.csv?dl=1"
# 
# FL_CMI = read.csv(file_path_local, header = T,stringsAsFactors = F)
# 
# file_path_local="~/Dropbox (UFL)/Weather Data Analysis/2021/FL extreme weather events analysis/Florida_hourly_NCDC.csv"
# file_path_dropbox="https://www.dropbox.com/s/h81j6kh50bq69ap/Florida_hourly_NCDC.csv?dl=1"



DtF_NCDC=read.csv('C:/Users/Ruoyao Qin/Desktop/FAWN_data/Florida_hourly_NCDC.csv',
                  sep=',',header=T,stringsAsFactors = F) %>%
  mutate(Time=ymd_hms(Time)) %>% 
  rename(Rain = Precip_mm)

# DtF_FAWN=read.csv('D:/FAWN_data/Florida_hourly_FAWN.csv',
#                   sep=',',header=T,stringsAsFactors = F) %>%
#   mutate(Time=ymd_hms(Time)) %>%
#   rename(Rain = Pecipit_mm)
# 

# 
# DtF_ALL = bind_rows(DtF_NCDC, DtF_FAWN)
# 
# 


#NCDC
DtF_NCDC %>% 
  mutate(Loc=Location) %>% 
  group_by(Loc) %>% 
  nest()->DtF_nest_all

DtF_nest_all %>% 
  mutate(Raw_dt_all = map(data, ~data_preprosessing(.))) %>% 
  mutate(Raw_dt_evt_all = map(Raw_dt_all, ~Get_Press_Evt(.)))->DtF_map_all

DtF_map_all %>% 
  select(Loc,Raw_dt_all) %>% 
  unnest(cols = c(Raw_dt_all)) %>%
  ungroup() %>% 
  select(-Loc)->Raw_dt_all_loc_all

DtF_map_all %>% 
  select(Loc,Raw_dt_evt_all) %>% 
  unnest(cols = c(Raw_dt_evt_all)) %>% 
  ungroup() %>%
  select(-Loc)->Raw_dt_evt_all_loc_all


# 
# data('Ukkel_RR')
# dates <- seq(from = as.Date('2017-06-30'), to = as.Date('2018-06-25'), by = 1)
# SPI_1 <- standardized.index(data = Ukkel_RR, agg.length = 30, index.out = dates)




Raw_dt_all_loc_all %>% 
  mutate(Yr = year(Time),
         Mon = month(Time),
         DAY = day(Time)) %>% 
  group_by(Yr, Mon, DAY, Location) %>% 
  summarise(Day_rain = sum(Rain),
            Day_Pressure_chng.av = max(Pressure_hPa) - min(Pressure_hPa),
            Temp_dew_diff = mean(Temp_C - DewPt_C)) %>% 
  mutate(Time_SPI = (as.Date(paste(Yr, Mon, DAY, sep = '-')))) %>% 
  ungroup() %>% 
  select(Time_SPI, Day_rain, Location, Yr, Mon, Day_Pressure_chng.av, Temp_dew_diff)-> SPI_RAW_1


SPI_RAW_1 %>%
  filter(Yr >= '1965' &  Yr <= '2015') %>% 
  group_by(Yr, Mon, Location) %>% 
  nest() -> spi_nest_ncdc


spi_a_ncdc = xts(SPI_RAW_1$Day_rain, order.by = SPI_RAW_1$Time_SPI) 

SPI_fun = function(data){
  index = as.data.table(data)$Time_SPI
  rain = as.data.table(data)$Day_rain
  dates <- seq(from = min(index), to = max(index), by = 1)
  SPI = standardized.index(data = spi_a_ncdc, agg.length = 30, index.out = dates)
  return (data.frame(time_stamp = index(SPI), Daily_SPI = coredata(SPI)))
}

spi_nest_ncdc %>% 
  mutate(SPI_final = map(data, ~SPI_fun(.))) -> SPI_map


# SPI_map %>% 
#   # select(-data) %>% 
#   unnest(cols = c(SPI_final)) %>% 
#   ungroup() -> SPI_all


SPI_map %>% 
   select(-SPI_final) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() -> SPI_all_new


SPI_map %>% 
  select(-data) %>% 
  unnest(cols = c(SPI_final)) %>% 
  ungroup() -> SPI_all_spi


SPI_all_spi %>% 
  left_join(., SPI_all_new, by = c('time_stamp' = 'Time_SPI', 'Location' = 'Location')) %>% 
  select(Location, time_stamp,value,Day_rain, Day_Pressure_chng.av, Temp_dew_diff)-> SPI_ult




SPI_ult %>% 
  filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>%
  # mutate(wet_dry = ifelse(between(Mon,4,10), 'wet', 'dry')) %>% 
  # filter(CMI >= 0.5 | CMI <= -0.5) %>%
  # select(-Time) %>% 
  # unique() %>% 
  ggplot(aes(x=value, y=Temp_dew_diff))+
  geom_jitter(size = 0.5)+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  # geom_smooth(aes(y=CMI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("SPI")+
  xlab("Mean of day (Temp - dew point)")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  # ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))



SPI_all_final %>% 
  # filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>%
  group_by(Location, Yr, Mon) %>% 
  summarise(SPI = mean(value),
            pressure_chng = mean(Day_Pressure_chng.av),
            temp_dew = mean(Temp_dew_diff)) %>% 
  ggplot(aes(x=pressure_chng))+
  geom_jitter(aes(y=SPI), size = 0.5)+
  geom_smooth(aes(y=SPI), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("SPI")+
  xlab("pressure change in month")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  # ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))



SPI_ult %>% 
  # filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US') %>% 
  # mutate(wet_dry = ifelse(between(month(time_stamp),6,10), 'wet', 'dry')) %>% 
  # filter(CMI >= 0.5 | CMI <= -0.5) %>%
  # select(-Time) %>% 
  # unique() %>% 

  group_by(Location, year(time_stamp), month(time_stamp), week(time_stamp)) %>% 
  summarise(spi.av = mean(value, na.rm = T),
            pre_sum = sum(Day_Pressure_chng.av,na.rm = T),
            diff_T_D = sum(Temp_dew_diff,na.rm = T)) %>% 
  filter(pre_sum != 0) %>% 
  ggplot(aes(x=pre_sum, colour = factor(round(diff_T_D %/% 40)*40), y=spi.av))+
  geom_density_2d()+
  # geom_jitter(size = 1)+
  # scale_x_log10()+
  #scale_colour_gradientn(colours=rainbow(4))
  # scale_colour_gradientn(colours=rainbow(4))         
  # stat_density_2d(aes(y=CMI,fill=(..level..*100)),geom='polygon',color="white")+
  # scale_fill_gradient("Density %",low=alpha("#6e6d6d", 0.3),high=alpha("#242323",0.3))+
  # guides(fill = guide_colorbar(barwidth = 10))+
  # geom_smooth(aes(y=value), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  ylab("SPI")+
  xlab("pre_sum")+
  #scale_x_datetime(breaks = date_breaks(breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  #scale_colour_manual("",values=c("black"))+
  #guides(colour = guide_legend(override.aes = list(fill=NA)))+
  #ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=12),legend.position = "bottom")+
  # ggtitle('JACKSONVILLE 1973 - 2018')+
  theme(plot.title = element_text(hjust = 0.5))












