#install.packages('lubridate')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('magrittr')
#install.packages('RcppRoll')
#install.packages('wrapr')
#install.packages('sqldf')
#install.packages('repmis')

Libs=c('lubridate','dplyr','tidyr','magrittr','RcppRoll','wrapr','knitr','repmis','data.table','magrittr','ggplot2','scales')
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


# read table via dropbox link
# test_dp <- read.csv("https://www.dropbox.com/s/10qbo3spd4r7x1s/Florida_hourly_NCDC.csv?dl=1",
#                     sep=',',header=T,stringsAsFactors = F)[c('Time','Location','Precip_mm')] %>%
#   mutate(Time=ymd_hms(Time)) %>%
#   rename(Rain = Precip_mm) %>%
#   filter(Location == 'JACKSONVILLE INTERNATIONAL AIRPORT, FL US')



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


DtF=read.csv('C:/Users/Chi Zhang/Desktop/PhD_Second/Precipitation Code/Florida_hourly_NCDC.csv',
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
DtF_clean <- DtF %>% anti_join(df)

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


# fill gap, do moving average calculate lag pressure lag of 24-hour
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
#<<<<<<< HEAD

  mutate(PreDry_Dur_hr=lag(Dur_hr)) %>% 
  #filter(TotalPrecip_mm>0) %>%
  filter(TotalPrecip_mm == 0) %>%

  #mutate(PreDry_Dur_hr=lag(Dur_hr)) %>% 
  mutate(PreRain_Dur_hr=lag(Dur_hr)) %>% 
  filter(TotalRain==0) %>%  write.table('./Drought_Evt.csv',row.names = FALSE,sep = ',')
#filter(TotalRain>0) %>%  write.table('./Rain_Evt.csv',row.names = FALSE,sep = ',')

#=======
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
  Get_Press_Evt(.) -> Raw_dt_evt
#<<<<<<< HEAD
#>>>>>>> 78d3b4ec4b927358a9a46bd2a05c431696671742



# Within a rain PCE, plot temp, rh and dew point
DtF_sep %>% 
  Get_Press_Evt_lab(.) -> Raw_dt



breaks='1 days'
Raw_dt %>% 
  filter(data.table::between(Time,ymd_hms('1985-12-11 01:00:00'),ymd_hms('1985-12-14 01:00:00'))) %>% 
  ggplot(aes(x=Time))+
  geom_area(aes(y=Pressure_chng.av,fill="Air pressure change (hPa)"),alpha=0.3)+
  geom_bar(aes(y=Rain,colour="Precipitation (mm)",width=2),stat="identity")+
  geom_line(aes(y=Temp_C,color='Temperature (C)'),size=1)+
  geom_line(aes(y=DewPt_C,color='Dew point'),size=0.5)+ 
  geom_point(aes(y=RH,color='RH'),size=2)+ 
  scale_x_datetime(breaks = breaks_width(width = breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  scale_colour_manual("",values=c("blue","black","#729e6f","red"))+
  scale_fill_manual(values=c("grey10"))+
  guides(colour = guide_legend(override.aes = list(fill=NA)))+
  ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=16),legend.position = "bottom")

#No rain 


breaks='1 days'
Raw_dt %>% 
  filter(data.table::between(Time,ymd_hms('1993-09-27 18:00:00'),ymd_hms('1993-10-02 01:00:00'))) %>% 
  ggplot(aes(x=Time))+
  geom_area(aes(y=Pressure_chng.av,fill="Air pressure change (hPa)"),alpha=0.3)+
  geom_bar(aes(y=Rain,colour="Precipitation (mm)",width=2),stat="identity")+
  geom_line(aes(y=Temp_C,color='Temperature (C)'),size=1)+
  geom_line(aes(y=DewPt_C,color='Dew point'),size=0.5)+ 
  geom_point(aes(y=RH,color='RH'),size=2)+ 
  scale_x_datetime(breaks = breaks_width(width = breaks)) +
  #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
  scale_colour_manual("",values=c("blue","black","#729e6f","red"))+
  scale_fill_manual(values=c("grey10"))+
  guides(colour = guide_legend(override.aes = list(fill=NA)))+
  ylab("Value")+
  theme_Result+
  theme(legend.text =element_text(size=16),legend.position = "bottom")

# Fig.8
# PCE rain probability on EPC & Rain depth on EPC -----------

# Raw_dt_Evt %>% 
#   mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
#   mutate(Evt_Press_chng=round(Sum_Press_Delta/80)*80,
#          RainEvt=St_Rain>0) %>% 
#   group_by(Evt_Press_chng) %>% 
#   summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n()) %>% 
#   ggplot(aes(x=Evt_Press_chng,y=RainProb))+
#   geom_line(size=2)+
#   stat_smooth(method='loess')+
#   scale_y_continuous(labels = scales::percent)+
#   ylab('Rain Probability')+
#   xlab('Event Pressure Change (hPa*hr)')+
#   xlim(-1200,1000)+
#   theme_Result


require(gridExtra)
Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
  mutate(Evt_Press_chng=round(Sum_Press_Delta/20)*20,
         RainEvt=St_Rain>0) %>% 
  group_by(Evt_Press_chng) %>% 
  summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n(),
            samplesize=n()) %>% 
  ggplot(aes(x=Evt_Press_chng,y=RainProb))+
  geom_area(size=2,fill="grey")+
  scale_y_continuous(labels = scales::percent,breaks = c(0,0.5,1))+
  ylab('POP')+
  xlab('Event Pressure Change (hPa)')+
  xlim(-900,900)+
  theme_Result +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())->P_area


Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
         Sum_Precip=as.numeric(Sum_Precip)) %>%
  filter(Sum_Precip>0) %>% 
  select(Sum_Press_Delta,Sum_Precip) %>% 
  ggplot(aes(x=Sum_Press_Delta))+
  stat_density_2d(aes(y=Sum_Precip,fill=(..level..*100)),geom='polygon',color="white")+
  geom_point(aes(y=Sum_Precip),alpha=0.2,size=0.7)+
  geom_smooth(aes(y=Sum_Precip,group = 1, color = "Local Fit Line"), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  scale_y_log10()+
  scale_color_manual("",values=c("black"))+
  #scale_alpha_continuous("Density %")+
  scale_fill_gradient("Density %",low=alpha("#CCCCCC", 0.3),high=alpha("#5C5A5A",0.3))+
  guides(fill = guide_colorbar(barwidth = 10))+
  ylab('Precipitation Depth (PD) (mm) in log scale')+
  xlab('Event Pressure Change (EPC) (hPa)')+
  xlim(-900,900)+
  theme_Result -> P_den


gA <- ggplotGrob(P_area)
gB <- ggplotGrob(P_den)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2)))

g <- arrangeGrob(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2))) #generates g
ggsave(file="./PCE vs Rain Prob and Depth.jpg", g, width=10,height=10)


#Fig 9
# Monthly temperature vs PCE frequency--------------------

Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
         MonT=as.numeric(MonT)) %>% 
  mutate(PCE_mon=month(St)) %>% #pull(Sum_Press_Delta) %>% abs %>% summary
  filter(abs(Sum_Press_Delta)>90) %>% 
  group_by(PCE_mon,MonT) %>% 
  tally %>% 
  #filter(Loc=="PHL",data.table::between(PCE_mon,7,9)) %>% 
  mutate(HlfYr=ifelse(PCE_mon<7,"Jan~Jun","Jul~Dec")) %>% 
  ggplot(aes(x=MonT,y=n))+
  geom_point(alpha=0.5,size=1,color="grey5")+
  #geom_boxplot(aes(,y=n))+
  facet_grid(.~HlfYr)+
  stat_smooth(se=FALSE)+
  labs(y="Monthly Frequency of Moderate and Intensive PCEs",
       x="Average Monthly Temperature (AMT) (?C)")+
  theme_Result+
  theme(strip.text.x = element_text(size = 14))
ggsave(file="./Monthly temperature vs PCE frequency.av.jpg", width=10,height=7)


#Fig 11
# Rain Probability of PCEs by AMT --------------------------

#For DePCEs
Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>% 
  mutate(RainEvt=St_Rain>0,
         YrSide=ifelse(Mon<7,"Jan ~ Jun","Jul ~ Dec"),
         MonT=round(MonT/0.2)*0.2) %>% 
  filter(Sum_Press_Delta<0) %>% 
  group_by(MonT,YrSide) %>% 
  summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n()) %>% 
  ggplot(aes(x=MonT,y=RainProb))+
  geom_point(size=1)+
  stat_smooth(method='loess')+
  facet_grid(YrSide~.)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=seq(from=-6,to=28,by=2))+
  ylab('Probability of Precipitation (POP)')+
  xlab(expression(paste("Average Monthly Temperature (AMT) (",degree,"C)")))+
  Plot_theme

#For InPCEs
Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>% 
  mutate(RainEvt=St_Rain>0,
         YrSide=ifelse(Mon<7,"Jan ~ Jun","Jul ~ Dec"),
         MonT=round(MonT/0.2)*0.2) %>% 
  filter(Sum_Press_Delta>0) %>% 
  group_by(MonT,YrSide) %>% 
  summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n()) %>% 
  ggplot(aes(x=MonT,y=RainProb))+
  geom_point(size=1)+
  stat_smooth(method='loess')+
  facet_grid(YrSide~.)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=seq(from=-6,to=28,by=2))+
  ylab('Rain Probability')+
  xlab(expression(paste("Average Monthly Temperature (",degree,"C)")))+
  Plot_theme

# for Both DePCEs and InPCEs
Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
  mutate(RainEvt=St_Rain>0,
         YrSide=ifelse(Mon<7,"Jan ~ Jun","Jul ~ Dec"),
         MonT=round(MonT/0.2)*0.2, 
         PCEType=ifelse(Sum_Press_Delta<0,"DePCE","InPCE")) %>% 
  group_by(MonT,YrSide,PCEType) %>% 
  summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n()) %>% 
  ggplot(aes(x=MonT,y=RainProb,color=PCEType,linetype=PCEType))+
  geom_point(aes(shape=PCEType),size=2.5)+
  stat_smooth(method='loess',se=F)+
  facet_grid(YrSide~.)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks=seq(from=-6,to=28,by=2))+
  scale_color_discrete("")+
  scale_linetype_manual("",values=c("solid","longdash"))+
  scale_shape_manual("",values=c(4,1))+
  ylab('Probability of Precipitation (POP)')+
  xlab(expression(paste("Average Monthly Temperature (AMT) (",degree,"C)")))+
  Plot_theme+
  theme(legend.key.width=unit(3,"line"))

ggsave(file="./AMT vs PCE Rain Prob facet on PCE types.jpg", width=10,height=6)


#Fig 12


# Rain Probability of PCE by EPC and AMT------------------

# Generate Locfit models for different half years
Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>% 
  mutate(RainEvt=St_Rain>0,
         YrSide=ifelse(Mon<7,"Jan ~ Jun","Jul ~ Dec"),
         Sum_Press_Delta=round(Sum_Press_Delta),
         MonT=round(MonT/0.2)*0.2) %>% 
  group_by(Sum_Press_Delta,MonT,YrSide) %>% 
  summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n())-> Dt_4_Locfit

library(locfit)
Locfit_Jan_Jun=locfit(RainProb~MonT+Sum_Press_Delta,data=Dt_4_Locfit %>% filter(YrSide=="Jan ~ Jun"))
Locfit_Jul_Dec=locfit(RainProb~MonT+Sum_Press_Delta,data=Dt_4_Locfit %>% filter(YrSide=="Jul ~ Dec"))

newdt=data.frame(MonT=NULL,Sum_Press_Delta=NULL)
for (i in seq(from=4,to=30,by=0.5))
{
  x=data.frame(MonT=rep(i,20),Sum_Press_Delta=seq(from=-900,to=1000,by=100))
  newdt=rbind(newdt,x)
}
# Predict values for different locfit
Rprob_plot=rbind(newdt %>% mutate(RainProb=predict(Locfit_Jan_Jun,newdata=newdt)) %>% mutate(YrSide="Jan ~ Jun"),
                 newdt %>% mutate(RainProb=predict(Locfit_Jul_Dec,newdata=newdt)) %>% mutate(YrSide="Jul ~ Dec"))

x=ggplot()+
  facet_grid(YrSide~.)+
  #geom_point(aes(color=RainProb))
  stat_contour(data=Rprob_plot,aes(x=MonT,y=Sum_Press_Delta,z=RainProb*100,color=..level..))+
  scale_color_continuous()

library(directlabels)
direct.label(x,list("top.points"))

# Plotting

p=Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>% 
  mutate(RainEvt=St_Rain>0,
         YrSide=ifelse(Mon<7,"Jan ~ Jun","Jul ~ Dec"),
         Sum_Press_Delta=round(Sum_Press_Delta/20)*20,
         MonT=round(MonT)) %>% 
  group_by(Sum_Press_Delta,MonT,YrSide) %>% 
  summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n()*100) %>% 
  ggplot(aes(x=MonT,y=Sum_Press_Delta))+
  geom_tile(aes(fill=RainProb),alpha=0.7)+
  facet_grid(YrSide~.)+
  scale_fill_gradient(low="tan", high="cyan3",
                      guide=guide_colorbar(title='Probability of Precipitation (POP) (%)',barwidth = 20, barheight = 1))+
  scale_x_continuous(breaks=seq(4,30,by=3))+
  scale_y_continuous(breaks=seq(-900,1000,by=400))+
  stat_contour(data=Rprob_plot,aes(x=MonT,y=Sum_Press_Delta,z=RainProb*100,color=..level..),size=1,breaks=c(10,20,30,40,60,80,90,100))+
  scale_color_gradient(low='grey34',high='tomato1')+
  labs(y='PCE Pressure Change (EPC) (hPa)',
       x=expression(paste("Average Monthly Temperature (AMT) (",degree,"C)")))+
  theme_Result+
  theme(legend.title=element_text(size=18,face='plain'),
        strip.text.y = element_text(size = 14))


direct.label(p, list("top.points",color='black',rot=15,cex=1.2))

ggsave(file="./Fig 12.jpg", width=10,height=6)


# 5% extreme situation (Fig 8,9,11,12)

cut_95th <- quantile(Raw_dt_evt$Sum_Precip,0.95)
Raw_dt_evt %>% filter(Sum_Precip>cut_95th) -> extreme_Raw_dt_evt

# Fig.8_extreme

require(gridExtra)
extreme_Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
  mutate(Evt_Press_chng=round(Sum_Press_Delta/20)*20,
         RainEvt=St_Rain>0) %>% 
  group_by(Evt_Press_chng) %>% 
  summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n(),
            samplesize=n()) %>% 
  ggplot(aes(x=Evt_Press_chng,y=RainProb))+
  geom_area(size=2,fill="grey")+
  scale_y_continuous(labels = scales::percent,breaks = c(0,0.5,1))+
  ylab('POP')+
  xlab('Event Pressure Change (hPa)')+
  xlim(-900,900)+
  theme_Result +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())->P_area


extreme_Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
         Sum_Precip=as.numeric(Sum_Precip)) %>%
  filter(Sum_Precip>0) %>% 
  select(Sum_Press_Delta,Sum_Precip) %>% 
  ggplot(aes(x=Sum_Press_Delta))+
  stat_density_2d(aes(y=Sum_Precip,fill=(..level..*100)),geom='polygon',color="white")+
  geom_point(aes(y=Sum_Precip),alpha=0.2,size=0.7)+
  geom_smooth(aes(y=Sum_Precip,group = 1, color = "Local Fit Line"), method = "loess", size = 1.5, linetype = 5, se = FALSE)+
  scale_y_log10()+
  scale_color_manual("",values=c("black"))+
  #scale_alpha_continuous("Density %")+
  scale_fill_gradient("Density %",low=alpha("#CCCCCC", 0.3),high=alpha("#5C5A5A",0.3))+
  guides(fill = guide_colorbar(barwidth = 10))+
  ylab('Precipitation Depth (PD) (mm) in log scale')+
  xlab('Event Pressure Change (EPC) (hPa)')+
  xlim(-900,900)+
  theme_Result -> P_den


gA <- ggplotGrob(P_area)
gB <- ggplotGrob(P_den)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2)))

g <- arrangeGrob(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2))) #generates g
ggsave(file="./Fig 8_extreme.jpg", g, width=10,height=10)

# Fig.9_extreme

Raw_dt_evt %>% 
  mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
         MonT=as.numeric(MonT)) %>% 
  mutate(PCE_mon=month(St)) %>% #pull(Sum_Press_Delta) %>% abs %>% summary
  filter(abs(Sum_Press_Delta)>90) %>% 
  group_by(PCE_mon,MonT) %>% 
  tally %>% 
  #filter(Loc=="PHL",data.table::between(PCE_mon,7,9)) %>% 
  mutate(HlfYr=ifelse(PCE_mon<7,"Jan~Jun","Jul~Dec")) %>% 
  ggplot(aes(x=MonT,y=n))+
  geom_point(alpha=0.5,size=1,color="grey5")+
  #geom_boxplot(aes(,y=n))+
  facet_grid(.~HlfYr)+
  stat_smooth(se=FALSE)+
  labs(y="Monthly Frequency of Moderate and Intensive PCEs",
       x="Average Monthly Temperature (AMT) (?C)")+
  theme_Result+
  theme(strip.text.x = element_text(size = 14))
ggsave(file="./Monthly temperature vs PCE frequency.av.jpg", width=10,height=7)







































































