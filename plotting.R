
# Plot of PCE Definition------------------
x=seq(0,20,0.01)
y=sin(x)
pl=data.frame(x,y)

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

library(magrittr)
library(ggplot2)
theme_Result= theme(panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(colour = "light grey"),
                    panel.border = element_rect(colour="black",fill=NA),
                    line = element_line(colour = "black", size = 0.5, linetype = 1),
                    axis.text.x = element_text(size=0,colour='black'),
                    axis.text.y = element_text(size=16,colour='black'),
                    axis.title.y = element_text(size = 18),
                    axis.title.x = element_text(size = 18),
                    plot.title = element_text(size = 20),
                    legend.text=element_text(size=18),
                    legend.title=element_text(size=18,face='plain'),
                    legend.position='bottom')

# load Climatic data and pre processing----------------
library(lubridate)
library(tidyverse)
library(padr)
library(RcppRoll)

Location_dt=tibble(USAF=c(725090,999999,724080,999999,725030),
                   NCDC=c(14739,13739,13739,14732,14732),
                   Loc=c("BOS","PHL","PHL","NYC","NYC"))

Dt=NULL
path="C:/Users/Chi Zhang/Desktop/PhD_Second/Precipitation Code/Data"
for (n in list.files(path,pattern=".txt$"))
{
  read_csv(paste(path,n,sep="/"),
           col_names = c('USAF',
                         'NCDC',
                         'Date',
                         'HrMn',
                         'I',
                         'Type',
                         'QCP',
                         'Temp','Temp_Q',
                         'DewPt','DewPt_Q',
                         'SLP','SLP_Q',
                         'Precip_Pr1','Precip_Amt1',"Precip_I1","Precip_Q1",
                         'Precip_Pr6','Precip_Amt6',"Precip_I6","Precip_Q6",
                         'Precip_Pr24','Precip_Amt24',"Precip_I24","Precip_Q24",
                         'Precip_Pr99','Precip_Amt99',"Precip_I99","Precip_Q99",
                         'RHX'), 
           
           col_types = cols(I=col_character(),
                            Precip_Pr1=col_character(),
                            Precip_Pr6=col_character(),
                            Precip_Pr24=col_character(),
                            Precip_Pr24=col_character(),
                            X31 = col_skip()), 
           skip = 2) %>% 
    bind_rows(Dt)->Dt
}

Dt %>% 
  left_join(Location_dt,by=c("USAF","NCDC")) %>% 
  select(-USAF,-NCDC,-I,-Type,-QCP,Temp_Q,SLP_Q,DewPt_Q,-ends_with("6"),-ends_with("24"),-ends_with("99")) %>% 
  unite(Time,Date,HrMn,sep=" ") %>% 
  #Filter out the invalid data
  mutate(Time=ymd_hm(Time),
         Temp=ifelse(Temp>900,NA,Temp),
         DewPt=ifelse(DewPt>900,NA,DewPt),
         SLP=ifelse(SLP>9000,NA,SLP),
         Precip_Amt1=ifelse(Precip_Pr1!="01" | Precip_Q1 %in% c("2","3","6","7"),NA,Precip_Amt1),
         RHX==ifelse(RHX>900,NA,RHX)) %>%
  select(-Precip_Q1,-Precip_Pr1,-Precip_I1) %>% 
  rename(Precip=Precip_Amt1) %>% 
  mutate(Time=round_date(Time,"hour")) %>% 
  group_by(Time,Loc) %>% 
  summarise(Temp=mean(Temp,na.rm =T),
            DewPt=mean(DewPt,na.rm =T),
            SLP=mean(SLP,na.rm =T),
            Precip=mean(Precip,na.rm=T),
            RHX=mean(RHX,na.rm=T)) %>% 
  arrange(Time) %>% 
  ungroup->Climate_Dt


pad_loc=function(x)
{
  x %>% distinct(Loc) %>% pull(Loc)->Location
  x %>% 
    pad(interval="hour") %>% 
    replace_na(list(Loc=Location)) %>% 
    return
}
# Function to manipulate data
Climate_Dt %>%  
  group_by(Loc) %>% 
  do(pad_loc(.))  %>% 
  group_by(Loc) %>% 
  #Fill gap
  mutate(SLP.spl=spline(x=Time,y=SLP,xout=Time)$y,
         Temp.spl=spline(x=Time,y=Temp,xout=Time)$y,
         DewPt.spl=spline(x=Time,y=DewPt,xout=Time)$y,
         RHX.spl=spline(x=Time,y=RHX,xout=Time)$y) %>%
  #Moving Average
  mutate(Temp.av=roll_mean(Temp.spl,n=24,align='center',fill=NA),
         SLP.av=roll_mean(SLP.spl,n=24,align='center',fill=NA),
         DewPt.av=roll_mean(DewPt.spl,n=24,align='center',fill=NA),
         RHX.av=roll_mean(RHX.spl,n=24,align='center',fill=NA)) %>% 
  #Change of Pressure in 24 hours
  mutate(SLP_chng.av=SLP.av-lag(SLP.av,24)) %>% 
  ungroup->Curated_Climate_Dt



#Precip Evt Separation function for 5 min interval

Precip_Evt_Sep= function(dt,IntE_P=4)
  #dt:       data of time and rain
  #IntE_P:   Inter event period 
  #           (time step based your time interval
  #            For example: in a 5 min time interval series
  #            a 4 hour inter event period is corresponding to
  #            48 for IntE_P)
{
  #The header of time and rain should be
  # Time    Precip
  
  dt %>% 
    #select(Time,Precip) %>% 
    replace_na(list(Precip=0)) %>% 
    mutate(Cum_Precip_4hr_L=roll_sum(Precip,IntE_P+1,align='left',fill=0)-Precip,
           Cum_Precip_4hr_R=roll_sum(Precip,IntE_P+1,align='right',fill=0)-Precip) %>% 
    #Start of Rain
    mutate(StR=ifelse((Cum_Precip_4hr_R==0 & Precip>0),1,0)) %>% 
    #Start of Dry
    mutate(StD=ifelse(lag(Cum_Precip_4hr_L)==0 & lag(Precip)>0,1,0)) %>% 
    replace_na(list(StR=0,StD=0)) %>% 
    mutate(Evt_lab=StR+StD) %>% 
    mutate(Evt_lab=cumsum(Evt_lab)) %>% 
    select(-Cum_Precip_4hr_L,-Cum_Precip_4hr_R) %>% 
    return
}

# Convert Climate data into pressure events series------------
Get_Press_Evt_lab=function(Dt) 
{
  Dt %>% 
    arrange(Time)%>%
    mutate(Mon=month(Time)) %>% 
    # Roll average over 24 hours
    mutate(Press_Evt_lab=ifelse(SLP_chng.av*lag(SLP_chng.av)<=0 & lag(SLP_chng.av)!=0,1,0)) %>% 
    mutate(Press_Evt_lab=ifelse(is.na(Press_Evt_lab),0,Press_Evt_lab)) %>% 
    mutate(Press_Evt_lab=cumsum(Press_Evt_lab)) %>% 
    return
}


Curated_Climate_Dt %>% 
  group_by(Loc) %>% 
  do(Precip_Evt_Sep(.)) %>% 
  do(Get_Press_Evt_lab(.))->Raw_dt


# summarise measures for pressure change events
Get_Press_Evt=function(Dt)
{
  Dt %>% 
    mutate(Mon=month(Time),
           Yr=year(Time)) %>% 
    group_by(Yr,Mon) %>% 
    summarise(MonT=mean(Temp,na.rm=T))->Dt_MonT
  
  Dt %>% 
    filter(Press_Evt_lab>0,
           Press_Evt_lab<max(Press_Evt_lab)) %>% 
    group_by(Press_Evt_lab,Loc) %>% 
    summarise(St=min(Time),
              End=max(Time),
              Dur=as.numeric(max(Time)-min(Time),units='hours')+1,
              Sum_Press_Delta=sum(SLP_chng.av),
              Press_NA=sum(is.na(SLP)),
              Temp_NA=sum(is.na(Temp)),
              Sum_Precip=sum(Precip,na.rm=T),
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

Raw_dt_Evt=Raw_dt %>% Get_Press_Evt

Raw_dt_Evt %<>% 
  mutate(Season = as.character(cut(
    month(St),
    c(2, 5, 8, 11),
    c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
  ))) %>%
  replace_na(list(Season = "Winter (DJF)"))

#----------------------------------------------------------------------


# Data quality check -------------
Raw_dt %>% 
    filter(!is.na(Temp)) %>% 
    group_by(Loc) %>% 
    filter(!is.na(Temp)) %>% 
    arrange(Time) %>% 
    mutate(gap=as.numeric(lead(Time)-Time)) %>%  
    ungroup() %>% 
    select(Loc,gap) %>% 
    #filter(gap==9) %>% View
    #filter(gap>1) %>% 
    mutate(gp=cut(gap,breaks=c(0,1,6,24,Inf))) %>% 
    group_by(Loc,gp) %>% 
    tally %>% 
    View


## Plot of data curation process 
Raw_dt %>% 
    filter(data.table::between(Time,ymd("1996-8-28"),ymd("1996-9-5")),Loc=="PHL") %>% 
    select(Time,Temp,Temp.av,Precip) %>% 
    mutate(Temp=ifelse(data.table::between(Time,ymd("1996-8-31"),ymd_h("1996-8-31 12")),NA,Temp)) %>% 
    mutate(Temp=as.numeric(Temp),
           Temp.av=as.numeric(Temp.av),
           Precip=as.numeric(Precip)) %>% 
    mutate(Temp_chg=Temp-lag(Temp),
           indx=hour(Time))->Smp_dt



preq=24
Daily_flux_mdl=lm(data=Smp_dt,
                  Temp_chg~sin(2*pi*indx/preq)+cos(2*pi*indx/preq)+
                      sin(4*pi*indx/preq)+cos(4*pi*indx/preq))

fit_df=data.frame(fit_val=fitted(Daily_flux_mdl)[1:24],indx=0:23)


Smp_dt %<>% 
    left_join(fit_df,by="indx")

Smp_dt %>% 
    filter(is.na(Temp)) %>% 
    mutate(cum_chg=cumsum(fit_val)) %>% 
    select(Time,cum_chg)-> cumfit

Smp_dt %>% 
    mutate(Temp_dum=Temp) %>% 
    left_join(cumfit,by="Time") %>%
    replace_na(list(Temp_dum=24.5,cum_chg=0)) %>% 
    mutate(Temp_cure=ifelse(is.na(Temp) | is.na(lead(Temp)) | is.na(lag(Temp)),Temp_dum+cum_chg,NA))%>% 
    select(Time,Temp_cure,Temp,Temp_chg,fit_val) %>% 
    rename(`Curated Temperature`=Temp_cure,
           `Raw data`=Temp,
           `Houlry temperature change`=Temp_chg,
           `Sinusoidal fit`=fit_val) %>% 
    gather(typ,val,-Time) %>% 
    ggplot(aes(x=Time))+
    geom_line(aes(y=val,linetype=typ,size=typ))+
    scale_size_manual("",values = c(2,1,0.7,0.7),guide=guide_legend(keywidth=1.5))+
    scale_linetype_manual("",values=c("solid","dotted","dashed","solid"),guide=guide_legend(keywidth=1.5))+
    labs(x="",y="Temperature (?C)")+
    Plot_theme

ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\Gap filling plot.jpg", width=10,height=4)



# Plot of Rain probability on hourly air pressure ----------------------------

# Select Location of data
dt=Raw_dt %>% filter(Loc=="NYC")


# Plotting
dt %>% 
    mutate(Precip=as.numeric(Precip),
           SLP=as.numeric(SLP)) %>% 
    mutate(P.or.D=Precip>0,
           SLP_v=SLP%/%1) %>% 
    group_by(SLP_v) %>% 
    summarise(PRate=sum(P.or.D,na.rm=TRUE)/n()) %>% 
    ggplot(aes(x=SLP_v,y=PRate))+
    geom_point()+
    stat_smooth(method='loess',se=F)+
    scale_y_continuous(labels = scales::percent,limits=c(0,1))+
    ylab('')+
    xlab('Air Pressure (hPa)')+
    Plot_theme +
  theme(axis.title.x=element_blank())-> Overall_POP_AP



dt %>% 
    mutate(Precip=as.numeric(Precip),
           SLP=as.numeric(SLP)) %>% 
    mutate(P.or.D=Precip>0,
           SLP_v=SLP%/%1 *1,
           Mon=month(Time, label = TRUE, abbr = FALSE)) %>% 
    group_by(SLP_v,Mon) %>% 
    summarise(PRate=sum(P.or.D,na.rm=TRUE)/n(),
              Occurance=n()) %>% 
    ggplot(aes(x=SLP_v,y=PRate))+
    geom_point()+
    stat_smooth(method='loess',se=F)+
    facet_wrap( ~ Mon, ncol=4)+
    scale_y_continuous(labels = scales::percent,limits=c(0,1))+
    ylab('Probability of Precipitation (POP)')+
    xlab('Air Pressure (hPa)')+
    Plot_theme ->Monthly_POP_AP

require(gridExtra)
gA <- ggplotGrob(Overall_POP_AP)
gB <- ggplotGrob(Monthly_POP_AP)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2),c(2)))

g <- arrangeGrob(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2),c(2))) #generates g
ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\Rain probability on hourly air pressure in NYC LaGuardia International Airport.jpg", 
       g, width=10,height=10)

# Histogram of daily average air pressure (Lumped three cities)  ------------



Raw_dt %>% 
    mutate(SLP=as.numeric(SLP)) %>% 
    mutate(Date=floor_date(Time,'day')) %>% 
    group_by(Date,Loc) %>% 
    summarize(DailySLP=mean(SLP,na.rm=T)) %>% 
    mutate(Month=month(Date)) %>% 
    ggplot()+
    geom_histogram(aes(x=DailySLP),binwidth=1)+
    facet_grid(Month~.,labeller = Monthlab)+
    xlab("Daily Average Air Pressure (hPa)")+
    ylab("Count")+
    #labs(title="Histogram of Daily Average Pressure (Historical)")+
    theme_Result+
    theme_bw(base_size = 20)+
    theme(legend.position = "bottom"
          #,strip.text.y = element_text(size = 15)
    )


# Histogram of pressure change of all precipitation hours (Lumped three cities)---------------

Raw_dt %>% 
    mutate(Precip=as.numeric(Precip),
           SLP_chng.av=as.numeric(SLP_chng.av)) %>% 
    #filter(Loc=='NYC') %>% 
    select(SLP_chng.av,Precip) %>% 
    filter(Precip>0) %>% 
    ggplot()+
    geom_density(aes(SLP_chng.av),fill=1,alpha=0.6)+
    labs(x='Air Pressure Change to The Same Time Previous Day (hPa)',y='Density')+
    theme_Result



# PCEs and precipitation from 1994-11-1 to 1995-1-10 in BOS (Bars: precipitation records, Area: air pressure change) -------------------------------

breaks='1 month'
Raw_dt %>% 
    filter(data.table::between(Time,ymd('1994-11-01'),ymd('1995-1-10')),
           Loc=='BOS') %>% 
    mutate(SLP_chng.av=as.numeric(SLP_chng.av),
           Precip=as.numeric(Precip),
           Temp.av=as.numeric(Temp.av)) %>% 
ggplot(aes(x=Time))+
    #geom_line(aes(y=SLP_chng.av,colour="Air pressure change (hPa)"),alpha=I(0.2))+
    geom_area(aes(y=SLP_chng.av,fill="Air pressure change (hPa)"),alpha=0.3)+
    geom_bar(aes(y=Precip,colour="Precipitation (mm)",width=2),stat="identity")+
    geom_line(aes(y=Temp.av,color='Smoothed Temperature (?C)'),size=1)+
    scale_x_datetime(breaks = date_breaks(breaks)) +
    #scale_y_continuous(sec.axis = sec_axis(~., name = "Smoothed temperature (?C)"))+
    scale_colour_manual("",values=c("black","#706C6C"))+
    scale_fill_manual(values=c("grey10"))+
    guides(colour = guide_legend(override.aes = list(fill=NA)))+
    ylab("Value")+
    theme_Result+
    theme(legend.text =element_text(size=16),legend.position = "bottom")

ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\Sample of PCE on rain and temp.av.jpg", width=10,height=6)

# Monthly temperature vs PCE frequency--------------------

Raw_dt_Evt %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta),
           MonT=as.numeric(MonT)) %>% 
    mutate(PCE_mon=month(St)) %>% #pull(Sum_Press_Delta) %>% abs %>% summary
    filter(abs(Sum_Press_Delta)>90) %>% 
    group_by(PCE_mon,MonT,Loc) %>% 
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
    
ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\Monthly temperature vs PCE frequency.av.jpg", width=10,height=7)


# EPC Histogram on rain and non-rain PCEs----------------------------

Raw_dt_Evt %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>% 
    filter(abs(Sum_Press_Delta)>5.5) %>% 
    {
        dt=.
        ggplot()+
            geom_density(data=dt,aes(Sum_Press_Delta,fill='All PCEs',linetype='All PCEs'),alpha=0.6, size=1)+
            geom_density(data=dt %>% 
                             filter(St_Rain>0),
                         aes(Sum_Press_Delta,fill='Rainy PCEs',linetype='Rainy PCEs'),alpha=0.6)+
            scale_fill_discrete("")+
            scale_linetype_manual("",values=c("dotted","solid"))+
            labs(x='Event Pressure Change (EPC) (hPa)',y='Density')+
            #scale_fill_discrete(guide=F)+
            theme_Result+
            theme(legend.key.width=unit(4,"line"))
    }


ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\EPC Histogram on rain and non-rain PCEs.jpg", width=10,height=6)



# Inter event period analysis ----------------
source("\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\Code\\Event separation period determination.R")

# PCE rain probability on EPC & Rain depth on EPC -----------

Raw_dt_Evt %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
    mutate(Evt_Press_chng=round(Sum_Press_Delta/80)*80,
           RainEvt=St_Rain>0) %>% 
    group_by(Evt_Press_chng) %>% 
    summarise(RainProb=sum(RainEvt,na.rm=TRUE)/n()) %>% 
    ggplot(aes(x=Evt_Press_chng,y=RainProb))+
    geom_line(size=2)+
    stat_smooth(method='loess')+
    scale_y_continuous(labels = scales::percent)+
    ylab('Rain Probability')+
    xlab('Event Pressure Change (hPa*hr)')+
    xlim(-1200,1000)+
    theme_Result



require(gridExtra)
Raw_dt_Evt %>% 
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
    xlim(-1182,1071)+
    theme_Result +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())->P_area


Raw_dt_Evt %>% 
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
    xlim(-1182,1071)+
    theme_Result -> P_den


gA <- ggplotGrob(P_area)
gB <- ggplotGrob(P_den)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2)))

g <- arrangeGrob(gA, gB, layout_matrix = rbind(c(1),c(2),c(2),c(2))) #generates g
ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\PCE vs Rain Prob and Depth.jpg", g, width=10,height=10)

# Average Monthly Temperature in historical observation -----------------

Raw_dt %>% 
    mutate(Temp=as.numeric(Temp)) %>% 
    mutate(Mon=month(Time),
           Yr=year(Time)) %>% 
    group_by(Loc,Mon,Yr) %>% 
    summarize(AMT=mean(Temp,na.rm=T)) %>% 
    filter(!(AMT>20 & Mon<2)) %>% 
ggplot(aes(x= factor(Monthlab(1)[Mon],levels=month.abb),y=AMT))+
    geom_jitter()+
    geom_boxplot(alpha = 0.7,outlier.shape =NA)+
    xlab("")+
    ylab(expression(paste("Average Monthly Temperature (",degree,"C)")))+
    #labs(title="Monthly Temperature")+
    theme_bw()+
    theme_Result+
    theme(legend.position="none"
          ,legend.text =element_text(size=14))

# Histograms of all PCEs and rainy PCEs by AMT--------------

ggplot()+
    geom_histogram(data=Raw_dt_Evt, aes(x=MonT,fill='Pressure Event'),alpha=0.6)+
    geom_histogram(data=Raw_dt_Evt %>% filter(St_Rain>0), aes(x=MonT,fill='Rainy Pressure Event'),alpha=0.6)+
    xlab(expression(paste("Average Monthly Temperature (",degree,"C)")))+
    scale_fill_discrete(guide=F)+
    theme_bw()+
    theme_Result

# Rain Probability of PCEs by AMT --------------------------

#For DePCEs
Raw_dt_Evt %>% 
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
Raw_dt_Evt %>% 
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
Raw_dt_Evt %>% 
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

ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\AMT vs PCE Rain Prob facet on PCE types.jpg", width=10,height=6)

#sample size of a tail
Raw_dt_Evt %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>%
    mutate(RainEvt=St_Rain>0,
           YrSide=ifelse(Mon<7,"Jan ~ Jun","Jul ~ Dec"),
           MonT=round(MonT/0.2)*0.2, 
           PCEType=ifelse(Sum_Press_Delta<0,"DePCE","InPCE")) %>% 
    filter(YrSide=="Jul ~ Dec",
           MonT<-3,
           PCEType=="InPCE") %>% 
    group_by(PCEType,MonT) %>% 
    tally


# Rain depth vs AMT -------------
library(ggExtra)

Centroid=data.frame(Raindepth=c(8.2,6.2,17.5,2.5),
                 AMT=c(22.2,22.7,2.3,2.2),
                 YrSide=c("Summer","Summer"," Winter"," Winter"),
                 PCEType=c("DePCE","InPCE","DePCE","InPCE"))

Raw_dt_Evt %>% 
    mutate(Sum_Precip=as.numeric(Sum_Precip),
           Sum_Press_Delta=as.numeric(Sum_Press_Delta),
           MonT=as.numeric(MonT) ) %>% 
    filter(Sum_Precip>0.01) %>% 
    mutate(
           YrSide=ifelse(MonT<12," Winter","Summer"),
           PCEType=ifelse(Sum_Press_Delta<0,"DePCE","InPCE")) %>% 
    ggplot(aes(x=MonT,y=Sum_Precip,color=PCEType))+
    #geom_point(size=0.3,alpha=0.5)+
    stat_density_2d(aes(linetype=PCEType),size=1.5)+
    geom_point(data=Centroid,aes(x=AMT,y=Raindepth,color=PCEType,shape=PCEType),size=6)+
    annotate("text", label = "Winter", x = 2, y = 7, size = 5)+
    annotate("text", label = "Summer", x = 22, y = 3, size = 5)+
    #facet_grid(.~YrSide,scale="free_x")+
    scale_linetype_manual("",values=c("solid","dashed"))+
    scale_y_log10(breaks=c(0.5,1,5,10,50,100))+
    scale_color_discrete("")+
    #guides(shape=FALSE)+
    scale_shape("")+
    ylab('Precipitation Depth (PD) (mm) in log scale')+
    xlab(expression(paste("Average Monthly Temperature (AMT) (",degree,"C)")))+
    Plot_theme+
    theme(legend.key.width=unit(5,"line"))

ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\AMT vs PCE Rain Depth on PCE types.jpg", width=10,height=6)



# Rain Probability of PCE by EPC and AMT------------------

# Generate Locfit models for different half years
Raw_dt_Evt %>% 
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
for (i in seq(from=-6,to=28,by=0.5))
{
    x=data.frame(MonT=rep(i,25),Sum_Press_Delta=seq(from=-1200,to=1200,by=100))
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

p=Raw_dt_Evt %>% 
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
    scale_x_continuous(breaks=seq(-6,30,by=3))+
    scale_y_continuous(breaks=seq(-1200,1300,by=400))+
    stat_contour(data=Rprob_plot,aes(x=MonT,y=Sum_Press_Delta,z=RainProb*100,color=..level..),size=1,breaks=c(10,20,30,40,60,80,90,100))+
    scale_color_gradient(low='grey34',high='tomato1')+
    labs(y='PCE Pressure Change (EPC) (hPa)',
         x=expression(paste("Average Monthly Temperature (AMT) (",degree,"C)")))+
    theme_Result+
    theme(legend.title=element_text(size=18,face='plain'),
          strip.text.y = element_text(size = 14))


direct.label(p, list("top.points",color='black',rot=15,cex=1.2))




# PCE density by AMT and EPC --------------------


Raw_dt_Evt %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)) %>% 
    mutate(YrSide=ifelse(Mon<7,"Jan ~ Jun","Jul ~ Dec"),
           Sum_Press_Delta=round(Sum_Press_Delta),
           MonT=round(MonT/0.2)*0.2) %>% 
    ggplot(aes(x=MonT,y=Sum_Press_Delta))+
    stat_density2d(aes(fill=(..level..)*10000,alpha=..level..),geom='polygon',colour='black') + 
    facet_grid(YrSide~.)+
    scale_y_continuous(breaks=seq(-1200,1200,by=400))+
    scale_x_continuous(breaks=seq(from=-6,to=28,by=3))+
    scale_fill_continuous(low="green",high="red")+
    guides(alpha="none",
           fill = guide_colorbar(title=expression(paste("Density (", 10^{-4},')')),barwidth = 40, barheight = 1))+
    labs(y='PCE Pressure Change (hPa*hr)',
         x=expression(paste("Average Monthly Temperature (",degree,"C)")))+
    theme_Result+
    theme(legend.title=element_text(size=18,face='plain'),
          strip.text.y = element_text(size = 14))


# Rain depth quantiles on AMT and EPC-----------------

Raw_dt_Evt %>% 
    mutate(Sum_Press_Delta=as.numeric(Sum_Press_Delta)%/%30*30,
           EPC_int=as.numeric(EPC_int),
           MonT=as.numeric(MonT)%/%1*1,
           Dur=as.numeric(Dur),
           Sum_Precip=as.numeric(Sum_Precip))%>% 
    filter(Sum_Precip>0) %>% 
    group_by(MonT,Sum_Press_Delta) %>% 
    summarise(`99.9%`=quantile(Sum_Precip,probs=0.999),
              `99%`=quantile(Sum_Precip,probs=0.99),
              `90%`=quantile(Sum_Precip,probs=0.90),
              `75%`=quantile(Sum_Precip,probs=0.75),
              `50%`=quantile(Sum_Precip,probs=0.50),
              `25%`=quantile(Sum_Precip,probs=0.25)) %>% 
    gather(Qntile,Sum_Precip,-MonT,-Sum_Press_Delta) %>% 
    ggplot(aes(x=MonT,y=Sum_Press_Delta))+
    geom_tile(aes(fill=factor((log(Sum_Precip))%/%0.5*0.5),group=Qntile),alpha=0.7)+ 
    facet_grid(.~Qntile)+
    labs(x=expression(paste("Average Monthly Temperature (AMT) (",degree,"C)")),
         y="Event Pressure Change (EPC) (hPa)")+
    scale_fill_discrete("Precipitation Depth (PD) (mm)",
                        breaks=0:7,
                        labels=signif((exp(0:7)),2))+
    guides(fill = guide_legend(nrow = 1))+
    Plot_theme
    
ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\Rain depth quantiles on AMT and EPC.jpg", width=10,height=6)

# Seasonal Rain depth vs AMT on EPC bins ---------------------


Raw_dt_Evt %>%
    filter(Press_NA<7) %>% 
    mutate(Season = as.character(cut(
        month(St),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)")) %>%
    mutate(
        Sum_Press_Delta = as.numeric(Sum_Press_Delta) %/% 30 * 30,
        #EPC_int = as.numeric(EPC_int),
        MonT = as.numeric(MonT) %/% 1 * 1,
        Dur = as.numeric(Dur),
        Sum_Precip = as.numeric(Sum_Precip)
    ) %>%
    mutate(
        MonT1 = MonT - 1,
        MonT2 = MonT + 1,
        Sum_Press_Delta1 = Sum_Press_Delta - 30,
        Sum_Press_Delta2 = Sum_Press_Delta + 30
    ) %>%
    filter(Sum_Precip > 0) %>%
    select(
        MonT,
        MonT1,
        MonT2,
        Sum_Press_Delta,
        Sum_Press_Delta1,
        Sum_Press_Delta2,
        Season,
        Sum_Precip
    ) %>%
    gather(
        MonT_C,
        MonT,
        -Sum_Press_Delta,
        -Sum_Press_Delta1,
        -Sum_Press_Delta2,
        -Season,
        -Sum_Precip
    ) %>%
    gather(Sum_Press_Delta_C,
           Sum_Press_Delta,
           -MonT_C,
           -MonT,
           -Season,
           -Sum_Precip) %>%
    group_by(MonT, Sum_Press_Delta, Season) %>%
    summarise(
        `95%` = quantile(Sum_Precip, probs = 0.95),
        `75%` = quantile(Sum_Precip, probs = 0.75),
        `50%` = quantile(Sum_Precip, probs = 0.50)
    ) %>%
    gather(Qntile, Sum_Precip, -MonT, -Sum_Press_Delta, -Season) %>%
    mutate(Sum_Press_C = as.character(cut(
        Sum_Press_Delta,
        c(-2000, -500, 0, 500, 2000),
        c(
            "-2000 hPa ~ -500 hPa",
            "-500 hPa ~ 0 hPa",
            "0 hPa ~ 500 hPa",
            "500 hPa ~ 2000 hPa"
        )
    ))) -> Df4Plot


Df4Plot %>%
{
    df = .
    df %>%
        mutate(Season = "All Season") -> df1
    
    ggplot(data = df, aes(x = MonT, y = Sum_Precip
                          , color = Season)) +
        geom_point(alpha = 0.3, size = 0.7) +
        stat_smooth(se = F, size = 1) +
        geom_smooth(
            data = df1,
            aes(x = MonT, y = Sum_Precip),
            se = F,
            linetype = "longdash"
        ) +
        facet_grid(Sum_Press_C ~ Qntile) +
        labs(x = expression(paste(
            "Average Monthly Temperature (AMT) (", degree, "C)"
        )),
        y = "Precipitation Depth (PD) (mm)") +
        #scale_color_discrete("")+
        scale_color_manual(
            "",
            breaks = c(
                "All Season",
                "Fall (SON)",
                "Spring (MAM)",
                "Summer (JJA)",
                "Winter (DJF)"
            ),
            values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00")
        ) +
        ylim(0,150)+
        Plot_theme
}

ggsave(file="\\\\swv.cae.drexel.edu\\personal\\Ziwen\\Writing\\Dissertation\\Chapter8\\images\\Seasonal Rain depth vs AMT on EPC bins.jpg", width=10,height=10)

# Get sample size of tails
Raw_dt_Evt %>%
    mutate(Season = as.character(cut(
        month(St),
        c(2, 5, 8, 11),
        c("Spring (MAM)", "Summer (JJA)", "Fall (SON)")
    ))) %>%
    replace_na(list(Season = "Winter (DJF)")) %>%
    mutate(
        Sum_Press_Delta = as.numeric(Sum_Press_Delta) %/% 30 * 30,
        EPC_int = as.numeric(EPC_int),
        MonT = as.numeric(MonT) %/% 1 * 1,
        Dur = as.numeric(Dur),
        Sum_Precip = as.numeric(Sum_Precip)
    ) %>%
    mutate(
        MonT1 = MonT - 1,
        MonT2 = MonT + 1,
        Sum_Press_Delta1 = Sum_Press_Delta - 30,
        Sum_Press_Delta2 = Sum_Press_Delta + 30
    ) %>%
    filter(Sum_Precip > 0) %>%
    select(
        MonT,
        MonT1,
        MonT2,
        Sum_Press_Delta,
        Sum_Press_Delta1,
        Sum_Press_Delta2,
        Season,
        Sum_Precip
    ) %>%
    gather(
        MonT_C,
        MonT,
        -Sum_Press_Delta,
        -Sum_Press_Delta1,
        -Sum_Press_Delta2,
        -Season,
        -Sum_Precip
    ) %>%
    gather(Sum_Press_Delta_C,
           Sum_Press_Delta,
           -MonT_C,
           -MonT,
           -Season,
           -Sum_Precip) %>%
    filter(Sum_Press_Delta>500,MonT>22) %>% 
        group_by(Season,MonT) %>% 
        tally

