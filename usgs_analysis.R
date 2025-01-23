# -------------------------------------------------------------------#
# Hydrograph Analysis
# Kendra Kaiser
# August 17, 2023
# Standard statistics and figures for hydrologic timeseries
# -------------------------------------------------------------------#

library(dplyr)
library(tidyverse)
library(tidyr)
library(dataRetrieval) #USGS data
library(lubridate) #date transformation
library(smwrBase) # waterYear

# download data 
johnson = 13313000
efsf = 13311250
sugar = 13311450
sfs = 13310700


usgs_sites = c(johnson, efsf, sugar, sfs)#  put all sites in one vector

pCode = "00060" # USGS code for streamflow
site_info<- whatNWISdata(sites= usgs_sites, parameterCd = pCode, outputDataTypeCd ='uv') 
site_info <- site_info %>% dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, alt_va, huc_cd, begin_date, end_date, count_nu)

# Download data from all sites into one dataframe
streamflow_data <- readNWISdv(siteNumbers = site_info$site_no, parameterCd = pCode, startDate = min(site_info$begin_date), endDate = max(site_info$end_date)) %>% renameNWISColumns() %>% data.frame

#Re-format dates and pull out month /day/ water year
streamflow_data$Date <- as.Date(streamflow_data$Date, format = "%Y-%m-%d")
streamflow_data$mo <- month(streamflow_data$Date)
streamflow_data$wy <- as.numeric(as.character(waterYear(streamflow_data$Date, numeric=TRUE)))
streamflow_data$day <- day(streamflow_data$Date)
streamflow_data$site_no <- as.numeric(streamflow_data$site_no)
streamflow_data$doy<- yday(streamflow_data$Date)
streamflow_data$plot_date <- as.Date(paste0(2020, "-", streamflow_data$mo, "-", streamflow_data$day))

day_stats1<- function(site, wy_start, wy_end){
  q<- streamflow_data %>% filter(site_no == site) %>% filter(wy >= wy_start & wy <= wy_end)
  
  monthly_q<- q %>% group_by(doy) %>%
    summarize(avg = mean(Flow, na.rm=TRUE))

  monthly_q$max<- q %>% group_by(doy) %>%
    summarize(max = max(Flow, na.rm=TRUE)) %>% dplyr::select(max) 
  
  monthly_q$min<- q%>% group_by(doy) %>%
    summarize(min = min(Flow, na.rm=TRUE)) %>% dplyr::select(min)
  
  monthly_q$med<- q%>% group_by(doy) %>%
    summarize(med = median(Flow, na.rm=TRUE)) %>% dplyr::select(med)
  
  # need to separate this better
  annual_flows <- q%>% group_by(wy) %>%
    summarize(min = min(Flow, na.rm=TRUE))
  
  quantiles <- quantile(q$Flow, probs =c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) %>% round(1)

  return(list(monthly_q, annual_flows, quantiles))
}

day_stats<-function(site, wy_start, wy_end){
  q<- streamflow_data %>% filter(site_no == site) %>% filter(wy >= wy_start & wy <= wy_end)
 
   q %>% group_by(doy) %>%
    summarize(
      avg= mean(Flow, na.rm=TRUE),
      median = median(Flow, na.rm = TRUE),
      max = max(Flow, na.rm = TRUE),
      min = min(Flow, na.rm = TRUE),
      .groups = "drop"
    )
}

johnson_stats<-mo_stats(johnson)
ggplot(johnson_stats[[1]], aes(mo, min$min), color='r') + 
  geom_line() +
  theme_bw() +
  ylab("Johnson Creek Min Monthly Streamflow (cfs)")

ggplot(johnson_stats[[2]], aes(wy, min), color='r') + 
  geom_point() +
  theme_bw() +
  ylab("Johnson Creek Annual Minimum Streamflow (cfs)")

efsf_stats<-mo_stats(efsf)
ggplot(efsf_stats[[1]], aes(mo, min$min), color='r') + 
  geom_line() +
  theme_bw() +
  ylab("EFSF Min Monthly Streamflow (cfs)")

ggplot(efsf_stats[[1]], aes(mo, max$max), color='r') + 
  geom_line() +
  theme_bw() +
  ylab("EFSF Max Monthly Streamflow (cfs)")

ggplot(efsf_stats[[1]], aes(mo, avg), color='r') + 
  geom_line() +
  theme_bw() +
  ylab("EFSF Avg Monthly Streamflow (cfs)")

ggplot(efsf_stats[[2]], aes(wy, min), color='r') + 
  geom_point() +
  theme_bw() +
  ylab("EFSF above Sugar Annual Minimum Streamflow (cfs)")


sfs_stats<-day_stats(sfs, 1989, 2024) %>% mutate(date = as.Date(doy - 1, origin = "2020-01-01"))
sfs_stats1<- day_stats(sfs, 1989, 1999) %>% mutate(date = as.Date(doy - 1, origin = "2020-01-01"))
sfs_stats2<- day_stats(sfs, 2000, 2010) %>% mutate(date = as.Date(doy - 1, origin = "2020-01-01"))
sfs_stats3<- day_stats(sfs, 2011, 2024) %>% mutate(date = as.Date(doy - 1, origin = "2020-01-01"))

colors<- c("1989-1999" = "black", "2000-2010" = "aquamarine4", "2011-2024"="blue")
ggplot() + 
  geom_line(data= sfs_stats1, aes(date, avg, color = "1989-1999")) +
  geom_line(data= sfs_stats2, aes(date, avg, color = "2000-2010")) +
  geom_line(data= sfs_stats3, aes(date, avg, color = "2011-2024")) +
  theme_bw() +
  scale_x_date(date_breaks = "month", date_labels = "%b")+
  theme(legend.position = c(0.75, 0.75), legend.text = element_text(size=8),legend.title = element_text(size=8)) +
  labs(x= "Day of Year", y="SFS @ Krassel Avg Daily Streamflow (cfs)", color= "Timeframe")
  

ggplot() + 
  geom_line(data= sfs_stats1, aes(date, max, color = "1989-1999")) +
  #geom_line(data= sfs_stats2, aes(date, max, color = "2000-2010")) +
  geom_line(data= sfs_stats3, aes(date, max, color = "2011-2024")) +
  theme_bw() +
  scale_x_date(date_breaks = "month", date_labels = "%b")+
  theme(legend.position = c(0.75, 0.75), legend.text = element_text(size=8),legend.title = element_text(size=8)) +
  labs(x= "Day of Year", y="SFS @ Krassel Max Daily Streamflow (cfs)", color= "Timeframe")

ggplot() + 
  geom_line(data= sfs_stats1[[1]], aes(doy, med$med), color='black') +
  geom_line(data= sfs_stats2[[1]], aes(doy, med$med), color='aquamarine4') +
  geom_line(data= sfs_stats3[[1]], aes(doy, med$med), color='blue') +
  theme_bw() +
  ylab("SFS Median Daily Streamflow (cfs)")

ggplot() + 
  geom_line(data= sfs_stats, aes(date, max), color='black') +
  theme_bw() +
  ylab("SFS Avg Daily Streamflow (cfs)")

#seq dates starting with the beginning of water year
flow_sfs <- streamflow_data %>% filter(site_no == sfs) %>% mutate(wyF=factor(wy)) %>%
  mutate(plot_date=as.Date(paste0(2020, "-", mo, "-", day)))


ggplot() +
  geom_line(data=flow_sfs, aes(x = plot_date, y = Flow, group = wyF), lwd = 0.5, color='lightgrey')+
  ylab("SFS @ Krassel Streamflow (cfs)")+
  xlab("Day of Year")+
  scale_x_date(date_labels = "%b")+
  theme_bw()





flow<- streamflow_data %>% filter(Date >= '2011-09-15' & Date <= '2022-1-15')
jc<-flow[flow$site_no == johnson,]
ec<- flow[flow$site_no == efsf,]
sug<- flow[flow$site_no == sugar,]

#get all the periods of record the same to compute total flow efsf+sugar and then calc proportion of efsf
ggplot(flow, aes(Flow[site_no == johnson],Flow[site_no == efsf]))+
  geom_point()

plot(flow$Flow[flow$site_no == johnson],flow$Flow[flow$site_no == efsf])

plot(flow$Date[flow$site_no == johnson], flow$Flow[flow$site_no == johnson], type='l')
lines(flow$Date[flow$site_no == efsf], flow$Flow[flow$site_no == efsf])
plot(flow$Date[flow$site_no == sug], flow$Flow[flow$site_no == sug])

mod<-lm(flow$Flow[flow$site_no == efsf]~flow$Flow[flow$site_no == johnson])
summary(mod)

sugar_stats<-mo_stats(sugar)
ggplot(sugar_stats[[2]], aes(wy, min), color='r') + 
  geom_point() +
  theme_bw() +
  ylab("Sugar Creek Annual Minimum Streamflow (cfs)")

ggplot(sugar_stats[[1]], aes(mo, max$max), color='r') + 
  geom_point() +
  theme_bw() +
  ylab("Sugar Creek Monthyl Max Streamflow (cfs)")

ggplot(sugar_stats[[1]], aes(mo, avg), color='r') + 
  geom_point() +
  theme_bw() +
  ylab("Sugar Creek Monthyl Max Streamflow (cfs)")


plot(flow$Date[flow$site_no == efsf], flow$Flow[flow$site_no == efsf], type='l')
lines(flow$Date[flow$site_no == sugar], flow$Flow[flow$site_no == sugar], col='blue')



