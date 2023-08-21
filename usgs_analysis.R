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

usgs_sites = c(johnson, efsf, sugar)#  put all sites in one vector

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

mo_stats<- function(site){
  q<- streamflow_data %>% filter(site_no == site)
  
  monthly_q<- q %>% group_by(mo) %>%
    summarize(avg = mean(Flow, na.rm=TRUE))

  monthly_q$max<- q %>% group_by(mo) %>%
    summarize(max = max(Flow, na.rm=TRUE)) %>% dplyr::select(max) 
  
  monthly_q$min<- q%>% group_by(mo) %>%
    summarize(min = min(Flow, na.rm=TRUE)) %>% dplyr::select(min)
  
  # need to seperate this better
  annual_flows <- q%>% group_by(wy) %>%
    summarize(min = min(Flow, na.rm=TRUE))
  
  quantiles <- quantile(q$Flow, probs =c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm=TRUE) %>% round(1)

  return(list(monthly_q, annual_flows, quantiles))
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

plot(sugar_stats[[1]])

plot(flow$Date[flow$site_no == efsf], flow$Flow[flow$site_no == efsf], type='l')
lines(flow$Date[flow$site_no == sugar], flow$Flow[flow$site_no == sugar], col='blue')

total<-merge(ec, sug, by ='Date')
total$q<- total$Flow.x+total$Flow.y
total$efp<- total$Flow.x/total$q
  
plot(total$efp, type='l')
curtailment =0.8
for (i in 1:length(total$q)){
  if (total$q[i] <= 25){
    total$div[i] <- total$q[i]*curtailment
   } else if (total$q[i] >= 25){
   total$div[i] <- 9.6 }
}

total$ef_remaining= total$Flow.x - total$div


