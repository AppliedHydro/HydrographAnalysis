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
site_info<- whatNWISdata(sites= usgs_site, parameterCd = pCode, outputDataTypeCd ='uv') 
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

  #plot(monthly_q$mo, monthly_q$avg, type= 'l', col="blue")
  #lines(monthly_q$mo, monthly_q$avg, col="black")
  #lot(monthly_q$mo, monthly_q$min$min, type='l', col="red")
  p<- ggplot(monthly_q, aes(mo, min$min), color='r') + 
    geom_line() +
    theme_bw() 
  
  return(p)
}

johnson_stats<-mo_stats(johnson)
efsf_stats<-mo_stats(efsf)
