-------------------------------------EXTRACT AND TIDY CANADIAN HYDROMETRIC DATA------------------------------------------

#Set working directory -- this is a folder/place where all files from this project is saved
  #Use getwd() to find filepath then enter it within the "" in line 5
work_directory <- "/Users/celynkhoo/R Projects/Hydrology"
setwd(work_directory)

library(tidyhydat)
library(dplyr) #data manipulation package
library(ggplot2) #data visualization tool
library(lubridate) #aids with date and time in R

----------------------------------------------GETTING HISTORICAL DATA---------------------------------------------------
  
download_hydat() #downloads HYDAT database, the Canadian National Water Data Archive, data here has been validated/corrected
NL_stns <- hy_stations(prov_terr_state_loc = "NL") #Finding station information and numbers for NL regional data

#Pulling data from the stations you want. Things you would change in the code following are the station numbers and data type
Pipers_Hole <- hy_stn_data_range() %>%
  filter(DATA_TYPE == "Q", STATION_NUMBER == "02ZH001") %>%
  hy_daily_flows()

Come_by_Chance <- hy_stn_data_range() %>%
  filter(DATA_TYPE == "Q", STATION_NUMBER == "02ZH002") %>%
  hy_daily_flows()

Ugjoktok <- hy_stn_data_range() %>%
  filter(DATA_TYPE == "Q", STATION_NUMBER == "03NF001") %>%
  hy_daily_flows()

#Station information in list form including name, lat, long, drainage area
hy_stations(station_number = unique(Pipers_Hole$STATION_NUMBER)) %>%
  as.list()
hy_stations(station_number = unique(Come_by_Chance$STATION_NUMBER)) %>%
  as.list()
hy_stations(station_number = unique(Ugjoktok$STATION_NUMBER)) %>%
  as.list()

#Plotting the time series for the entire record with a snoother added, for other stations: replace 'Pipers_Hole' with new station
#Picking a time frame for this plot will be better
Pipers_Hole %>%
  ggplot(aes(x=Date, y=Value)) +
  geom_line() + 
  geom_point() +
  geom_smooth() +
  labs(title = "Piper's Hole River", subtitle = "Station Number = 02ZH001", y = "Discharge (m^3/s)") +
  theme_minimal()

#Normalizing multiple stations by drainage area
stns <- c("02ZH001", "02ZH002", "03NF001")
runoff_data <- hy_daily_flows(station_number = stns, start_date = "2018-01-01", end_date = "2018-12-31") %>%
  left_join(hy_stations(station_number = stns) %>%
              select(STATION_NUMBER, STATION_NAME, DRAINAGE_AREA_GROSS), by = "STATION_NUMBER") %>%
  mutate(runoff = Value / DRAINAGE_AREA_GROSS * 86400 / 1e6 *1e3)

ggplot(runoff_data) +
  geom_line(aes(x=Date, y=runoff, colour = STATION_NAME)) +
  labs(title = "Normalized Discharge", y="Mean daily runoff (mm/day)", subtitle = "Data Source: Canadian National Water Data Archive") + scale_fill_gradient2() +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom", legend.title = element_blank(), legend.direction = "vertical") 
#Save as PNG 600 x 500

-------------------------------------------------GETTING REALTIME DATA--------------------------------------------------------

#Real-time data has not been validated and may have some missing values  
  
realtime_dd(station_number = "02ZH001") #select specific realtime discharge station
realtime_plot(station_number = "02ZH001") #plots the most recent month 
  
