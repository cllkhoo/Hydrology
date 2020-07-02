---------------------------------------------------EXTRACT AND TIDY CANADIAN HYDROMETRIC DATA------------------------------------------

#Set working directory -- this is a folder/place where all files from this project is saved
  #Use getwd() to find filepath then enter it within the "" in line 5
work_directory <- "/Users/celynkhoo/R Projects/Hydrology"
setwd(work_directory)

library(tidyhydat)
library(dplyr) #data manipulation package
library(ggplot2) #data visualization tool
library(lubridate) #aids with date and time in R

-------------------------------------------------------------GETTING HISTORICAL DATA---------------------------------------------------
  
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

#Plotting historical data. You can change the station number and start date for the station you want. 
his_PipersHole <- hy_daily_flows(station_number = "02ZH001", start_date = "2010-01-01")
plot(his_PipersHole)

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

----------------------------------------------------------GETTING REALTIME DATA--------------------------------------------------------

#Real-time data has not been validated and may have some missing values  
  
realtime_dd(station_number = "02ZH001") #select specific realtime discharge station
realtime_plot(station_number = "02ZH001") #plots the most recent month 
  
------------------------------------------------MANIPULATING THE DATA FOR TREND ANALYSES----------------------------------------------

#Before we manipulate the data for trend analyses, we will need to change the extracted data with dates that were given in the form of YYYY-MM-DD  
PipersHole_tidy <- Pipers_Hole %>%
  mutate(year = year(Date), month = month(Date), day = day(Date)) %>% #adds columns of year, month and day
  mutate(Julian = yday(PipersHole_tidy$Date)) #adds a column for Julian Date
  
#GETTING THE 10 YEAR AVERAGE DISCHARGE. Question: Is there any trends in long term annual discharge?
PipersHole_decadal1 <- PipersHole_tidy %>% 
  filter(year > 2008 & year <2019) %>% #subsets the decade you want (most recent decade)
  group_by(Julian) %>% #groups dataframe by month and day so you will end up with values for each day in a calendar year
  summarise(mean_discharge = mean(Value)) #output values are the average of discharge for the day
PipersHole_decadal2 <- PipersHole_tidy %>% 
  filter(year > 1998 & year <2009) %>% #subsets the decade prior
  group_by(Julian) %>% 
  summarise(mean_discharge2 = mean(Value))
PipersHole_decadalcombined <- cbind(PipersHole_decadal1, PipersHole_decadal2) #Joins both dataframes together but I want it as row additions...
  
#Plotting the average daily discharge for the last 2 decades
PHR_ADD1_Plot <- PipersHole_decadal1 %>% 
ggplot(aes(x=Julian , y=mean_discharge)) +
  geom_line(colour = "black") +
  labs(title = "Decadal Discharge", subtitle =  "Piper's Hole River", y = "Average Daily Discharge (m^3/s)", x = "Julian Date") +
  theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1))
PHR_ADD2_Plot <- PipersHole_decadal2 %>%
  ggplot(aes(x=Julian , y=mean_discharge)) +
  geom_line(colour = "dimgrey", linetype = "dashed") +
  labs(title = "Decadal Discharge", subtitle =  "Piper's Hole River", y = "Average Daily Discharge (m^3/s)", x = "Julian Date") +
  theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1))
#Need to figure out how to combine the two graphs...

#EXTRACTING PEAKS FROM ANNUAL DISCHARGE 
#The extracted data has a date output in the form of YYYY-MM-DD. What we first need to do is spilt the date and group into year
#then find the maximum discharge from each year
PipersHole_MaxDischargeValue <- PipersHole_tidy %>%
  group_by(year) %>% #groups the dataframe by year column
  summarise(max(Value)) %>% #extracting the annual max. discharge
  rename(Max_Discharge = 'max(Value)') #renames the annual max. discharge column

#Plotting maximum annual discharge
Plot_PHR_MDV <- ggplot(PipersHole_MaxDischargeValue, aes(x = year, y = Max_Discharge)) +
  geom_point (colour = "black") +
  labs (title = "Piper's Hole Annual Maximum Discharge", x = "Year", y = "Maximum Discharge (m^3/s)") +
  theme(axis.text = element_text(colour = 'black', size=14), axis.title = element_text(colour = 'black', size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 1)) +
  geom_smooth(method = "lm", se = FALSE)

#Adding a linear regression statistics for the question: Has the magnitude of the annual maximum discharge changed over time?
lm_eq_PHR <- function(PipersHole_MaxDischargeValue){
   m_PHR <- lm(Max_Discharge ~ year, PipersHole_MaxDischargeValue);
  eq_PHR <- substitute((y) == a + b %.% (x)*","~~(r)^2~"="~r2, 
        list(a = format(unname(coef(m_PHR)[1]), digits = 2),
             b = format(unname(coef(m_PHR)[2]), digits = 2),
             r2 = format(summary(m_PHR)$r.squared, digits = 3)))
             as.character(as.expression(eq_PHR)); }
Plot_PHR_MDV_Trend <- Plot_PHR_MDV + geom_text(x =1980, y = 500, label = lm_eq_PHR(PipersHole_MaxDischargeValue), parse = TRUE)
#In the case of Piper's Hole, the trend line increases but because the R2 is not good, there is not a strong change in magnitude

#Question: Has the timing of maximum discharge changed over time?

#CENTER OF MASS TRENDS
#To find the center of mass (50% of annual discharge), we will first need to get the sum of daily discharge for the year
#then divide by 2 (to find the 50% value). We will then have to iteratively add discharge starting from Jan 1st until the 
#50% value is reached. The date where this falls is then the center of mass. 



  
  
