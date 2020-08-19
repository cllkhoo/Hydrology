---------------------------------------------------EXTRACT AND TIDY CANADIAN HYDROMETRIC DATA------------------------------------------
  
#Set working directory -- this is a folder/place where all files from this project is saved
#Use getwd() to find filepath then enter it within the "" in line 5
work_directory <- "/Users/celynkhoo/R Projects/Hydrology" 
setwd(work_directory)
  
library(tidyhydat)
library(dplyr) #data manipulation package
library(ggplot2) #data visualization tool
library(lubridate) #aids with date and time in R
library(scales) #graphical scales for visualization
  
-------------------------------------------------------------GETTING HISTORICAL DATA---------------------------------------------------
    
download_hydat() #downloads HYDAT database, the Canadian National Water Data Archive, data here has been validated/corrected
NL_stns <- hy_stations(prov_terr_state_loc = "NL") #Finding station information and numbers for NL regional data
  
#Pulling data from the stations you want. Things you would change in the code following are the station numbers and data type
Raw_Data <- hy_stn_data_range() %>%
  filter(DATA_TYPE == "Q", STATION_NUMBER == "02ZH001") %>% 
  hy_daily_flows()

write.csv(Raw_Data, "PHR_HistoricalRawData.csv", row.names = FALSE) #Saves Raw_Data dataframe as a csv file. Substitute PHR with desired station name.

#Station information in list form including name, lat, long, drainage area. "Raw_Data" in the following code will call 
#the station specified in line 21-22
hy_stations(station_number = unique(Raw_Data$STATION_NUMBER)) %>%
  as.list()

##Tidying the dataframe for analyses##
Tidy_Data <- Raw_Data %>%
  mutate(year = year(Date), month = month(Date), day = day(Date)) %>% #adds columns of year, month and day
  mutate(Julian = yday(PipersHole_tidy$Date)) %>% #adds a column for Julian Date
  mutate(Season =
           ifelse(month %in% c(12,1,2), "Winter", 
           ifelse(month %in% c(3,4,5), "Spring",
           ifelse(month %in% c(6,7,8), "Summer",
           ifelse(month %in% c(9,10,11), "Fall", "Error"))))) #Adds a column for seasonal designation
#Here, seasons are defined by 'Boreal' hydrology with Winter starting in December to February,
#Spring starting in March to May, Summer from June to August and Fall from September to November.

write.csv(Tidy_Data, "PHR_HistoricalRawDataTidy.csv", row.names = FALSE) #Saves Tidy_Data dataframe as a csv file. Substitute PHR with desired station name.

----------------------------------------------------------GETTING REALTIME DATA--------------------------------------------------------

#Real-time data aren't been validated and may have some missing values  
realtime_dd(station_number = "02ZH001") #select specific realtime discharge station
realtime_plot(station_number = "02ZH001") #plots the most recent month 

------------------------------------------------MANIPULATING THE DATA FOR TREND ANALYSES----------------------------------------------
  
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



  
  
