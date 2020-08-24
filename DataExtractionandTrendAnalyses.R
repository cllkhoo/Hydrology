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

#Above codes are uploaded by Dilanka Ahtukorala for Peak fall and spring melt discharge and timing

#Setting up the work directory
setwd("C:/Users/Dilanka/Dropbox/Thesis writting n papers/Publication/Excel files/Trend analysis/Merge codes")
merge_code <- "C:/Users/Dilanka/Dropbox/Thesis writting n papers/Publication/Excel files/Trend analysis/Merge codes"
setwd(merge_code)

install.packages("tidyhydat")
library(tidyhydat)
download_hydat()

install.packages("dplyr")
install.packages("lubridate")

library(dplyr) #data manipulation package
library(ggplot2) #data visualization tool
library(lubridate) #aids with date and time in R
library(scales) #graphical scales for visualization

search_stn_name("south brook")
rawdataSB <- hy_stations() %>%
  filter(HYD_STATUS == "ACTIVE") %>%
  filter(PROV_)
  hy_daily_flows(station_number = "02YL004")

PEI_stns <- hy_stations() %>%
    filter(HYD_STATUS == "ACTIVE") %>%
    filter(PROV_TERR_STATE_LOC == "NL") %>%
    pull_station_number() 

#SB1 refers to South Brook
SB1 <- hy_daily_flows(station_number = "02YL004")

#alternative way
SB1 <- hy_stn_data_range() %>%
  filter(DATA_TYPE == "Q", STATION_NUMBER == "02YL004")%>%
  hy_daily_flows()

#station information
hy_stations(station_number = unique(SB1$STATION_NUMBER)) %>%
  as.list()

#To assign the special dates for the hydrologic year (if not we can use prevous dates based on the boreal hydrology)
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-10", format = "%Y-%m-%d") # Winter 
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring
  SS <- as.Date("2012-6-1",  format = "%Y-%m-%d") # Summer
  FE <- as.Date("2012-9-10",  format = "%Y-%m-%d") # Fall
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring melt",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

Flowdata <- SB1 %>%
  mutate(year = year(Date), month = month(Date), day = day(Date)) %>% #adds columns of year, month and day
  mutate(Julian = yday(SB1$Date)) %>% #adds a column for Julian Date
  mutate(season = getSeason(SB1$Date))
Flowdata2 <- Flowdata %>% select(Date,Value, year,season)


#To calculate the maximum discharge by year and season
max <- Tidy_Data %>% group_by(season,year) %>% slice(which.max(Value))
#To select the necessary coloumns
max2 <- max %>% select(Date,Value, year,season)


#To convert value in m^3s-1 to m^3, and add a new coloum by multiplying Value coloumn
Watervolume <- max2 %>% mutate(Volume = Value * 86400) 
#To get runoff by normalizing to the catchment area
runoff <- Watervolume %>% mutate(runoff = (Volume * 10^9)/(7860*10^12))
#To get total water volume
TotalWV2 <- Watervolume %>% group_by(season,year) %>% summarise(Seasonal_volume= sum(Volume))
#To get the seasonal runoff 
TotalWV3 <- TotalWV2 %>% mutate(Seasonal_runoff= (Seasonal_volume* 10^9)/(7860*10^12))
#To get total annual volume
TotalAnnual <- Watervolume %>% group_by(year) %>% summarise(Total_volume = sum(Volume))
#To get the annual runoff 
TotalAnnual2 <- TotalAnnual %>% mutate(Annualrunoff= (Total_volume* 10^9)/(7860*10^12))


fall <- Flowdata2 %>%
  filter(season == "Fall")
fallR <- runoff  %>%
  filter(season == "Fall")

springmelt <- Flowdata2 %>%
  filter(season == "Spring melt")
springmeltR <- runoff %>%
  filter(season == "Spring melt")

fallmax <- max2 %>%
  filter(season == "Fall")

springmeltmax <- max2 %>%
  filter(season == "Spring melt")

wintermax <- max2 %>%
  filter(season == "Winter")

Totalfalldischarge <- TotalWV2%>%
  filter(season == "Fall")

Totalspringmeltdischarge <- TotalWV2%>%
  filter(season == "Spring melt")

Totalwinterdischarge <- TotalWV2%>%
  filter(season == "Winter")

#TO get seasonal runoff (mm) by year

Totalfallrunoff <- TotalWV3%>%
  filter(season == "Fall")

Totalspringmeltrunoff <- TotalWV3%>%
  filter(season == "Spring melt")

Totalwinterrunoff <- TotalWV3%>%
  filter(season == "Winter")

# 1.Ratios
#To get the ratio of fall seasonal voloumes to annual
#Ratiofall <- mutate(TotalAnnual$Total_volume_/Totalfalldischarge$Seasonal_volume_)
SeasonalAnualFall <- merge(TotalAnnual,Totalfalldischarge, by = "year", sort = TRUE, all.x = TRUE)
#SeasonalAnual$ratio <- SeasonalAnual%>% mutate(ratio = Total_volume_.x/Total_volume_.y)
SeasonalAnualFall <- SeasonalAnualFall %>% mutate(ratio = Seasonal_volume/Total_volume)

#For runoff
SeasonalAnualFallR <- merge(TotalAnnual2,Totalfallrunoff, by = "year", sort = TRUE, all.x = TRUE)
SeasonalAnualFallR <- SeasonalAnualFallR %>% mutate(ratio = Seasonal_runoff/Annualrunoff)


#To get the ratio of spring melt seasonal voloumes to annual
SeasonalAnualSpringmlet <- merge(TotalAnnual,Totalspringmeltdischarge, by = "year", sort = TRUE, all.x = TRUE)
SeasonalAnualSpringmlet <- SeasonalAnualSpringmlet %>% mutate(ratio = Seasonal_volume/Total_volume)

SeasonalAnualSpringmletR <- merge(TotalAnnual2,Totalspringmeltrunoff, by = "year", sort = TRUE, all.x = TRUE)
SeasonalAnualSpringmletR <- SeasonalAnualSpringmletR %>% mutate(ratio = Seasonal_runoff/Annualrunoff)

#To get the ratio of winter seasonal voloumes to annual

SeasonalAnualWinter <- merge(TotalAnnual,Totalwinterdischarge, by = "year", sort = TRUE, all.x = TRUE)
SeasonalAnualWinter <- SeasonalAnualWinter %>% mutate(ratio = Seasonal_volume/Total_volume)

#For runoff
SeasonalAnualwinterR <- merge(TotalAnnual2,Totalwinterrunoff, by = "year", sort = TRUE, all.x = TRUE)
SeasonalAnualwinterR <- SeasonalAnualwinterR  %>% mutate(ratio = Seasonal_runoff/Annualrunoff)

#Plotting figures
library(ggplot2)

#2. Seasonal plots
ylabplot = expression(Discharge~  m^3*~ s^-1)
ylabplotR = expression(Runoff~  (mm))

plotfall<-ggplot(fall, aes(x = Date, y = Value)) + geom_line(color = "#00AFBB", size = 0.7 )+ geom_point(color = "#00AFBB", size = 0.5)+
  #geom_line(aes(color = "#00AFBB"))+ 
  facet_wrap(vars(year), scales = "free" )+ ggtitle("Fall discharge for South Brook")+ labs(y=ylabplot, x= "Date" , face = "bold")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+ylim(0,20)

plotsm<-ggplot(springmelt, aes(x = Date, y = Value)) + geom_line(color = "#00AFBB", size = 0.7)+ geom_point(color = "#00AFBB", size = 0.5)+
  #geom_line(aes(color = "#00AFBB"))+
  facet_wrap(vars(year), scales = "free" )+ggtitle("Spring melt discharge for South Brook")+ labs(y=ylabplot, x= "Date" , face = "bold")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+ylim(0,35)

#runoff plots
plotfallR<-ggplot(fallR, aes(x = Date, y = runoff)) + geom_line(color = "#00AFBB", size = 0.7 )+ geom_point(color = "#00AFBB", size = 0.5)+
  #geom_line(aes(color = "#00AFBB"))+ 
  facet_wrap(vars(year), scales = "free" )+ ggtitle("Fall runoff(mm) for Humber River")+ labs(y=ylabplotR, x= "Date" , face = "bold")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+ylim(0,0.25)

plotspringmeltR<-ggplot(springmeltR, aes(x = Date, y = runoff)) + geom_line(color = "#00AFBB", size = 0.7 )+ geom_point(color = "#00AFBB", size = 0.5)+
  #geom_line(aes(color = "#00AFBB"))+ 
  facet_wrap(vars(year), scales = "free" )+ ggtitle("Sprin gmelt runoff(mm) for Humber River")+ labs(y=ylabplotR, x= "Date" , face = "bold")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+ylim(0,0.4)

ylab = expression(Peak~ discharge~  m^3*~ s^-1)
ylab2 = expression(Total~ discharge~  m^3*~ s^-1)

install.packages("ggpmisc")
library(ggpmisc)

#3. Seasonal peak discharge values vs time
#Peak discharge vs time
trendfallpeak <-ggplot(fallmax, aes(x = year, y = Value)) + geom_point(color = "#0072B2", size = 2)+geom_smooth(method = lm, se=FALSE)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y=ylab, x= "Year" , face = "bold",title = "Peak fall vs time") + scale_y_continuous(limits= c(0,40))+
  theme_bw()
FallPeak.lm = lm(year ~ Value, data=fallmax)
summary(FallPeak.lm) 
SpearmanFall <- cor.test(fallmax$year, fallmax$Value,  method = "spearman")
SpearmanFall

trendspringpeak <-ggplot(springmeltmax, aes(x = year, y = Value)) + geom_point(color = "#0072B2", size = 2)+geom_smooth(method = lm, se=FALSE)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y=ylab, x= "Year" , face = "bold",title = "Peak springmelt vs time") + scale_y_continuous(limits= c(0,40))+
  theme_bw()
#TO get statistics
SpringmeltPeak.lm = lm(year ~ Value, data=springmeltmax)
summary(SpringmeltPeak.lm) 
SpringmeltPeak.lm$coefficients
print(SpringmeltPeak.lm)

Spearmanspringmelt <- cor.test(springmeltmax$year, springmeltmax$Value,  method = "spearman")
Spearmanspringmelt


trendwinterpeak <-ggplot(wintermax, aes(x = year, y = Value)) + geom_point(color = "#0072B2", size = 2)+geom_smooth(method = lm, se=FALSE)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y=ylab, x= "Year" , face = "bold",title = "Peak winter vs time")+theme_bw()

#TO get statistics
WinterPeak.lm = lm(year ~ Value, data=wintermax)
summary(WinterPeak.lm) 
print(WinterPeak.lm)
WinterPeak.lm$coefficients
Spearmanwinter <- cor.test(wintermax$year, wintermax$Value,  method = "spearman")
Spearmanwinter

#4. Total seasonal water volumes
trendtotalfall <- ggplot(Totalfalldischarge, aes(x = year, y = Total_discharge)) + geom_bar( fill = "#0072B2",stat = "identity")+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y=ylab2, x= "Date" , face = "bold", title="Total fall discharge") 
trendtotalspringmelt <- ggplot(Totalspringmeltdischarge, aes(x = year, y = Total_discharge)) + geom_bar( fill = "#0072B2",stat = "identity")+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y=ylab2, x= "Date" , face = "bold",title="Total springmlet discharge") 
trendtotalwinter <- ggplot(Totalwinterdischarge, aes(x = year, y = Total_discharge)) + geom_bar( fill = "#0072B2",stat = "identity")+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y=ylab2, x= "Date" , face = "bold",title="Total winter discharge") 


fallmax$Month <- format(as.Date(fallmax$Date), "%m-%d")
springmeltmax$Month <- format(as.Date(springmeltmax$Date), "%m-%d")


#5. Trend and slopes for seasonal Peak dates and year

trendfalltiming <-ggplot(fallmax, aes(x = year, y = Month)) + geom_point(color = "#0072B2", size = 2)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y= "Peak discharge date", x= "Date" , face = "bold",title = "Peak fall Date vs time") +
  theme_bw()

trendspringmelttiming <-ggplot(springmeltmax, aes(x = year, y = Month)) + geom_point(color = "#0072B2", size = 2)+ 
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y= "Peak discharge date", x= "Date" , face = "bold",title = "Peak springmelt Date vs time")+
  theme_bw()


# create fall and springmelt begin date
Startdatefall <- read_excel("Fall_begin_date.xlsx", sheet = "1") 
Startdatefall[['Date']] <- as.Date.POSIXct(Startdatefall[['Date']], format='%m/%d/%y')
Startdatefall$Day <-format(as.Date(Startdatefall$Date), "%m-%d")


Startdatespringmelt <- read_excel("Springmlet_begin_date.xlsx", sheet = "1") 
Startdatespringmelt[['Date']] <- as.Date.POSIXct(Startdatespringmelt[['Date']], format='%m/%d/%y')
Startdatespringmelt$Day <-format(as.Date(Startdatespringmelt$Date), "%m-%d")


#To plot start day with year
Startdatefallplot <-ggplot(Startdatefall, aes(x = Year, y = Day)) + geom_point(color = "#0072B2", size = 2)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y= "Start date", x= "Date" , face = "bold",title = "Start date fall") +
  theme_bw()

Startdatespringmeltplot <-ggplot(Startdatespringmelt, aes(x = Year, y = Day)) + geom_point(color = "#0072B2", size = 2)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ labs(y= "Start date", x= "Date" , face = "bold",title = "Start date springmelt") +
  theme_bw()
#To plot seasonal and annual total water volume ratios
SeasonalAnualFallplot <-ggplot(SeasonalAnualFall, aes(x = year, y = ratio)) + geom_point(color = "#0072B2", size = 2)+geom_smooth(method = lm, se=FALSE)+
  scale_x_continuous(breaks=seq(1983, 2019, 5) )+ scale_y_continuous(limits= c(0.0,0.4)) +labs(y= "Ratio", x= "Year" , face = "bold",title = "Fall seasonal precip/Annual precip") + 
  theme_bw()
SeasonalAnualSpringmletplot <-ggplot(SeasonalAnualSpringmlet, aes(x = year, y = ratio)) + geom_point(color = "#0072B2", size = 2)+geom_smooth(method = lm, se=FALSE)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ scale_y_continuous(limits= c(0.0,0.6))+ labs(y= "Ratio", x= "Year" , face = "bold",title = "Springmelt seasonal precip/Annual precip") +
  theme_bw()
#To plot seasonal and annual total runoff ratios
SeasonalAnualFallRplot <-ggplot(SeasonalAnualFallR, aes(x = year, y = ratio)) + geom_point(color = "#0072B2", size = 2)+geom_smooth(method = lm, se=FALSE)+
  scale_x_continuous(breaks=seq(1983, 2019, 5) )+ scale_y_continuous(limits= c(0.0,0.4)) +labs(y= "Ratio", x= "Year" , face = "bold",title = "Fall seasonal runoff/Annual runoff") + 
  theme_bw()
SeasonalAnualSpringmletRplot <-ggplot(SeasonalAnualSpringmletR, aes(x = year, y = ratio)) + geom_point(color = "#0072B2", size = 2)+geom_smooth(method = lm, se=FALSE)+
  scale_x_continuous(breaks=seq(1983, 2019, 5))+ scale_y_continuous(limits= c(0.0,0.6))+ labs(y= "Ratio", x= "Year" , face = "bold",title = "Springmelt seasonal runoff/Annual runoff") +
  theme_bw()



  
  
