## Plot of individual average for each day of measurement

source("path_file_lilian.R")

Data <- read_xlsx(paste0(path.data, "Biologger full data.xlsx"))

##Find the correct date to split data in 2 seasons

#Select winter data (before temp increase)
#Full_Data_Win <- Full_Data %>%
# filter(
#   Date < "2025-03-14"
# )

#ggplot(
#  data = Full_Data_Win, 
# mapping = aes(x = `DateTime`, y = Temp),
#) +
# geom_point() +
#geom_smooth() 

#Select spring data (during temp increase)
#Full_Data_Spr <- Full_Data %>%
# filter(
#  Date > "2025-03-14"
#)

#ggplot(
# data = Full_Data_Spr, 
# mapping = aes(x = `DateTime`, y = Temp),
#) +
# geom_point() +
#geom_smooth() 




##UXL0528DAT is faulty (wrong temp and incomplete data) 
##UXL0557DAT is one of the loggers that was eaten
##Data after 13 may is not reliable (culling going on, fish disturbed)
##UXL0530, 532, 533 and 511DAT showed a drift in acceleration values (could be added back if Asgeir can correct the values)
##Filter out the data points with sampling frequency = 2spm : those with both skew = 0 and kurt = -2 or skew = 0 and kurt = 0

Full_Data_Clean <- Data %>%
  filter(
    Biologger != "UXL0528DAT",
    Biologger != "UXL0557DAT",
    Biologger != "UXL0530DAT",
    Biologger != "UXL0532DAT",
    Biologger != "UXL0533DAT",
    Biologger != "UXL0511DAT",
    !(Skew == 0 & Kurt == -2),
    !(Skew == 0 & Kurt == 0),
    AvgEA != MaxEA
  )

Full_Data_Clean %>%
  summarise(n_biologgers = n_distinct(Biologger))




##Add a month column to be able to sort by month
Full_Data_Clean <- Full_Data_Clean %>%
  mutate(
    Date = dmy(Date),
    Time = lubridate::hms(Time),
    Month = lubridate::month(Date, label=TRUE),
    ##Also add a season factor, based on equinox
    Season = as.factor(if_else(
      Date < "2025-03-21",
      "Winter",
      "Spring",
    )),
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
  ) %>%
  ## Create a DateTime column to plot all data over the measurement period
  mutate(
    Time=hms::hms(Time),
    DateTime = as.factor(paste0(Date, " ", Time))
  ) %>%
  mutate(
    DateTime = ymd_hms(DateTime),
  )
#do not change date and time to ymd() and hms() yet, otherwise the excel conversion converts them to numeric wrongly


Full_Data_Clean <- Full_Data_Clean %>%
  mutate(
    BurstScore = as.numeric(if_else(
      VAR < 190, ### Value estimated based on visual analysis of party popper plots and clustering data
      0,
      1,
    )),
    ##Add a SwimStyle column to be able to split data based on bursting
    Swimstyle = as.factor(if_else(
      VAR < 190,
      "Steady",
      "Dynamic",
    )),
    ## Add a Tempgroup column to be able to split data based on the temp threshold identified when looking at VAR/Temp
    Tempgroup = as.factor(if_else(
      Temp < 4.5,
      "LowTemp",
      "HighTemp"
    )),
    corr.AvgEA = as.numeric(AvgEA-MaxEA/60),
  ) %>%
  filter(
    Biologger != "UXL0471DAT", #weird results + infected fish
  )

str(Full_Data_Clean$BurstScore)
str(Full_Data_Clean$Swimstyle)
str(Full_Data_Clean$Tempgroup)
str(Full_Data_Clean$Time)

### Force the plots to show low-temp on the left and high-temp on the right
Full_Data_Clean$Tempgroup <- factor(Full_Data_Clean$Tempgroup, levels = c("LowTemp", "HighTemp"))
Full_Data_Clean$food <- factor(Full_Data_Clean$food, levels = c("high-food", "low-food"))


### test number and % of 0s
colSums(Full_Data_Clean[, "BurstScore"] == 0)                  #' Number of zeros
100 * colSums(Full_Data_Clean[, "BurstScore"] == 0) / nrow(Full_Data_Clean) #' % of zeros


#idea to linearize time of day effect : convert it to "distance from noon", so you get only one side of the bell curve ?

Full_Data_Clean <- Full_Data_Clean %>%
  group_by(Biologger) %>%
  mutate(
    Time = as.numeric(Time),
    TimeToNoon = as.numeric(abs(45000 - Time)), ### 45000 is 12:30, it is the time that performs better as center of the distribution
  )%>%
  mutate(
    Time = hms::hms(Time),
    ) %>%
  ungroup()

str(Full_Data_Clean$TimeToNoon)



#determine whether fedeers are ON or OFF based on date and feeders rhythm

start_date <- "2025-01-14"

Full_Data_Clean <- Full_Data_Clean %>% 
  mutate(
    start_date = ymd(start_date),
    days_since_start = as.numeric(difftime(Date, start_date, units = "days")),
    cycle_pos = days_since_start %% 3,
    feeders = ifelse(food == "high-food",
                     "ON",
                     ifelse(
                       cycle_pos == 1, "OFF",   # feeders OFF
                       "ON"               # feeders ON
                     )
    ),
    fasting_day = ifelse(
      cycle_pos == 1, "Fasting",   # fasting day for low-food group
      "Ad Libitum"               # feeding day for low-food group
    ),
    feeders = as.factor(feeders),
    fasting_day = as.factor(fasting_day),
    Date = ymd(Date)
  )



### Because not enough data in the burst data set to add Time as a covariate (some time groups are empty or 1), we will group by day/night instead

#Import suntable obtained from NOAA solar calculator (and formated in excel)
sun_table <- read_xlsx("C:/Users/03278487/OneDrive - Luonnonvarakeskus/PhD projects/Biolgger paper/NOAA_SunTimes.xlsx")
sun_table <- sun_table %>%
  select(Date, Sunrise, Sunset)%>%
  mutate(
    Date = ymd(Date),
  )

#Add the sunrise and sunset to the main dataframe
Full_Data_Clean <- Full_Data_Clean %>%
  left_join(sun_table, by = "Date")

#Convert times to a time format and rename columns
Full_Data_Clean <- Full_Data_Clean %>% 
  mutate(
    Sunrise = hm(Sunrise),
    Sunset = hm(Sunset),
    Daylength = as.numeric(Sunset-Sunrise)
  )%>%
  rename(
    NOAA_Sunrise = Sunrise,
    NOAA_Sunset = Sunset,
  )

#Create a Light factor based on surise and sunset times
Full_Data_Clean <- Full_Data_Clean %>% 
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins")
  )%>% 
  mutate(
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
    Light = as.factor(ifelse(Time < NOAA_Sunrise, 
                             "Night",
                             ifelse(Time < NOAA_Sunset,
                                    "Day",
                                    "Night"))
    ),
    Time = as.numeric(Time),
    am_pm = if_else( 
      Time > 43201,
      "PM",
      "AM"),
    am_pm = as.factor(am_pm),
    
  )%>% 
  mutate(
    Time = hms::hms(Time),
  )


str(Full_Data_Clean$NOAA_Sunrise)
str(Full_Data_Clean$NOAA_Sunset)
str(Full_Data_Clean$Light)
str(Full_Data_Clean$feeders)
str(Full_Data_Clean$DayLength)

Full_Data_Clean$Light <- factor(Full_Data_Clean$Light, levels = c("Night", "Day"))
Full_Data_Clean$Season <- factor(Full_Data_Clean$Season, levels = c("Winter", "Spring"))

Full_Data_Clean <- Full_Data_Clean %>% 
  mutate(
    Date = ymd(Date),
    start_date = ymd(start_date),
  )%>%
  mutate(
    MeasNB = floor(as.numeric(Date-start_date)/6)+1
  )%>%
  mutate(
    MeasNB = as.factor(MeasNB)
  )



### Convert covariates to factor with n levels
Full_Data <- Full_Data_Clean %>%
  mutate(
    greb1l = as.factor(greb1l),
  )%>%
  filter(
    Date < "2025-05-19"
  )

str(Full_Data$Biologger)
str(Full_Data$fam)
str(Full_Data$greb1l)
str(Full_Data$omy05)
str(Full_Data$tank)
str(Full_Data$Date)
str(Full_Data$Time)
str(Full_Data$DateTime)
str(Full_Data$BurstScore)
str(Full_Data$Swimstyle)
str(Full_Data$Season)
str(Full_Data$Light)
str(Full_Data$feeders)
str(Full_Data$DayLength)
str(Full_Data$NOAA_Sunrise)
str(Full_Data$NOAA_Sunset)
str(Full_Data$MeasNB)


IndivActData <- Full_Data%>%  
  group_by(MeasNB, Light,
    Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_corrAvgEA = mean(corr.AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    MaxEA = max(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    BurstProp = as.numeric(100*mean(BurstScore, na.rm = TRUE)),greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    Date = last(ymd(Date))
  )


culling <- "2025-05-19"
IndivActData <- IndivActData %>%
  mutate(
    culling=ymd(culling),
)


ggplot(IndivActData, aes(x = ymd(Date), y = mean_AvgEA, col=greb1l, group = Biologger)) +
  facet_wrap(~food)+
  geom_point()+
  geom_path()

ggplot(Full_Data, aes(x = ymd(Date), y = log(AvgEA), col=greb1l)) +
  facet_wrap(~food)+
  geom_smooth()


HourActData <- Full_Data%>%  
  group_by(Season, Time, greb1l, tank) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_corrAvgEA = mean(corr.AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    MaxEA = max(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    BurstProp = as.numeric(100*mean(BurstScore, na.rm = TRUE)),greb1l = first(greb1l),
    Date = last(ymd(Date))
  )

str(HourActData$Time)


ggplot(HourActData, aes(x = Time, y = mean_MaxEA)) +
  facet_wrap(~Season+greb1l)+
  coord_polar()+
  geom_col(alpha=.4)


ggplot(HourActData, aes(x = Time, y = mean_MaxEA)) +
  facet_wrap(~Season+greb1l+tank)+
  coord_polar()+
  geom_col(alpha=.5)
































SubDATA22 <- Full_Data%>% 
  mutate(
    Time = as.numeric(Time)
  )%>%
  filter(
    Time == 45000,
    tank == 22,
    Season == "Winter"
  )

SubDATA23 <- Full_Data%>% 
  mutate(
    Time = as.numeric(Time)
  )%>%
  filter(
    Time == 45000,
    tank == 23,
    Season == "Winter"
  )


mean(SubDATA22$AvgEA)
mean(SubDATA23$AvgEA)

SubDATA <- SubDATA%>%  
  group_by(Season, greb1l) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_corrAvgEA = mean(corr.AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    MaxEA = max(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    BurstProp = as.numeric(100*mean(BurstScore, na.rm = TRUE)),greb1l = first(greb1l),
    Date = last(ymd(Date))
    