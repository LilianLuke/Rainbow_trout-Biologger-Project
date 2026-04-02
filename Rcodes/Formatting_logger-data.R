## This script is for formatting data from the biologgers and add the body mass and length measurements from feb and may + heart mass from may
## It also calculates SGR and modified Fulton's condition factor

library(dplyr)
library(lubridate)
library(purrr)
library(tidyverse)
library(stringi)
library(data.table)
library(readxl)
library(vctrs)
library(writexl)
library(ggthemes)
library(mgcv)
library(gamm4)
library(lme4)
library(nlme)
library(nortest)
library(car)
library(plotrix)
library(carData)
library(ggplot2)
library(ggrepel)
library(MuMIn)
library(openxlsx)
library(lsmeans)
library(emmeans)
library(tools)
library(ggeffects)

#rm(list=ls(all=TRUE));gc()

## Set working directory (does not work otherwise?)
setwd(path.raw)

#list all individual logger files
Biologgers <- list.files(path = path.raw, pattern="\\DAT.CSV")

## WARNING : The CSV encoding is ISO-8859-1 (Latin-1), not UTF-8

##Merge all individual files in one data frame
merged_df <- map_dfr(Biologgers, function(fpath) {
  read_delim(fpath,
             delim   = ";",
             locale  = locale(encoding = "Latin1"),
             trim_ws = TRUE) %>%
    #add the biologger number associated with each measurement
    #(will be useful to match the activity to a fish PIT tag, genotype, food treatment, family and tank)
    mutate(
      Biologger = substring(
        file_path_sans_ext(basename(fpath)), 
        2)
    )
})


##clean up data frame
merged_df_clean <- merged_df %>%
  # drop unnecessary columns
  dplyr::select(-ACT, -`MinEA(mg)`) %>%
  #rename columns with complicated names
  rename(`Temp`=`Temp(°C)`, VAR = `VAR(Minute)`) %>%
  # move the “Biologger” column to position 1
  relocate(Biologger)


##read the file containing both the PIT tag and the Biologger number
lookup <- read_xlsx(paste0(path_meas, "Biologger fish sampling.xlsx"))

##Make sure both columns are plain character 
##(not necessarily important but the guy on stackoverflow said it is good to do it)
lookup <- lookup %>%
  mutate(
    Biologger = as.character(Biologger) %>% trimws(),
    `PIT tag` = tolower(`PIT tag`) %>% trimws()
  )

##merge both file, associating the right pit tag to the right biologger
  
merged_with_PIT <- merged_df_clean %>%
  left_join(lookup, by = "Biologger") %>%
  ##drop unnecessary columns
  dplyr::select(-`Biologger no.`, -`Sample ID`, -notes, -...7, -...8)


##import measurement files and merge to get weight and length for feb and may measurements
setwd(path_meas)

May_surements <- list.files(path = path_meas, pattern="^MeasurementTroutMay25.*")

lookupMay <- map_dfr(May_surements, function(fpath) {
  read_csv(fpath)
})

lookupFeb <- read_csv(paste0(path_meas, "MeasurementTroutFeb25.csv"))

lookupFeb <- lookupFeb %>%
  #change column names 
  mutate(
    WeightFeb = as.numeric(Weight),
    LengthFeb = as.numeric(Length),
    `PIT tag` = tolower(PIT) %>% trimws(),
    FebMeasDate = mdy(Date),
  ) %>%
  #keep only useful columns
  dplyr::select(FebMeasDate, WeightFeb, LengthFeb, `PIT tag`)
  


lookupMay <- lookupMay %>%
  #change column names 
  mutate(
    WeightMay = as.numeric(Weight),
    LengthMay = as.numeric(Length),
    `PIT tag` = tolower(PIT) %>% trimws(),
    `DateB`= dmy(`Record Date`),
  ) %>%
  #keep only useful columns
  dplyr::select(Date, DateB, WeightMay, LengthMay, `PIT tag`)


lookupMay <- lookupMay %>%
  #change column names 
  mutate(DateA = mdy(Date),) %>%
  mutate(MayMeasDate = coalesce(DateA, DateB))%>%
  dplyr::select(MayMeasDate, WeightMay, LengthMay, `PIT tag`)
    
    
#str(lookupMay$MeasDate)


##merge the measas.Date()##merge the measurement columns to DF based on PIT
merged_with_Meas <- merged_with_PIT %>%
  left_join(lookupFeb, by = "PIT tag") %>%
  left_join(lookupMay, by = "PIT tag") 
  



lookupGen <- read_xlsx(paste0(path_meas, "Lammi_Biologger_Fish_selection.xlsx"))

lookupGen <- lookupGen %>%
#change column names 
  mutate(
    greb1l = as.factor(greb1l) %>% trimws(),
    omy05 = as.factor(inv) %>% trimws(),
    fam = as.factor(Family) %>% trimws(),
    tank = as.numeric(Tank),
    #add a food column that asigns food treatment based on tank number 
    #(even=high-food, odd=low-food)
    food = as.factor(if_else(
      tank %% 2 == 1,
      "low-food", 
      "high-food"
    )),
    `PIT tag` = tolower(PIT) %>% trimws()
  ) %>%
  #keep only useful columns
  dplyr::select(greb1l, omy05, fam, tank, food, `PIT tag`)


## Add genotyping and food treatment data to creat a DF with all the data
Full_Data <- merged_with_Meas %>%
  left_join(lookupGen, by = "PIT tag") %>%
  #create new columns for growth and body condition factors
  mutate(
    #calculate specific growth rate (Crane et al. 2019)
    deltaT= as.numeric((MayMeasDate)-(FebMeasDate)),
    deltaW= as.numeric(log(WeightMay)-log(WeightFeb)),
    deltaL= as.numeric(LengthMay-LengthFeb),
    g = as.numeric((deltaW)/(deltaT)),
    SGRMass = as.numeric(100*expm1(g)),
    #Calculate growth rate for length (mm/day)
    growthlength= as.numeric(deltaL/deltaT),
    #based on coef found previously, calculate modified Fulton's condition factor (Aykanat et al. 2024)
    FultonFeb = as.numeric(100*WeightFeb/(LengthFeb)^3.453),
    FultonMay = as.numeric(100*WeightMay/(LengthMay)^3.311),
    DeltaFulton = as.numeric(FultonMay-FultonFeb),
    PercentDeltaFulton = as.numeric(100*(FultonMay-FultonFeb)/FultonFeb),
    `Heart weight (g)`= as.numeric(`Heart weight (g)`),
    HtoBratio = as.numeric(`Heart weight (g)`/WeightMay),
    AvgEA = as.numeric(`AvgEA(mg)`),
    MaxEA = as.numeric(`MaxEA(mg)`)
  ) 

#Get the coef of the weight/length correlation (Aykanat et al. 2024)
FultonFebCoef <- lm(log(WeightFeb)~log(LengthFeb), data=Full_Data)
summary(FultonFebCoef)

FultonMayCoef <- lm(log(WeightMay)~log(LengthMay), data=Full_Data)
summary(FultonMayCoef)

plot(log(WeightMay)~log(LengthMay), data=Full_Data)
 


## Make sure all factors are considered correctly
Full_Data <- Full_Data %>%
  ## Qualitative variable as factors
  mutate(
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger)
  ) %>%
  dplyr::select(
    -`AvgEA(mg)`, -`MaxEA(mg)`
  ) 

ggplot(
  data = Full_Data, 
  mapping = aes(x = WeightFeb, y = WeightMay),
) +
  geom_point(alpha=.5)+
  geom_smooth()


## Save the new dataframe to path.data
write_xlsx(Full_Data, paste0(path.data, "/Biologger full data.xlsx"))
write_csv(Full_Data, paste0(path.data, "/Biologger full data.csv"))



############################################################################################################################
############################################################################################################################
###########################                                                                       ##########################
########################### In the following script we exclude faulty biologgers from the dataset ##########################
###########################                                                                       ##########################
############################################################################################################################
############################################################################################################################




##UXL0528DAT is faulty (wrong temp and incomplete data) 
##UXL0557DAT is one of the loggers that was eaten
##Data after 13 may is not reliable (culling going on, fish disturbed)
##UXL0530, 532, 533 and 511DAT showed a drift in acceleration values (could be added back if Asgeir can correct the values)
##Filter out the data points with sampling frequency = 2spm : those with both skew = 0 and kurt = -2 or skew = 0 and kurt = 0

Full_Data_Clean <- Full_Data %>%
  filter(
    Biologger != "UXL0528DAT",
    Biologger != "UXL0557DAT",
    Biologger != "UXL0530DAT",
    Biologger != "UXL0532DAT",
    Biologger != "UXL0533DAT",
    Biologger != "UXL0511DAT",
    !(Skew == 0 & Kurt == -2),
    !(Skew == 0 & Kurt == 0),
  )

Full_Data_Clean %>%
  summarise(n_biologgers = n_distinct(Biologger))




##Add a month column to be able to sort by month
Full_Data_Clean <- Full_Data_Clean %>%
  mutate(
    Date = dmy(Date),
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
  filter(
    Date < "2025-05-13",
  )%>%
  ## Create a DateTime column to plot all data over the measurement period
  mutate(
    Date = as.character(Date),
    Time = as.character(Time),
  ) %>%
  mutate(
    DateTime = ymd(Date) + hms(Time)
  ) 
#do not change date and time to ymd() and hms() yet, otherwise the excel conversion converts them to numeric wrongly



## check if any NA messes up with the model
any(is.na(Full_Data))
any(is.na(Full_Data$greb1l))
any(is.na(Full_Data$MaxEA))

## make sure factors are read correctly
str(Full_Data_Clean$Biologger)
str(Full_Data_Clean$fam)
str(Full_Data_Clean$greb1l)
str(Full_Data_Clean$omy05)
str(Full_Data_Clean$tank)
str(Full_Data_Clean$Date)
str(Full_Data_Clean$Time)
str(Full_Data_Clean$DateTime)


Biologgerlist <- Full_Data_Clean %>% 
  distinct(Biologger) %>% 
  arrange(Biologger)



## Save the new dataframe to path.data
write_xlsx(Full_Data_Clean, paste0(path.data, "/Biologger Clean data.xlsx"))
write_csv(Full_Data_Clean, paste0(path.data, "/Biologger Clean data.csv"))












############################################################################################################################
############################################################################################################################
####################                                                                                    ####################
#################### In the following script we compute a set of new response and explanatory variables ####################
####################                                                                                    ####################
############################################################################################################################
############################################################################################################################



### Add a BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
Full_Data_Out <- Full_Data_Clean %>%
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

str(Full_Data_Out$BurstScore)
str(Full_Data_Out$Swimstyle)
str(Full_Data_Out$Tempgroup)
str(Full_Data_Out$Time)

Full_Data_Out <- Full_Data_Out %>%
  mutate(
    Time = hms(Time)
  )
    
    
### Force the plots to show low-temp on the left and high-temp on the right
Full_Data_Out$Tempgroup <- factor(Full_Data_Out$Tempgroup, levels = c("LowTemp", "HighTemp"))
Full_Data_Out$food <- factor(Full_Data_Out$food, levels = c("high-food", "low-food"))


### test number and % of 0s
colSums(Full_Data_Out[, "BurstScore"] == 0)                  #' Number of zeros
100 * colSums(Full_Data_Out[, "BurstScore"] == 0) / nrow(Full_Data_Out) #' % of zeros


### "linearize" time of day effect : convert it to "distance from noon", so you get only one side of the bell curve

Full_Data_Out <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    Time = as.numeric(Time),
    TimeToNoon = as.numeric(abs(45000 - Time)), ### 45000 is 12:30, it is the time that performs better as center of the distribution
  )

str(Full_Data_Out$Time)



### determine whether fedeers are ON or OFF based on date and feeders rhythm

start_date <- "2025-01-14"

Full_Data_Out <- Full_Data_Out %>% 
  mutate(
    start_date = ymd(start_date),
    days_since_start = as.numeric(difftime(Date, start_date, units = "days")),
    cycle_pos = days_since_start %% 3,
    feeders = ifelse(food == "high-food",
                     "ON",                #because feeders are always on in the adlib group
                     ifelse(
                       cycle_pos == 1, "OFF",   # feeders OFF (because the 3-days cycle starts on a fasting day)
                       "ON"               # feeders ON (for the other 2 days in the cycle)
                     )
    ),
    fasting_day = ifelse( food == "high-food",
                          "Ad Libitum", 
                          ifelse(cycle_pos == 1, 
                                 "Fasting",     # fasting day for low-food group
                                 "Ad Libitum")  # feeding days for low-food group
                          ),
    feeders = as.factor(feeders),
    fasting_day = as.factor(fasting_day),
  )



### Because not enough data in the burst data set to add Time as a covariate (some time groups are empty or 1), we will group by day/night instead

### Import suntable obtained from NOAA solar calculator (and formated in excel)
sun_table <- read_xlsx("C:/Users/03278487/OneDrive - Luonnonvarakeskus/PhD projects/Biolgger paper/NOAA_SunTimes.xlsx")
sun_table <- sun_table %>%
  select(Date, Sunrise, Sunset)%>%
  mutate(Date=as.character(Date))

str(sun_table)
#Add the sunrise and sunset to the main dataframe
Full_Data_Out <- Full_Data_Out %>%
  left_join(sun_table, by = "Date")

#Convert times to a time format and rename columns
Full_Data_Out <- Full_Data_Out %>% 
  mutate(
    Sunrise = hm(Sunrise),
    Sunset = hm(Sunset),
  )%>%
  rename(
    NOAA_Sunrise = Sunrise,
    NOAA_Sunset = Sunset,
  )

### Create a Light factor based on surise and sunset times
Full_Data_Out <- Full_Data_Out %>% 
  mutate(
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
    Time = hms::hms(Time),
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



### Calculate Daylength
Full_Data_Out <- Full_Data_Out %>% 
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins")
  )%>%
  mutate(
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
  )



str(Full_Data_Out$NOAA_Sunrise)
str(Full_Data_Out$NOAA_Sunset)
str(Full_Data_Out$Light)
str(Full_Data_Out$feeders)
str(Full_Data_Out$DayLength)

Full_Data_Out$Light <- factor(Full_Data_Out$Light, levels = c("Night", "Day"))
Full_Data_Out$Season <- factor(Full_Data_Out$Season, levels = c("Winter", "Spring"))



### Add a MeasNB variable to avoid having 2 dates for each measurement window
Full_Data_Out <- Full_Data_Out %>% 
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

write_xlsx(Full_Data_Out, paste0(path.data, "/Biologger final data.xlsx"))
write_csv(Full_Data_Out, paste0(path.data, "/Biologger final data.csv"))



                        