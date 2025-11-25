## This script is for analysing the raw activity data based on a 2 types of swimming model (steady vs dynamic)

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
library(DHARMa)
library(glmmTMB)
library(performance)
library(flexmix)
library(mclust)
 
source("path_file_lilian.R")
  
Full_Data_Clean <- read_xlsx(paste0(path.data, "Biologger Clean data.xlsx"))


### Convert covariates to factor with n levels
Full_Data_Out <- Full_Data_Clean %>%
  mutate(
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
  ) %>%
  ## Convert Date and Time back to a format that can be plotted correctly
  mutate(
    Date = ymd(Date),
    Time = hms(Time),
  )%>%
  filter (
    !AvgEA == MaxEA, ### Removes all the data point with 2spm (we only use those with 60spm)
  )



### make sure all covariates are considered correctly
str(Full_Data_Out$Biologger)
str(Full_Data_Out$fam)
str(Full_Data_Out$food)
str(Full_Data_Out$greb1l)
str(Full_Data_Out$omy05)
str(Full_Data_Out$tank)
str(Full_Data_Out$Date)
str(Full_Data_Out$Time)
str(Full_Data_Out$DateTime)



### Create a log column for each ACT parameter to be able to run GAMMS on it
Full_Data_Out <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    LogAvgEA = as.numeric(log10(`AvgEA`+0.001)),
    LogMaxEA = as.numeric(log10(`MaxEA`+0.001)),
    LogVAR = as.numeric(log10(VAR+0.76)),
  )

### Add a BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
Full_Data_Out <- Full_Data_Out %>%
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
    corr.VAR = as.numeric(VAR-(MaxEA-(MaxEA/60))^2/60),
    th.VAR = as.numeric(((MaxEA-(10))^2+59*(0-(10))^2)/60),
  ) %>%
  filter(
    Biologger != "UXL0471DAT",
  )

str(Full_Data_Out$BurstScore)
str(Full_Data_Out$Swimstyle)
str(Full_Data_Out$Tempgroup)

### Force the plots to show low-temp on the left and high-temp on the right
Full_Data_Out$Tempgroup <- factor(Full_Data_Out$Tempgroup, levels = c("LowTemp", "HighTemp"))
Full_Data_Out$food <- factor(Full_Data_Out$food, levels = c("high-food", "low-food"))


### test number and % of 0s
colSums(Full_Data_Out[, "BurstScore"] == 0)                  #' Number of zeros
100 * colSums(Full_Data_Out[, "BurstScore"] == 0) / nrow(Full_Data_Out) #' % of zeros


#idea to linearize time of day effect : convert it to "distance from noon", so you get only one side of the bell curve ?

Full_Data_Out <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    Time = as.numeric(Time),
    TimeToNoon = as.numeric(abs(45000 - Time)), ### 45000 is 12:30, it is the time that performs better as center of the distribution
  )

str(Full_Data_Out$TimeToNoon)



#determine whether fedeers are ON or OFF based on date and feeders rhythm

start_date <- "2025-01-14"

Full_Data_Out <- Full_Data_Out %>% 
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
  )



### Because not enough data in the burst data set to add Time as a covariate (some time groups are empty or 1), we will group by day/night instead

#Import suntable obtained from NOAA solar calculator (and formated in excel)
sun_table <- read_xlsx("C:/Users/03278487/OneDrive - Luonnonvarakeskus/PhD projects/Biolgger paper/NOAA_SunTimes.xlsx")
sun_table <- sun_table %>%
  select(Date, Sunrise, Sunset)

#Add the sunrise and sunset to the main dataframe
Full_Data_Out <- Full_Data_Out %>%
  left_join(sun_table, by = "Date")

#Convert times to a time format and rename columns
Full_Data_Out <- Full_Data_Out %>% 
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



#Calculate Daylength?
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
  
#write_xlsx(Full_Data_Out, paste0(path.data, "/Biologger final data.xlsx"))
#write_csv(Full_Data_Out, paste0(path.data, "/Biologger final data.csv"))

### First run a glmm on the whole data

F1 <- glmmTMB(log(AvgEA) ~ Light*Tempgroup*food*greb1l+ (1 | Biologger) + (1| tank) + (1|fam), ## <= Tank and fam as random explain too little variance, drop them
              family = tweedie(link="log"), 
              data = Full_Data_Out)
performance(F1)
#=> No cond. R" values because fam and tank variances are too low
summary(F1) 


 
F1.1 <- glmmTMB(log(AvgEA) ~ Light*Tempgroup*food*greb1l+fam + (1 | Biologger), ## <= Best model for full data
                family = tweedie(link="log"), 
                data = Full_Data_Out)

# R^2
performance(F1.1) # gives KPIs, but hard to decide what is okay or not...
## marg. R2 values are not great... most of the variability is ICC (= fish-specific baseline)
summary(F1.1) 

### Then, analyse day and night separately
Full_Data_Day <- Full_Data_Out %>% 
  filter(
    Light == "Day"
    )

F2 <- glmmTMB(log(corr.AvgEA+1) ~ Tempgroup*food*greb1l+ fam + (1 | Biologger), ## <= Best model ?
              family = tweedie(link="log"), 
              data = Full_Data_Day)
# R^2
performance(F2) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... most of the variability is ICC
summary(F2)



Full_Data_Night <- Full_Data_Out %>% 
  filter(
    Light == "Night"
  )

F3 <- glmmTMB(log(corr.AvgEA+1) ~ Tempgroup*food*greb1l + fam + (1 | Biologger), ## <= Best model ?
              family = tweedie(link="log"), 
              data = Full_Data_Night)
# R^2
performance(F3) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... most of the variability is ICC
summary(F3)



EMM1 <- emmeans(F1, ~greb1l:food:Light, data=Full_Data_Out) 
EMM_resp1 <- regrid(EMM1)
pairs(EMM_resp1)


F4 <- glmmTMB(log(MaxEA) ~ Light*Tempgroup*food*greb1l+ fam + (1 | Biologger), ## <= Best model ?
              family = tweedie(link="log"), 
              data = Full_Data_Out)

# R^2
performance(F4) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... most of the variability is ICC
summary(F4)



EMM2 <- emmeans(F2, ~greb1l:food:Light, data=Full_Data_Out) 
EMM_resp2 <- regrid(EMM2)
pairs(EMM_resp2, adjust="none")

0
# Get scaled quantile residuals.
F1Bernqr <- simulateResiduals(fittedModel = F1.1, plot = FALSE)

#' Plot the scaled quantile residuals versus each covariate.
#' Tempgroup
plotResiduals(F1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out$Tempgroup, 
              xlab = "Tempgroup") 

#' food
plotResiduals(F1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out$food, 
              xlab = "food") 


#' greb1l
plotResiduals(F1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out$greb1l, 
              xlab = "greb1l") 


#' Light
plotResiduals(F1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out$Light, 
              xlab = "Light") 



ggplot(Full_Data_Out, aes(x = DateTime, y = AvgEA)) +
  geom_point(alpha=.2) +
  labs(title = "Average Acceleration accross the measurement period", x="", y = "Average acceleration (mg)") +
  theme_minimal()+
  geom_smooth()



ggplot(Full_Data_Out, aes(x = DateTime, y = MaxEA)) +
  geom_point(alpha=.2) +
  labs(title = "Maximum Acceleration accross the measurement period", x="", y = "Maximum acceleration (mg)") +
  theme_minimal()+
  geom_smooth()



ggplot(Full_Data_Out, aes(x = food, y = AvgEA)) +
  facet_wrap(~Light)+
  geom_boxplot(aes(fill = greb1l)) +
  theme_minimal()


ggplot(Full_Data_Out, aes(x = food, y = MaxEA)) +
  facet_wrap(~Light+Tempgroup)+
  geom_boxplot(aes(fill = greb1l)) +
  theme_minimal()

ggplot(Full_Data_Out, aes(x = MeasNB, y = log(VAR))) +
  geom_boxplot()+
  theme_minimal()


### The analysis of ACT parameters on full data makes no sense

### Run a 2 steps analysis (derived from 0-altered models)

### First step : binomial model, what decides if a fish is bursting or not ?

F3 <- glmmTMB(BurstScore ~ TimeToNoon*food*Tempgroup + (1 |Biologger )+ (1 |tank )+ (1 |fam ),  ## <= Best model
              family = "binomial", 
              data = Full_Data_Out)

# R^2
performance(F3) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... but I would say OK for this situation
summary(F3)


## Plot the results

Full_Data_grouped <- Full_Data_Out%>%  
  group_by(food, Light, Tempgroup, Biologger) %>%
  summarise(
    BurstProp = as.numeric(100*mean(BurstScore, na.rm = TRUE)),
    mean_AvgEA = as.numeric(mean(AvgEA, na.rm = TRUE)),
    mean_MaxEA = as.numeric(mean(MaxEA, na.rm = TRUE)),
    mean_VAR = as.numeric(mean(VAR, na.rm = TRUE)),
    mean_cAvgEA = as.numeric(mean(corr.AvgEA, na.rm = TRUE)),
    mean_cVAR = as.numeric(mean(corr.VAR, na.rm = TRUE)),
    Month = first(Month),
    Biologger = first(Biologger),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    Light = first(Light),
    Season = first(Season),
    LengthFeb = first(LengthFeb),
    WeightFeb = first(WeightFeb),
    Tempgroup = first(Tempgroup),
  )

ggplot(Full_Data_grouped, aes(x = Light, y = BurstProp)) +
  facet_wrap(~Tempgroup)+
  geom_boxplot(aes(fill = food)) +
  labs(title = "Bursting proportion per group", x="", y = "Proportion of active swimming measurement") +
  scale_fill_manual(values = c("darkgreen", "palegreen2"))


### Create one df for each season (not the best approach here)
#Full_Data_Spring <- Full_Data_Out %>%
#  filter(
#    Season == "Spring"
#    )
#
#Full_Data_Winter <- Full_Data_Out %>%
#  filter(
#    Season == "Winter"
#  )


### Step 2: quantitative analysis of active and steady swimming separately
### Create one df for each swimstyle

DynamicData <- Full_Data_Out%>%
  filter(
    Swimstyle == "Dynamic",
  )



D1 <- glmmTMB(corr.AvgEA ~ Light*Tempgroup*food*greb1l + (1 |tank) + (1|fam) + (1 |Biologger), # <= Best model, no effect of greb1l
              family = tweedie(link="log"),
              data = DynamicData)

# R^2
performance(D1) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... but I would say OK for this situation
summary(D1)

ggplot(DynamicData, aes(x = food, y = corr.AvgEA)) +
  facet_wrap(~Light+Tempgroup)+
  geom_boxplot(aes(fill = greb1l)) +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue3", "darkred"))

EMMD1 <- emmeans(D1, ~Light:food:Tempgroup:greb1l, data=DynamicData) 
EMM_respD1 <- regrid(EMMD1)
pairs(EMM_respD1, adjust="none")


DynamicDayta <- DynamicData%>%
  filter(
    Light == "Day",
  )

D1bis <- glmmTMB(scale(AvgEA)+10 ~ Tempgroup*food*greb1l + (1 |Biologger)+ (1 |tank), # <= Best model, no effect of greb1l
              family = tweedie(link="log"),
              data = DynamicDayta)
# R^2
performance(D1bis) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... but I would say OK for this situation
summary(D1bis)

ggplot(DynamicData, aes(x = food, y = corr.AvgEA)) +
  facet_wrap(~Light+Tempgroup)+
  geom_boxplot(aes(fill = greb1l)) +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue3", "darkred"))


# Get scaled quantile residuals.
D1bisBernqr <- simulateResiduals(fittedModel = D1bis, plot = FALSE)

#' Plot the scaled quantile residuals versus each covariate.
#' Tempgroup
plotResiduals(D1bisBernqr, 
              quantreg = FALSE,
              form = DynamicDayta$Tempgroup, 
              xlab = "Tempgroup") 

#' food
plotResiduals(D1bisBernqr, 
              quantreg = FALSE,
              form = DynamicDayta$food, 
              xlab = "food") 


#' greb1l
plotResiduals(D1bisBernqr, 
              quantreg = FALSE,
              form = DynamicDayta$greb1l, 
              xlab = "greb1l") 



D2 <- glmmTMB(MaxEA ~ Light+(Tempgroup+greb1l)*food+ (1|LengthFeb) + (1|tank) + (1|fam) + (1 |Biologger),
          family = tweedie(link = "log"),
          data = DynamicData)

# R^2
performance(D2) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... but I would say OK for this situation
summary(D2)

ggplot(DynamicData, aes(x = greb1l, y = MaxEA)) +
  geom_boxplot(aes(fill = food)) +
  theme_minimal() +
  scale_fill_manual(values = c("darkgreen", "palegreen2"))

ggplot(DynamicData, aes(x = Tempgroup, y = MaxEA)) +
  geom_boxplot(aes(fill = food)) +
  theme_minimal() +
  scale_fill_manual(values = c("darkgreen", "palegreen2"))



EMMD2 <- emmeans(D2, ~greb1l:food, data=Full_Data_Out) 
EMM_respD2 <- regrid(EMMD2)
pairs(EMM_respD2)

EMMD2 <- emmeans(D2, ~food:Tempgroup, data=Full_Data_Out) 
EMM_respD2 <- regrid(EMMD2)
pairs(EMM_respD2)


ggplot(Full_Data_Out, aes(x = MaxEA, y = corr.AvgEA, col=feeders)) +
  facet_wrap(~food+Light)+
  geom_point(alpha=0.2) +
  theme_minimal()

ggplot(DynamicData, aes(x = Light, y = corr.AvgEA, fill=fasting_day)) +
  facet_wrap(~food+am_pm)+
  geom_boxplot()+
  theme_minimal()

ggplot(Full_Data_Out, aes(x = feeders, y = corr.AvgEA, col=greb1l)) +
  facet_wrap(~food+Light)+
  stat_summary(fun.data = "mean_sdl") +
  theme_minimal()


D3 <- glmmTMB(MaxEA ~ Light+Tempgroup*greb1l*food + (1|LengthFeb)+ (1|tank) + (1|fam) + (1 |Biologger),
              family = tweedie(link = "log"),
              data = DynamicData)

# R^2
performance(D3) # gives KPIs, but hard to decide what is okay or not...
## AIC is lesser than using food
summary(D3)


sapply(DynamicData[c("fam","Biologger","tank")], function(x) length(unique(x)))
table(DynamicData$fam)     # voir si certains niveaux n'ont qu'1 obs
table(DynamicData$Biologger)
table(DynamicData$tank)

# tables croisées rapides
table(DynamicData$Biologger, DynamicData$fam)[1:129, 1:3]   # aperçu
table(DynamicData$Biologger, DynamicData$tank)[1:129, 1:6] 




colSums(DynamicData[, "greb1l"] == "Allele 1")   
colSums(DynamicData[, "greb1l"] == "Allele 2")  

100 * colSums(DynamicData[, "Light"] == "Day") / nrow(DynamicData) #' % of zeros

100 * colSums(SteadyData[, "Light"] == "Day") / nrow(SteadyData) #' % of zeros

100 * colSums(Full_Data_Out_Test[, "Light"] == "Day") / nrow(Full_Data_Out_Test) #' % of zeros




DayData <- Full_Data_Out_Test%>%
  filter(
    Light == "Day"
  )

colSums(DayData[, "Swimstyle"] == "Day")   
colSums(DayData[, "Swimstyle"] == "Night")  

100 * colSums(DayData[, "Swimstyle"] == "Dynamic") / nrow(DayData) #' % of zeros


NightData <- Full_Data_Out_Test%>%
  filter(
    Light == "Night",
  )
100 * colSums(NightData[, "Swimstyle"] == "Dynamic") / nrow(NightData) #' % of zeros





ggplot(DynamicData, aes(x = food, y = MaxEA)) +
  facet_wrap(~Tempgroup)+
  geom_boxplot(aes(fill = greb1l)) +
  labs(title = "WTF is going on?", x="", y = "Maximum Acceleration") +
  theme_minimal()

ggplot(DynamicData, aes(x = Time, y = AvgEA)) +
  facet_wrap(~food+Month)+
  geom_point(alpha=0.2) +
  labs(title = "WTF is going on?", x="", y = "Average Acceleration") +
  theme_minimal()+
  geom_smooth()




SteadyData <- Full_Data_Out%>%
  filter(
    Swimstyle == "Steady"
  )


S1 <- glmmTMB(AvgEA ~ Light + (1 |Biologger) ,
              family = tweedie(link="log"),
              data = SteadyData)

# R^2
performance(S1) # gives KPIs, but hard to decide what is okay or not...
## R2 values are not great... but I would say OK for this situation
summary(S1)
#### 60% of changes in Steady AvgEA is caused by Biologger

ggplot(SteadyData, aes(x = Light, y = AvgEA)) +
  geom_boxplot() +
  labs(title = "WTF is going on?", x="", y = "Average Acceleration") +
  theme_minimal()+
  geom_smooth(method="lm")




### TO analyse burst data, use the count data (BurstScore)
### Create a dataset with burst frequency within temperature groups 
### (cannot use count because there are more measurements in lowtemp group, but because BurstScore is either 0 or 1, mean = frequency and is independant of the number of measurements)
Full_Data_perday <- Full_Data_Out%>%  
  group_by(Date, Biologger) %>%
  summarise(
    mean_BurstScore = as.numeric(100*mean(BurstScore, na.rm = TRUE)),
    mean_AvgEA = as.numeric(mean(AvgEA, na.rm = TRUE)),
    mean_MaxEA = as.numeric(mean(MaxEA, na.rm = TRUE)),
    mean_VAR = as.numeric(mean(VAR, na.rm = TRUE)),
    mean_cAvgEA = as.numeric(mean(corr.AvgEA, na.rm = TRUE)),
    mean_cVAR = as.numeric(mean(corr.VAR, na.rm = TRUE)),
    Month = first(Month),
    Biologger = first(Biologger),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    Season = first(Season),
    LengthFeb = first(LengthFeb),
    WeightFeb = first(WeightFeb),
    Tempgroup = first(Tempgroup),
  )

str(Full_Data_perday$Biologger)

ggplot(Full_Data_perday, aes(x = Date, y = mean_cVAR, col=greb1l)) +
  facet_wrap(~food+Tempgroup)+
  geom_point(alpha=0.2) +
  labs(title = "WTF is going on?", x = "MaxEA", y = "(Variance corrected for MaxEA)") +
  theme_minimal()






CountBurstMod <- glmmTMB(mean_BurstScore ~ (Tempgroup+greb1l)*food + (1 |tank )+ (1 |fam ), ##best model
                         family= gaussian(),
                         data = TempBurst)

hist(resid(CountBurstMod))
qqPlot(resid(CountBurstMod))
performance(CountBurstMod)
summary(CountBurstMod)


EMM <- emmeans(CountBurstMod, ~greb1l:food:Tempgroup, data=TempBurst) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp, adjust="none")



predict.CountBurstMod <- ggpredict(CountBurstMod, terms = c("Tempgroup","food","greb1l"), ci.lvl = 0.95)


predict.CountBurstMod$x <- dplyr::recode(predict.CountBurstMod$x, "LowTemp" = "Water < 4.5°C",  "HighTemp" = "Water > 4.5°C")
predict.CountBurstMod$group <- dplyr::recode(predict.CountBurstMod$group, "high-food" = "High Food",  "low-food" = "Low Food")
predict.CountBurstMod$facet <- dplyr::recode(predict.CountBurstMod$facet, "Allele 1" = "Summer",  "Allele 2" = "Winter")
colnames(predict.CountBurstMod)[6] <- "Food"
colnames(predict.CountBurstMod)[7] <- "greb1l"

burst.plot <- ggplot(predict.CountBurstMod, aes(y= predicted, x=x, group = greb1l))+
  facet_grid(~Food)+
  geom_point(position = position_dodge(.4), size= 6, aes(col=greb1l))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, col=greb1l), position = position_dodge(.4),
                width=0.3) +
  scale_color_manual(values = c("gold2", "royalblue"))+
  xlab(expression(bold("Temperature")))+
  ylab(expression(bold("Predicted proportion of burst swimming (95% C.I.)")))+
  ylim(c(0, 6))+
  theme_bw()+
  theme(panel.spacing.x=unit(0, "lines"),
        plot.title = element_text(size =18),
        plot.subtitle =  element_text(size =17),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=15, vjust=0.6),
        panel.grid.minor =element_blank(),
        panel.grid.major=element_blank(),
        strip.text.x = element_text(size = 18, face="italic"),
        legend.position = c(0.1, 0.9) ,
        legend.box.spacing = unit(0, "lines"),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17, face="italic")
  )

burst.plot




ggplot(TempBurst, 
       aes(x=LengthFeb, y = mean_BurstScore, col=greb1l))+
  facet_wrap(~food)+
  #geom_boxplot(aes(fill=greb1l))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="", y="Proportion of time bursting (%)")+
  theme_bw()+
  scale_color_manual(values = c("gold2", "royalblue"),
                     labels = c("Summer runners", "Winter runners")) +
  theme(axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 17, face = "plain"),                        
        panel.grid = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 19, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.17,0.9),
  )    



ggplot(TempBurst, 
       aes(x=food, y = mean_BurstScore))+
  facet_wrap(~Tempgroup)+
  geom_boxplot(aes(fill=greb1l))+
  ylim(0, 9)+
  labs(x="", y="Proportion of burst swimming (%)")+
  theme_bw()+
  scale_fill_manual(values = c("gold2", "royalblue"),
                    labels = c("Summer", "Winter")) +
  theme(axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 17, face = "plain"),                        
        panel.grid = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 14, face = "italic"),
        legend.title = element_text(size = 17, face = "italic"),
        legend.position = c(0.17,0.9),
  )    






ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = BurstScore),
) +
  facet_wrap(Full_Data_Out$Biologger)+
  geom_point(alpha=.5)+
  geom_smooth()


### Maybe try to analyse the differences in burst power? Look at MaxEA using Filtered Data


### Analyse Burst&coast swim data 


## For MaxEA, Gamma is best but q-q plot looks weird

ACTmodelMax_F <- glmmTMB(MaxEA~ (TimeToNoon+Tempgroup+greb1l)*food + (1 |Biologger ),
                         family = Gamma("log"), 
                         data = FilteredData)

hist(resid(ACTmodelMax_F))
qqPlot(resid(ACTmodelMax_F))
performance(ACTmodelMax_F)
summary(ACTmodelMax_F)

ACTmodelVAR_F <- glmmTMB(VAR~ Light*Tempgroup*food + (1 |Biologger ), 
                         family = Gamma("log"), 
                         data = FilteredData)
hist(resid(ACTmodelVAR_F))
qqPlot(resid(ACTmodelVAR_F))
performance(ACTmodelVAR_F)
summary(ACTmodelVAR_F)


### Analyse Steady swim data 



ACTmodelAvg_B <- glmmTMB(AvgEA~ TimeToNoon*Tempgroup*food*greb1l + (1 |Biologger ),
                         family = Gamma("log"), 
                         data = BaselineData)

hist(resid(ACTmodelAvg_B))
qqPlot(resid(ACTmodelAvg_B))
performance(ACTmodelAvg_B)
summary(ACTmodelAvg_B)
drop1(ACTmodelAvg_B, test = "Chi")   


ACTmodelVAR_B <- glmmTMB(VAR~ Light*Tempgroup*food*greb1l + (1 |Biologger ), 
                         family = gaussian, 
                         data = BaselineData)
hist(resid(ACTmodelVAR_F))
qqPlot(resid(ACTmodelVAR_F))
performance(ACTmodelVAR_F)
summary(ACTmodelVAR_F)




### Analyse Steady swim data 


BaselineDataGAMM <- BaselineData%>%
  mutate(
    TimeToNoon = as.numeric(TimeToNoon),
    DateTime = as.numeric(DateTime),
    Date = as.numeric(Date),
  )

BaselineDataGAMM <- droplevels(BaselineDataGAMM)

BaselineDataGAMM_LF_HT <- subset(BaselineDataGAMM, food =="low-food" & Tempgroup =="HighTemp")
BaselineDataGAMM_LF_HT <- droplevels(BaselineDataGAMM_LF_HT)

Baseline2 <- subset(Full_Data_Out, VAR<220)
Baseline_LF <- subset(Baseline2, food =="low-food")
Baseline_LF_HT <- subset(Baseline_LF, Tempgroup =="HighTemp")
Baseline_LF_HT <- droplevels(Baseline_LF_HT)
### For MaxEA, Gamma is best but q-q plot looks weird

ACTmodelAvg_B <- gamm(AvgEA~ s(Time, by=greb1l),
                      random = list(Biologger= ~1),
                      family = Gamma("log"), 
                      data = BaselineDataGAMM_LF_HT)

hist(resid(ACTmodelAvg_B))
qqPlot(resid(ACTmodelAvg_B))
performance(ACTmodelAvg_B$gam)
summary(ACTmodelAvg_B$gam)

ACTmodelVAR_B <- gamm(VAR~ s(Time) + (Light+Tempgroup+greb1l)*food,
                      random = list(Biologger= ~1),
                      family = Gamma("log"), 
                      data = FilteredData)
hist(resid(ACTmodelVAR_B))
qqPlot(resid(ACTmodelVAR_B))
performance(ACTmodelVAR_B$gam)
summary(ACTmodelVAR_B$gam)



ACTmodelAvg_B <- glmmTMB(AvgEA~ (Tempgroup+TimeToNoon+greb1l)*food + (1 |Biologger ),
                         family = gaussian(), 
                         data = Baseline2)

hist(resid(ACTmodelAvg_B))
qqPlot(resid(ACTmodelAvg_B))
performance(ACTmodelAvg_B)
summary(ACTmodelAvg_B)
drop1(ACTmodelAvg_B, test = "Chi")   

ggplot(
  data = Baseline_LF_HT, 
  mapping = aes(x = Time, y = AvgEA, col=greb1l),
) +
  #facet_wrap(~food+Tempgroup)+
  geom_point(alpha=0.2)+
  geom_smooth()




ACTmodelVAR_B <- glmmTMB(VAR~ Light*greb1l + (1 |Biologger ), 
                         family = Gamma("log"), 
                         data = Baseline_LF_HT)
hist(resid(ACTmodelVAR_B))
qqPlot(resid(ACTmodelVAR_B))
performance(ACTmodelVAR_B)
summary(ACTmodelVAR_B)


ggplot(
  data = Baseline_LF_HT, 
  mapping = aes(x = Light, y = AvgEA),
) +
  #facet_wrap(~food+Tempgroup)+
  geom_boxplot(aes(fill=greb1l))
#geom_smooth(method="lm")



###################################           Trying to apply what I learned from the zero-inflated glmm course           ###############################################################################but Ibu

phi.est <- summary(ACTmodelMax_F$gam)$dispersion                        #' phi  
mu1     <- fitted(ACTmodelMax_F$gam)                       #' Exp(Y)
VarY    <- phi.est * mu1 ^ 2            #' var(Y) 
E1      <- (FilteredDataGAMM$LogMaxEA - mu1) / sqrt(VarY) #' Pearson residuals

#' Add the Pearson residuals and the fitted values to the
#' CR2 object.
FilteredDataGAMM$E1  <- E1
FilteredDataGAMM$mu1 <- mu1


ggplot(FilteredDataGAMM, aes(x = mu1, y = E1)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values", y = "Pearson residuals")



M1gam <- gam(LogMaxEA~ s(Time)+WeightFeb:food+(Tempgroup+greb1l)*food,
             family = Gamma(link = identity),
             data = FilteredDataGAMM)

## check with DHARMa
E1GammaQR <- simulateResiduals(fittedModel = M1gam, plot = FALSE)

# Check uniformity and outliers
plotQQunif(E1GammaQR, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)

# Residuals vs fitted
plotResiduals(E1GammaQR, quantreg = TRUE)


### NOTE : RESULTS ARE NOT GOOD
#########################################################################################################################################################################################



#

summary(ACTmodelMax_F$gam)
anova.gam(ACTmodelMax_F$gam)

predict.MaxEA <- ggpredict(ACTmodelMax_F, terms = c("Tempgroup", "greb1l","food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.MaxEA)


ggplot(
  data = FilteredData, 
  mapping = aes(x = Tempgroup, y = LogMaxEA),
) +
  facet_wrap(~food)+
  geom_boxplot(aes(fill=greb1l))




## Then, analyse baseline swimming activity (use Avg, maybe VAR)

### Convert time to numeric for the gamms

BaselineDataGAMM <- BaselineData%>%
  mutate(
    Time = as.numeric(Time),
    DateTime = as.numeric(DateTime),
  )

### DO NOT INCLUDE DATE AS RANDOM, IT MAKES R CRASH

## For AvgEA, gaussian is okay for log data but Gamma on raw data looks better (q-q plot-wise) => keep Gamma on raw

ACTmodelAvg_B <- gamm(AvgEA~ s(Time) + scale(Temp)*greb1l*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

ACTmodelAvg_B2 <- gamm(AvgEA~ s(Time) + Tempgroup*greb1l*food, 
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                       data = BaselineDataGAMM)

ACTmodelAvg_B3 <- gamm(AvgEA~ s(Time)+ Tempgroup*greb1l*food,  ### BEST MODEL
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma(link = log),
                       data = BaselineDataGAMM)

ACTmodelAvg_B4 <- gamm(AvgEA~ s(Time)+scale(Temp)+greb1l*food, 
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                       data = BaselineDataGAMM)

hist(resid(ACTmodelAvg_B2$gam))
qqPlot(resid(ACTmodelAvg_B2$gam))
summary(ACTmodelAvg_B3$gam)
anova.gam(ACTmodelAvg_B3$gam)

MuMIn::AICc(ACTmodelAvg_B2)

EMM <- emmeans(ACTmodelAvg_B, ~food*greb1l, data=BaselineData, at = list(Temp = c(2, 4, 6))) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)


predict.AvgEA <- ggpredict(ACTmodelAvg_B3, terms = c("Tempgroup","greb1l", "food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.AvgEA)


###################################           Trying to apply what I learned from the zero-inflated glmm course           ###############################################################################but Ibu

phi.est <- summary(ACTmodelAvg_B3$gam)$dispersion           #' phi  
mu1     <- fitted(ACTmodelAvg_B3$gam)                       #' Exp(Y)
VarY    <- phi.est * mu1 ^ 2            #' var(Y) 
E1      <- (BaselineDataGAMM$AvgEA - mu1) / sqrt(VarY) #' Pearson residuals

#' Add the Pearson residuals and the fitted values to the
#' CR2 object.
BaselineDataGAMM$E1  <- E1
BaselineDataGAMM$mu1 <- mu1


ggplot(BaselineDataGAMM, aes(x = mu1, y = E1)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values", y = "Pearson residuals")



M1gam <- gam(AvgEA~ s(Time)+ Tempgroup*greb1l*food,
             family = Gamma(link = identity),
             data = BaselineDataGAMM)

## check with DHARMa
E1GammaQR <- simulateResiduals(fittedModel = M1gam, plot = FALSE)

# Check uniformity and outliers
plotQQunif(E1GammaQR, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)

# Residuals vs fitted
plotResiduals(E1GammaQR, quantreg = TRUE)


### NOTE : RESULTS ARE NOT GOOD
#########################################################################################################################################################################################




## For MaxEA, Gamma is best but q-q plot looks weird

ACTmodelMax_B <- gamm(LogMaxEA~ s(Time)+scale(LengthFeb)+scale(Temp)*greb1l*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

ACTmodelMax_B2 <- gamm(LogMaxEA~ s(Time)+scale(Temp)*(greb1l+food), 
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                       data = BaselineDataGAMM)

ACTmodelMax_B3 <- gamm(LogMaxEA~ s(Time)+scale(LengthFeb):food+Tempgroup*greb1l*food, 
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                       data = BaselineDataGAMM)


hist(resid(ACTmodelMax_B3$gam))
qqPlot(resid(ACTmodelMax_B3$gam))

summary(ACTmodelMax_B3$gam)
anova.gam(ACTmodelMax_B3$gam)

MuMIn::AICc(ACTmodelMax_B3)




EMM <- emmeans(ACTmodelMax_F, ~Temp*food*greb1l, data=BaselineData, at = list(Temp = c(2, 4, 6))) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)


predict.MaxEA <- ggpredict(ACTmodelMax_B3, terms = c("Tempgroup","greb1l", "food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.MaxEA)



## For VAR, Gamma on log is best, q-q plot looks alright-ish

ACTmodelVar_B <- gamm(VAR~ s(Time)+Tempgroup*greb1l*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

hist(resid(ACTmodelVar_B$gam))
qqPlot(resid(ACTmodelVar_B$gam))
summary(ACTmodelVar_B$gam)
anova.gam(ACTmodelVar_B$gam)
EMM <- emmeans(ACTmodelVar_B, ~Temp*greb1l*food, data=BaselineData, at = list(Temp=c(2, 4, 6))) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)


predict.VAR <- ggpredict(ACTmodelVar_B, terms = c("Tempgroup","greb1l","food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.VAR)



CorTestBL<- lme(LogVAR ~ LogMaxEA,
                method="ML",
                random=~1|fam/tank,
                data = BaselineData)
qqPlot(resid(CorTestBL))
summary(CorTestBL)



