### This Script is for plotting temperature curves based on biologger data


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

source("path_file_lilian.R")

Data <- read_xlsx(paste0(path.data, "Biologger act&physio data.xlsx"))

DailyData <- Data %>%
  group_by(Date) %>%
  mutate(
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
    Time = lubridate::hms(Time),
    Temp = mean(Temp),
    BurstScore = as.numeric(BurstScore),
    Swimstyle = as.factor(Swimstyle),
    Season = as.factor(Season),
    Light = as.factor(Light),
    feeders = as.factor(feeders),
    fatmass = as.numeric(fatmass),
    fatpercent = as.numeric(fatpercent),
    WeightMay = as.numeric(WeightMay),
    WeightMayDXA = as.numeric(WeightMayDXA),
    NOAA_Sunrise = lubridate::hms(NOAA_Sunrise),
    NOAA_Sunset = lubridate::hms(NOAA_Sunset),
    MeasNB = as.factor(MeasNB),
  )%>%
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins"))%>%
  mutate(
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
    Time = hms::hms(Time),
    DayLengthHour = difftime(NOAA_Sunset, NOAA_Sunrise, units = "hours")
  )



str(DailyData)

DailyData <- DailyData%>%
mutate(DayLengthHour = as.numeric(DayLengthHour)
  )
scale = (max(DailyData$DayLengthHour)-1)/max(DailyData$Temp)

ggplot(data=DailyData, aes(x=Date))+
  geom_point(aes(y= DayLengthHour, col="Day length"))+
  geom_line(aes(y= DayLengthHour, col="Day length"))+
  geom_point(aes(y = scale*Temp, col= "Temperature"))+
  geom_line(aes(y = scale*Temp, col= "Temperature"))+
  theme_bw(base_size = 16)+
  scale_y_continuous(
    name = "Day length (h)",
    sec.axis=sec_axis(transform = ~./scale, name = "Temperature (°C)"))+
  scale_color_manual(values = c("orange", "darkred"), breaks = c("Day length", "Temperature"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text  = element_text(size = 18)
  )

