## This script is for formatting activity data from the biologgers.

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

source("path_file_lilian.R")

#list all individual logger files
Biologgers <- list.files(path = path_logger26, pattern="\\DAT.CSV")

## WARNING : The CSV encoding is ISO-8859-1 (Latin-1), not UTF-8
setwd(path_logger26)

##Merge all individual files in one data frame
merged_df <- map_dfr(Biologgers, function(fpath) {
  read_delim(fpath,
             delim   = ";",
             locale  = locale(encoding = "Latin1"),
             trim_ws = TRUE,
             col_types = cols(
               `VAR(Minute)` = col_number(),)
             ) %>%
    #add the biologger number associated with each measurement
    #(will be useful to match the activity to a fish PIT tag, genotype, food treatment, family and tank)
    mutate(
      Biologger = substring(
        file_path_sans_ext(basename(fpath)), 
        2)
    )
})

setwd(work_direc)

##clean up data frame
merged_df_clean <- merged_df %>%
  # drop unnecessary columns
  dplyr::select(-ACT, -`MinEA(mg)`) %>%
  #rename columns with complicated names
  rename(`Temp`=`Temp(°C)`, VAR = `VAR(Minute)`, AvgEA = `AvgEA(mg)`, MaxEA = `MaxEA(mg)`, ) %>%
  # move the “Biologger” column to position 1
  relocate(Biologger)

lookup <- read_xlsx(paste0(path_data26, "/Formated_DXA_data.xlsx"))%>%
  mutate(
  Biologger = as.factor(paste0("UXL0", Biologger, "DAT")),
  ) 
lookup <- lookup %>%
  mutate(
    Biologger = as.character(Biologger) %>% trimws(),
    PIT = tolower(PIT) %>% trimws()
  )


#############################################################################
### Add photoperiod, hyperoxia treatment and feeding times to the dataset.###
#############################################################################

prelimData <- merged_df_clean %>%
  left_join(lookup, by = "Biologger")%>%
  mutate(
    Temp = as.numeric(Temp),
    Vgll3 = as.factor(Vgll3),
    fam = as.factor(fam),
    tank = as.factor(Tank),
    Biologger = as.factor(Biologger),
    DateTime = lubridate::dmy_hms (paste0(Date, Time)),
    Date = dmy(Date),
    Time = lubridate::hms(Time)
  )%>%
  select(-Tank)%>%
  mutate(
    Light = as.factor(ifelse(Time < lubridate::hms("07:00:00"), #lights progressively turn on
                             "Night",
                             ifelse(Time < lubridate::hms("19:10:00"), #lights turn off
                                    "Day",
                                    "Night"))
                      ),
    Oxygen = if_else(
      DateTime >= dmy_hms("31/08/2026 18:00:00") & #hyperoxia start
        DateTime <= dmy_hms("02/09/2026 21:00:00"), #hyperoxia end
      "hyperoxia",
      "normoxia"
    ),
  ) %>%
  mutate(
    start_time = lubridate::hms("08:00:00"), #starting time of the feeders
    time_since_start = as.numeric(difftime(as.numeric(Time), as.numeric(lubridate::hms(start_time)), units = "secs")), #calculates how many minutes passed since 08:00
    cycle_pos = time_since_start %% 3273, #sets a 54.54minutes cycle, so if difftime=56min, cyclepos=1, because feeders in C4 were programmed to drop food 11 times between 08:00 and 17:00, so 11times in 600 mins, and 600/11=54.54, ~3273 secs
    food = if_else(tank != "C4",
                   if_else(
                     hour(hms::hms(Time)) >= 8 & #because feeders only turn on at 08:00
                       hour(hms::hms(Time)) <= 17 & #because feeders turn off at 17:00
                       minute(hms::hms(Time)) == 0, #because food is only droped at sharp hours
                     "feeding",
                     "not feeding"),
                   if_else(hour(hms::hms(Time)) >= 8 & #because feeders only turn on at 08:00
                             hour(hms::hms(Time)) <= 17 & #because feeders turn off at 17:00
                             cycle_pos == 0,
                           "feeding",   # because the food drops during the 1st minute of the cycle (cycle starts at 08:00, not 08:01)
                           "not feeding"
                   )
    ),
  )%>%
  mutate(
    start_date = lubridate::dmy("24/08/2026"),
    days_since_start = as.numeric(difftime(Date, start_date, units = "days")),
    Heatwave = if_else(
      Date < dmy("24/08/2026"), #heatwave start
      "pre-heatwave",
      if_else(
        Date <= dmy("04/09/2026"), #heatwave end
       # paste0("heatwave day", days_since_start),
       "heatwave",
        "post-heatwave"
      )
    )
  )%>%
  filter(Biologger != "UXL0449DAT",
    !(Skew == 0 & Kurt == -2),
    !(Skew == 0 & Kurt == 0),
    Temp > 0,
    Temp < 100,
    Date < "2026-09-09")

ggplot(data=prelimData, aes(y= log(AvgEA), x=Vgll3, fill= Vgll3))+
  facet_wrap(~food+Light)+
  geom_boxplot()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("salmon", "red4"), name = "Vgll3")+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic")
  )


ggplot(data=prelimData, aes(y= log(AvgEA), x=DateTime, col= Vgll3))+
  facet_wrap(~food+Light)+
  geom_point(alpha=.1)+
  geom_smooth()+
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("salmon", "red4"), name = "Vgll3")+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic")
  )


ggplot(data=prelimData, aes(y= log(AvgEA), x=tank, fill= Vgll3))+
  facet_wrap(~food+Light)+
  geom_violin()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("salmon", "red4"), name = "Vgll3")+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic")
  )



ggplot(data=prelimData, aes(y= AvgEA, x=MaxEA, col= Vgll3))+
  facet_wrap(~food+Light)+
  geom_point(alpha=.1)






