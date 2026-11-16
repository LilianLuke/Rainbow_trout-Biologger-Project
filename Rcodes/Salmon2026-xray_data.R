### this script is for formatting the salmon 2026 x-ray data 

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

##read the file with the Fish full PIT to link with surgery data (Logger ID)
Laukaa_AllFish_list <- read.xlsx(paste0(path_gen26, "Laukaa_all_fish_genot_2026.xlsx"))

str(Laukaa_AllFish_list)

## rename and select useful columns, filter out the fish that did not have a logger and add UXL and DAT to match the format in the activity data file
Laukaa_AllFish_list <- Laukaa_AllFish_list %>%
  mutate(
    PIT = as.character(PIT),
    rearTank = as.factor(Tank),
    fam = as.factor(Family),
    Vgll3 = as.factor(Vgll3),
    Sex = as.factor(Sex)
  )%>%
  select(PIT, rearTank, Vgll3, Sex, fam
  )

str(Laukaa_AllFish_list)

##read the file with the Fish PIT AND Logger ID
Laukaa_LogFish_list <- read.xlsx(paste0(path_data26, "LoggerFish_List.xlsx"))

str(Laukaa_LogFish_list)

## rename and select useful columns, filter out the fish that did not have a logger and add UXL and DAT to match the format in the activity data file
Laukaa_LogFish_list <- Laukaa_LogFish_list %>%
  mutate(
    PIT = as.character(PIT),
    InitialWeight = as.numeric(`Weight.(g)`),
    InitialLength = as.numeric(`Length.(mm)`),
    Biologger = as.character(`Biologger.ID`),
    Tank = as.factor(Tank),
  )%>%
  select( PIT, InitialWeight, InitialLength, Biologger, Tank
  )

str(Laukaa_LogFish_list)


## Merge AllFish and LoggerFish based on PIT to get all fish info in one file
LogFish_clean <- Laukaa_LogFish_list %>%
  left_join(Laukaa_AllFish_list, by = "PIT") %>%
  filter(Biologger != "", Biologger != "no logger", Biologger != "might not have one, 519 was inserted in another fish") %>%
  mutate(
    Vgll3 = as.factor(Vgll3),
    Sex = as.factor(Sex),
    PITend = as.numeric(substr(as.character(PIT),
                    nchar(as.character(PIT))-4,
                    nchar(as.character(PIT))))
  )

str(LogFish_clean)


##read the file from the Xray machine with all DXA info

Xray_data_raw <- read.xlsx(paste0(path_dxa26, "Biologger_x-ray_data_2026.xlsx"))

str(Xray_data_raw)

## rename and select useful columns
Xray_data <- Xray_data_raw %>%
  mutate(
    PITend = as.numeric(PatientName),
    xrayID = as.factor(PatientName),
    FinalWeightDXA = as.numeric(`Total.Weight(g)`),
    leanmass = as.numeric(`Lean(g)`),
    fatpercent = as.numeric(`Fat(%%)`),
    fatmass = as.numeric(`Fat(g)`),
    BMC = as.numeric(BMC),
    leanpercent = as.numeric(100*(leanmass/FinalWeightDXA)),
    BMCpercent = as.numeric(100*(BMC/FinalWeightDXA)),
  )%>%
  select( fatpercent, leanpercent, fatmass, leanmass, FinalWeightDXA, PITend, xrayID
  )

str(Xray_data)


LogFish_DXA <- LogFish_clean %>%
  left_join(Xray_data, by = "PITend")%>%
  filter(Biologger != "", Biologger != "no logger", Biologger != "might not have one, 519 was inserted in another fish")%>%
  select(PIT, Biologger, rearTank, Vgll3, fam, Tank, InitialWeight, InitialLength, fatpercent, leanpercent, fatmass, leanmass, FinalWeightDXA)

str(LogFish_DXA)

write_xlsx(LogFish_DXA, paste0(path_data26, "/Formated_DXA_data.xlsx"))



ggplot(data=LogFish_DXA, aes(y=fatpercent, x=Vgll3 , fill=Vgll3))+
  facet_wrap(~Tank)+
  geom_violin()+
  scale_fill_manual(values = c("gold2", "royalblue"))+
  theme_bw(base_size = 16)


