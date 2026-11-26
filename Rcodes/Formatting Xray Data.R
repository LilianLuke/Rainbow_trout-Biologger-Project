### this script is for ading the x-ray data to the Full_Data_Out dataset

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

##read the file from the Xray machine with all the info  

Xray_data <- read.xlsx(paste0(path_meas, "Biologger_x-ray_data.xlsx"))

## rename and select useful columns
Xray_data <- Xray_data %>%
  mutate(
    ID = as.character(PatientID),
    WeightMayDXA = as.numeric(`Total.Weight(g)`),
    leanmass = as.numeric(`Lean(g)`),
    fatpercent = as.numeric(`Fat(%%)`),
    fatmass = as.numeric(`Fat(g)`),
    BMC = as.numeric(BMC),
    leanpercent = as.numeric(100*(leanmass/WeightMayDXA)),
    BMCpercent = as.numeric(100*(BMC/WeightMayDXA)),
  )%>%
  select( fatpercent, leanpercent, BMCpercent, fatmass, leanmass, BMC, WeightMayDXA, ID
  )

##read the file with the logger number to link with the ID from the xray machine
Xray_list <- read.xlsx("Biologgers_2025_ x-ray list.xlsx")


str(Xray_list$Logger.number)

## rename and select useful columns, filter out the fish that did not have a logger and add UXL and DAT to match the format in the activity data file
Xray_list <- Xray_list %>%
  mutate(
    ID = as.character(Animal.ID),
    Loggerno = as.character(Logger.number)
  )%>%
  filter(
    Loggerno != "0",
    Loggerno != "no logger"
  )%>%
  mutate(
    Biologger = paste0("UXL0", Loggerno, "DAT")
  )

str(Xray_list$Logger.number)



## Merge both files to have the biologger ID and the data together

Xray_data_clean <- Xray_data %>%
  left_join(Xray_list, by = "ID") %>%
  ##drop unnecessary columns
  dplyr::select(-`Animal.ID`, -`Logger.number`,-ID, -PIT, -Loggerno)%>%
  filter(Biologger != "NA")


write_xlsx(Xray_data_clean, paste0(path.data, "/Biologger physio data.xlsx"))
write_csv(Xray_data_clean, paste0(path.data, "/Biologger physio data.csv"))


