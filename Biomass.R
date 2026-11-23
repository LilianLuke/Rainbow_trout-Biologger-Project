### Biomass checks 


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


Nov_measurements <- read_xlsx(paste0(path_meas, "November2024_Trout_Lammi.xlsx"))%>% ## Import measurement data from november 2024
  mutate(
    Tank= as.factor(Tank),
    Regime= as.factor(Regime)
  )%>%
  select(-Smoltification)

str(Nov_measurements)
with(Nov_measurements, table(Regime))

NovBiomass <- Nov_measurements%>%  ## Create a summary table per tank/food treatment
  group_by(Regime) %>%
  summarise(
    Total_Biomass = sum(Weight, na.rm = TRUE)/(3*1000), ## biomass in kg
    mean_bodymass = mean(Weight, na.rm = TRUE), ## mean body mass in g
    Density = sum(Weight, na.rm = TRUE)/(3*4.6*1000), ## Denisty in kg/m^3
  )
    

print(NovBiomass)


setwd(path_meas)
Bind_May <- list.files(path = path_meas, pattern="^MeasurementTroutMay25.*") ## Import measurement data from may 2025
May_surements <- map_dfr(Bind_May, function(fpath) {
  read_csv(fpath)
})%>%
  mutate(
    WeightMay= as.numeric(Weight),
    LengthMay= as.numeric(Length),
    Regime = as.factor(if_else(
      Tank %% 2 == 1,
      "Restricted", 
      "Control")),
    Tank= as.factor(Tank)
    )%>%
  select(PIT, Length, Weight, Tank, Regime)%>% 
  na.omit()

setwd(work_direc)
str(May_surements)

with(May_surements, table(Regime))

MayBiomass <- May_surements%>%  ## Create a summary table per tank/food treatment
  group_by(Regime) %>%
  summarise(
    Total_Biomass = sum(Weight, na.rm = TRUE)/(3*1000), ## biomass in kg
    mean_bodymass = mean(Weight, na.rm = TRUE), ## mean body mass in g
    Density = sum(Weight, na.rm = TRUE)/(3*4.6*1000), ## Denisty in kg/m^3
  )

print(MayBiomass)

