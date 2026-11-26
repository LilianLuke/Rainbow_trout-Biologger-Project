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
  select(-Smoltification)%>%
  filter(Weight > 10)

str(Nov_measurements)

sum(duplicated(Nov_measurements$PIT)) ## check if any duplicate fish
Nov_measurements %>% ## check if duplicates are actual ID or typos
  count(PIT) %>%
  filter(n > 1)

Nov_measurements <- Nov_measurements%>% ## remove duplicate PITs and typos, just to make sure
  filter(PIT!= 0,
         !duplicated(PIT, fromLast = TRUE))


with(Nov_measurements, table(Regime))

NovBiomass <- Nov_measurements%>%  ## Create a summary table per tank/food treatment
  group_by(Regime) %>%
  summarise(
    Total_Biomass = sum(Weight, na.rm = TRUE)/(3*1000), ## average biomass per tank in kg (3 tanks)
    mean_bodymass = mean(Weight, na.rm = TRUE), ## mean body mass in g
    Density = sum(Weight, na.rm = TRUE)/(3*4.6*1000), ## average denisty per tank in kg/m^3 (3 tanks, 4.6m^3 each)
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
  na.omit()%>%
  filter(Weight > 10)

sum(duplicated(May_surements$PIT)) ## check if any duplicate fish
May_surements %>% ## check if duplicates are actual ID or typos
  count(PIT) %>%
  filter(n > 1)

May_surements <- May_surements%>% ## remove duplicate PITs and typos, just to make sure (excluding fish means underestimated total biomass)
  filter(PIT != 0,
         !duplicated(PIT, fromLast = TRUE))
setwd(work_direc)

str(May_surements)

with(May_surements, table(Regime, Tank))

MayBiomass <- May_surements%>%  ## Create a summary table per tank/food treatment
  group_by(Regime) %>%
  summarise(
    Total_Biomass = sum(Weight, na.rm = TRUE)/(3*1000), ## average biomass per tank in kg (3 tanks)
    mean_bodymass = mean(Weight, na.rm = TRUE), ## mean body mass in g
    Density = sum(Weight, na.rm = TRUE)/(3*4.6*1000), ## average denisty per tank in kg/m^3 (3 tanks, 4.6m^3 each)
  )

print(MayBiomass)

## look for outliers on plots?

ggplot(data=Nov_measurements, aes(y=log(Weight), x=log(Length)))+
  facet_wrap(~Regime)+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 16)
## no obvious outliers

ggplot(data=May_surements, aes(y=log(Weight), x=log(Length)))+
  facet_wrap(~Regime)+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 16)
## no obvious outliers, maybe the one >1kg fish but nothing really obvious and I doubt that one fish would change much...


