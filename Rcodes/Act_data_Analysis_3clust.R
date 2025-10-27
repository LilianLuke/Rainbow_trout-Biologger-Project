#### THIS SCRIPT IS FOR ANALYSING THE DATA BASED ON A 3-TYPES OF SWIMMING MODEL (Steady vs calm vs dynamic) ####

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
    BurstScore = as.factor(if_else(
      VAR < 190, ### ADJUST VALUE
      0,
      1,
    )),
    ##Add a SwimStyle column to be able to split data based on bursting
    Swimstyle = as.factor(if_else(
      VAR < 20,
      "Steady",
      if_else(
        VAR < 150,
        "Calm",
        "Dynamic"
      ),
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





