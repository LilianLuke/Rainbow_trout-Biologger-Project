## This script is for Excluding undesireable data points (broken logger or unreliable data)
## It also adds month and seasons to be able to separate the measurements later


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


Full_Data <- read_xlsx(paste0(path.data, "Biologger full data.xlsx"))

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
    ##Also add a season factor, based on previous analysis
    Season = as.factor(if_else(
      Date < "2025-03-14",
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

