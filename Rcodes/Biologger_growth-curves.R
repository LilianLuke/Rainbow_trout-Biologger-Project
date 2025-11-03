### This script is to tidy body measurement data and plot growth curves ###


library(dplyr)
library(lubridate)
library(purrr)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(ggplot2)
library(ggeffects)



## Step 1: Load the complete dataset, as it contains meausrements for february and may already.

source("path_file_lilian.R")

Full_Data_Clean <- read_xlsx(paste0(path.data, "Biologger Clean data.xlsx"))


## Step 2: load the file with the measurements of November

Nov_Meas <- read_xlsx(paste0(path_meas, "November2024_Trout_Lammi.xlsx"))

Nov_Meas <- Nov_Meas %>%
  mutate(
    LengthNov = as.numeric(Length),
    WeightNov = as.numeric(Weight),
    `PIT tag` = tolower(PIT),
  )

## Step 3: merge the files together based on PIT tag 

Full_Meas_Data <- Full_Data_Clean %>%
  left_join(Nov_Meas, by = "PIT tag") %>%
  ##drop unnecessary columns
  dplyr::select(Biologger, LengthNov, LengthFeb, LengthMay, WeightNov, WeightFeb, WeightMay, food, greb1l, tank, fam)%>%
  group_by(Biologger)%>%
  summarize(
    LengthNov = first(LengthNov),
    WeightNov = first(WeightNov),
    LengthFeb = first(LengthFeb),
    WeightFeb = first(WeightFeb),
    LengthMay = first(LengthMay),
    WeightMay = first(WeightMay),
    food = first(food),
    greb1l = first(greb1l),
    tank = first(tank),
    fam = first(fam)
  )


## Step 4: rearrange data to have only 3 columns (lenght, weight, and month)

Nov_Meas_Data <- Full_Meas_Data %>%
  dplyr::select(
    Biologger, 
    LengthNov, 
    WeightNov,
    food,
    greb1l,
    tank,
    fam
    )%>%
  mutate(
    Month = "Nov",
  )%>%
  rename(
    Length = LengthNov,
    Weight = WeightNov,
  )
  
Feb_Meas_Data <- Full_Meas_Data %>%
  dplyr::select(
    Biologger, 
    LengthFeb, 
    WeightFeb,
    food,
    greb1l,
    tank,
    fam
  )%>%
  mutate(
    Month = "Feb",
  )%>%
  rename(
    Length = LengthFeb,
    Weight = WeightFeb,
  ) 

May_Meas_Data <- Full_Meas_Data %>%
  dplyr::select(
    Biologger, 
    LengthMay, 
    WeightMay,
    food,
    greb1l,
    tank,
    fam
  )%>%
  mutate(
    Month = "May",
  )%>%
  rename(
    Length = LengthMay,
    Weight = WeightMay,
  ) 


Full_Meas_Data_Clean <- Nov_Meas_Data %>%
  bind_rows(Feb_Meas_Data, May_Meas_Data)%>%
  mutate(
    Month = as.factor(Month),
  )%>%
  filter(
    !Length == "NA",
    !Biologger == "UXL0471DAT" ## This fish was shorter and lighter in may than in february, and I noted it was infected with flavio, so I exlcuded it from the measurements
  )

str(Full_Meas_Data_Clean$Month)
Full_Meas_Data_Clean$Month <- factor(Full_Meas_Data_Clean$Month, levels = c("Nov", "Feb", "May"))

## Step 5: Plot the data 

ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Length, col=greb1l, group = Biologger),
) +
  facet_wrap(~food)+
  geom_point(alpha=.5)+
  geom_path()+
  theme(legend.position = "none")


ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Weight, col=greb1l, group = Biologger),
) +
  facet_wrap(~food)+
  geom_point(alpha=.5)+
  geom_path()+
  theme(legend.position = "none")

## Looks like there is a greb effect in food-restricted ;/ 

ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Length),
) +
  facet_wrap(~food)+
  geom_boxplot(aes(fill=greb1l))

ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Weight),
) +
  facet_wrap(~food)+
  geom_boxplot(aes(fill=greb1l))



