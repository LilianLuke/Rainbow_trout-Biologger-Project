## This script is for formatting data from the biologgers and add the body mass and length measurements from feb and may + heart mass from may
## It also calculates SGR and modified Fulton's condition factor

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

#rm(list=ls(all=TRUE));gc()

## Set working directory (does not work otherwise?)
setwd(path.raw)

#list all individual logger files
Biologgers <- list.files(path = path.raw, pattern="\\DAT.CSV")

## ATTENTION : The CSV encoding is ISO-8859-1 (Latin-1), not UTF-8

##Merge all individual files in one data frame
merged_df <- map_dfr(Biologgers, function(fpath) {
  read_delim(fpath,
             delim   = ";",
             locale  = locale(encoding = "Latin1"),
             trim_ws = TRUE) %>%
    #add the biologger number associated with each measurement
    #(will be useful to match the activity to a fish PIT tag, genotype, food treatment, family and tank)
    mutate(
      Biologger = substring(
        file_path_sans_ext(basename(fpath)), 
        2)
    )
})


##clean up data frame
merged_df_clean <- merged_df %>%
  # drop unnecessary columns
  dplyr::select(-ACT, -`MinEA(mg)`) %>%
  #rename columns with complicated names
  rename(`Temp`=`Temp(°C)`, VAR = `VAR(Minute)`) %>%
  # move the “Biologger” column to position 1
  relocate(Biologger)


##read the file containing both the PIT tag and the Biologger number
lookup <- read_excel("C:/Users/03278487/OneDrive - Valtion/General - ThermoEvo/Lilian data/Biologger Mercury Data/Biologger fish sampling.xlsx")

##Make sure both columns are plain character 
##(not necessarily important but the guy on stackoverflow said it is good to do it)
lookup <- lookup %>%
  mutate(
    Biologger = as.character(Biologger) %>% trimws(),
    `PIT tag` = tolower(`PIT tag`) %>% trimws()
  )

##merge both file, associating the right pit tag to the right biologger
  
merged_with_PIT <- merged_df_clean %>%
  left_join(lookup, by = "Biologger") %>%
  ##drop unnecessary columns
  dplyr::select(-`Biologger no.`, -`Sample ID`, -notes, -...7, -...8)


##import measurement files and merge to get weight and length for feb and may measurements
setwd(Path_Meas)

May_surements <- list.files(path = Path_Meas, pattern="^MeasurementTroutMay25.*")

lookupMay <- map_dfr(May_surements, function(fpath) {
  read_csv(fpath)
})

lookupFeb <- read_csv("C:/Users/03278487/OneDrive - Valtion/General - ThermoEvo/Lilian data/Lammi Trout Info/MeasurementTroutFeb25.csv")

lookupFeb <- lookupFeb %>%
  #change column names 
  mutate(
    WeightFeb = as.numeric(Weight),
    LengthFeb = as.numeric(Length),
    `PIT tag` = tolower(PIT) %>% trimws(),
    FebMeasDate = mdy(Date),
  ) %>%
  #keep only useful columns
  dplyr::select(FebMeasDate, WeightFeb, LengthFeb, `PIT tag`)
  


lookupMay <- lookupMay %>%
  #change column names 
  mutate(
    WeightMay = as.numeric(Weight),
    LengthMay = as.numeric(Length),
    `PIT tag` = tolower(PIT) %>% trimws(),
    `DateB`= dmy(`Record Date`),
  ) %>%
  #keep only useful columns
  dplyr::select(Date, DateB, WeightMay, LengthMay, `PIT tag`)


lookupMay <- lookupMay %>%
  #change column names 
  mutate(DateA = mdy(Date),) %>%
  mutate(MayMeasDate = coalesce(DateA, DateB))%>%
  dplyr::select(MayMeasDate, WeightMay, LengthMay, `PIT tag`)
    
    
#str(lookupMay$MeasDate)





##merge the measas.Date()##merge the measurement columns to DF based on PIT
merged_with_Meas <- merged_with_PIT %>%
  left_join(lookupFeb, by = "PIT tag") %>%
  left_join(lookupMay, by = "PIT tag") 
  



lookupGen <- read_excel("C:/Users/03278487/OneDrive - Valtion/General - ThermoEvo/Lilian data/Lammi Trout Info/Lammi_Biologger_Fish_selection.xlsx")

lookupGen <- lookupGen %>%
#change column names 
  mutate(
    greb1l = as.factor(greb1l) %>% trimws(),
    omy05 = as.factor(inv) %>% trimws(),
    fam = as.factor(Family) %>% trimws(),
    tank = as.numeric(Tank),
    #add a food column that asigns food treatment based on tank number 
    #(even=high-food, odd=low-food)
    food = as.factor(if_else(
      tank %% 2 == 1,
      "low-food", 
      "high-food"
    )),
    `PIT tag` = tolower(PIT) %>% trimws()
  ) %>%
  #keep only useful columns
  dplyr::select(greb1l, omy05, fam, tank, food, `PIT tag`)

## Calculate modified Fulton's condition factor
 

## Add genotyping and food treatment data to creat a DF with all the data
Full_Data <- merged_with_Meas %>%
  left_join(lookupGen, by = "PIT tag") %>%
  #create new columns for growth and body condition factors
  mutate(
    #calculate specific growth rate (Crane et al. 2019)
    deltaT= as.numeric((MayMeasDate)-(FebMeasDate)),
    deltaW= as.numeric(log(WeightMay)-log(WeightFeb)),
    deltaL= as.numeric(LengthMay-LengthFeb),
    g = as.numeric((deltaW)/(deltaT)),
    SGRMass = as.numeric(100*expm1(g)),
    #Calculate growth rate for length (mm/day)
    growthlength= as.numeric(deltaL/deltaT),
    #based on coef found previously, calculate modified Fulton's condition factor (Aykanat et al. 2024)
    FultonFeb = as.numeric(100*WeightFeb/(LengthFeb)^3.453),
    FultonMay = as.numeric(100*WeightMay/(LengthMay)^3.311),
    DeltaFulton = as.numeric(FultonMay-FultonFeb),
    PercentDeltaFulton = as.numeric(100*(FultonMay-FultonFeb)/FultonFeb),
    `Heart weight (g)`= as.numeric(`Heart weight (g)`),
    HtoBratio = as.numeric(`Heart weight (g)`/WeightMay),
    AvgEA = as.numeric(`AvgEA(mg)`),
    MaxEA = as.numeric(`MaxEA(mg)`)
  ) 

#Get the coef of the weight/length correlation (Aykanat et al. 2024)
FultonFebCoef <- lm(log(WeightFeb)~log(LengthFeb), data=Full_Data)
summary(FultonFeb)

FultonMayCoef <- lm(log(WeightMay)~log(LengthMay), data=Full_Data)
summary(FultonMay)

plot(log(WeightMay)~log(LengthMay), data=Full_Data)
 


## Make sure all factors are considered correctly
Full_Data <- Full_Data %>%
  ## Qualitative variable as factors
  mutate(
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger)
  ) %>%
  dplyr::select(
    -`AvgEA(mg)`, -`MaxEA(mg)`
  ) 

ggplot(
  data = Full_Data, 
  mapping = aes(x = WeightFeb, y = WeightMay),
) +
  geom_point(alpha=.5)+
  geom_smooth()



## Save the new dataframe to path.data
write_xlsx(Full_Data, paste0(path.data, "/Biologger full data.xlsx"))
write_csv(Full_Data, paste0(path.data, "/Biologger full data.csv"))

                        