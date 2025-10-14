### This script is to compare activity between feeding and non-feeding days in the low-food group


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



Full_Data_Clean <- read_xlsx("C:/Users/03278487/OneDrive - Valtion/General - ThermoEvo/Lilian data/R outputs/Biologgers data/Biologger Clean data.xlsx") 


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
    !AvgEA == MaxEA
  )



##Create a log column for each act parameter to be able to run GAMMS on it
Full_Data_Out <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    LogAvgEA = as.numeric(log10(`AvgEA`+0.001)),
    LogMaxEA = as.numeric(log10(`MaxEA`+0.001)),
    LogVAR = as.numeric(log10(VAR+0.76)),
  )

##Add a BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
Full_Data_Out <- Full_Data_Out %>%
  mutate(
    BurstScore = as.numeric(if_else(
      VAR < 220,
      0,
      1,
    )),
    ##Add a SwimStyle column to be able to split data based on bursting
    Swimstyle = as.factor(if_else(
      VAR < 220,
      "Steady",
      "Burst",
    )),
    ## Add a Tempgroup column to be able to split data based on the temp threshold identified when looking at VAR/Temp
    Tempgroup = as.factor(if_else(
      Temp < 4.5,
      "LowTemp",
      "HighTemp"
    )),
  ) 


### Force the plots to show winter or low-temp on the left and spring or high-temp on the right
Full_Data_Out$Season <- factor(Full_Data_Out$Season, levels = c("Winter", "Spring"))
Full_Data_Out$Tempgroup <- factor(Full_Data_Out$Tempgroup, levels = c("LowTemp", "HighTemp"))

## make sure factors are read correctly
str(Full_Data_Out$Biologger)
str(Full_Data_Out$fam)
str(Full_Data_Out$greb1l)
str(Full_Data_Out$omy05)
str(Full_Data_Out$tank)
str(Full_Data_Out$Date)
str(Full_Data_Out$Time)
str(Full_Data_Out$DateTime)
str(Full_Data_Out$BurstScore)
str(Full_Data_Out$Swimstyle)
str(Full_Data_Out$Tempgroup)


comparefooddata <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    start_time = first(Time[Number == 2]),
    start_type = case_when(
      start_time == hms::as_hms("00:30:00") ~ "midnight",
      start_time == hms::as_hms("12:30:00") ~ "noon",
      TRUE ~ "other"
    )
  ) %>%
  ungroup()


middayfooddata <- comparefooddata %>%
  mutate(
    time= as.numeric(Time),
    daytime = if_else( 
      time > 43200,
      "PM",
      "AM"),
    fasting = case_when(
      daytime == "AM" & start_type == "noon" ~ "yes",
      .default = "no",
    ),
    fasting = as.factor(fasting),
    start_type = as.factor(start_type),
    daytime = as.factor(daytime),
    
  )




FoodBurst <- middayfooddata%>%  
  group_by(food, Tempgroup, daytime, start_type, fasting, Biologger) %>%
  summarise(
    mean_BurstScore = as.numeric(100*mean(BurstScore, na.rm = TRUE)),
    Biologger = first(Biologger),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    LengthFeb = first(LengthFeb),
    WeightFeb = first(WeightFeb)
  )


Fasting <- lme(mean_BurstScore ~  daytime*start_type*food*Tempgroup, ##best model
                     method="ML",
                     random=list(fam=~1, tank=~1),
                     data = FoodBurst)
hist(resid(Fasting))
qqPlot(resid(Fasting))
summary(Fasting)
predict.Fasting <- ggpredict(Fasting, terms = c("Tempgroup", "daytime", "start_type", "food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.Fasting)

EMM <- emmeans(Fasting, ~daytime+start_type+Tempgroup, data=FoodBurst) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp, adjust="none")




ggplot(
  data = FoodBurst, 
  mapping = aes(x = Tempgroup, y = mean_BurstScore, color=start_type),
) +
  facet_wrap(~daytime+food)+
  geom_boxplot()

ggplot(
  data = middayfooddata, 
  mapping = aes(x = hms::hms(Time), y = BurstScore),
) +
  facet_wrap(~start_type+food)+
  geom_point(alpha=.01)+
  geom_smooth()



