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


source("path_file_lilian.R")
Full_Data_Clean <- read_xlsx(paste0(path.data, "Biologger final data.xlsx")) 


Full_Data_Out <- Full_Data_Clean %>%
  mutate(
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
    Light = as.factor(Light),
  ) %>%
  ## Convert Date and Time back to a format that can be plotted correctly
  mutate(
    Date = ymd(Date),
    Time = hms(Time),
  )%>%
  filter (
    !AvgEA == MaxEA
  )



##Add a BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
Full_Data_Out <- Full_Data_Out %>%
  mutate(
    BurstScore = as.numeric(if_else(
      VAR < 190,
      0,
      1,
    )),
    ##Add a SwimStyle column to be able to split data based on bursting
    Swimstyle = as.factor(if_else(
      VAR < 190,
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
str(Full_Data_Out)
str(Full_Data_Out$Biologger)


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


dayfooddataLM <- middayfooddata %>%
  filter(
    start_type == "noon",
    Light=="Day"
  )%>%
  mutate(
    Date=as.numeric(Date)
  )


nightfooddataLM <- middayfooddata %>%
  filter(
    start_type == "midnight"
  )%>%
  mutate(
    Date=as.numeric(Date)
  )



Fasting <- lme(log(AvgEA) ~ Date*daytime*food,
                     method="ML",
                     random=list(Biologger=~1, fam=~1, tank=~1),
                     data = middayfooddataLM)
hist(resid(Fasting))
qqPlot(resid(Fasting))
summary(Fasting)
predict.Fasting <- ggpredict(Fasting, terms = c("Date", "daytime", "food", "Light"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.Fasting)

Fasting <- bam(AvgEA ~ s(Date) + daytime*food+ s(tank, bs="re")+ s(Biologger, bs="re") + s(fam, bs="re"),
               family = Gamma(link = "log"),
               data = dayfooddataLM)

Fasting <- bam(AvgEA ~ s(Date) + daytime*food*Light+ s(tank, bs="re")+ s(Biologger, bs="re") + s(fam, bs="re"),
               family = Gamma(link = "log"),
               data = nightfooddataLM)


EMM <- emmeans(Fasting, ~ daytime*food*start_type, data=nightfooddataLM) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)

### The model shows a general effect of the food treatment, with activity being lower in the low food group 
### compared to the high food group on the afternoon of the second feeding day

### It also shows a significant immediate effect of absence of food, 
### with activity being significantly lower on the morning of the fasting day
### compared to the afternoon of the feeding day


### Note: the low-fed fish are already less active in the night before the fasting day,
### which suggests that they may have learned the feeding schedule.


middayfooddatanoon <- middayfooddata %>%
  filter(
    start_type == "midnight",
    Light == "Day"
  )

ggplot(
  data = middayfooddatanoon, 
  mapping = aes(x = Date, y = BurstScore, color=food),
) +
  facet_wrap(~daytime)+
  geom_point()+
  geom_smooth()


ggplot(
  data = middayfooddatanoon, 
  mapping = aes(x = Date, y = BurstScore, color=daytime),
) +
  facet_wrap(~food)+
  geom_point(alpha=.05)+
  geom_smooth(method=lm)



ggplot(
  data = FoodBurst, 
  mapping = aes(x = Tempgroup, y = mean_AvgEA, fill=start_type),
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



