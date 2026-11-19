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
library(ggeffects)
library(glmmTMB)
library(performance)
library(DHARMa)
library(mgcv)
library(tidygam)
library(hms)
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
Full_Data_Clean <- read_xlsx(paste0(path.data, "Biologger final data.xlsx")) ## Import dataset


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



##Add the BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
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

## some of the loggers started measuring at 00:00 instead of 12:00
## and these first points were usually excluded of the data because of error in the measurements
## so the first measurement is either at 00:30 or 12:30
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

## create a column to tell whether the fish were fed or not during the measurement
daytimefooddata <- comparefooddata %>%
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
## To see if there is a direct food effect, we can compare activity in the morning 
## between loggers that started at midnight and those that started at noon. 
## Indeed, both have the same average food availability, but loggers that started recording at midnight get food in the morning
## while loggers that started at noon do not receive food in the morning (because their morning will be on the next day, i.e., the fasting day)
## and measurements occurring during the night, as food was never dropped during night time
AMfooddataLM <- daytimefooddata %>%
  filter(
    daytime == "AM",
    Light=="Night"
  )

ggplot(data=AMfooddataLM, aes(y= log(AvgEA), x=food, fill=start_type))+
  geom_boxplot()+
  theme_bw(base_size = 16)+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic")
  )+
  labs(y="log(AvgEA)")

## No obvious sign of greater activity when fish have food vs when they do not.


FastingLM <- glmmTMB(log(AvgEA) ~ start_type*food + (1|Biologger),
                   data = AMfooddataLM)


hist(resid(FastingLM))
qqPlot(resid(FastingLM))
Fastingqr <- simulateResiduals(fittedModel = FastingLM, plot = FALSE)
Fastingqr$scaledResiduals
plotQQunif(Fastingqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)

summary(FastingLM)

predict.FastingLM <- ggpredict(FastingLM, terms = c("food", "start_type"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.FastingLM)

## Even though no statistical significance was reached,
## It seems like there is a bigger difference in the ad libitum fed group, 
## which makes no sense since food availability is the same on both mornings...


## Since we found date affects activity quite a lot, maybe try a GAMM including date 
## to try and see if there are more subtle patterns?

AMfooddataGAMM <- daytimefooddata %>% ## need to convert Date to numeric for Gamm
  mutate(
    Date=as.numeric(Date)
  )


Fasting <- bam(AvgEA ~ s(Date, k=20) + start_type*food+ s(tank, bs="re")+ s(Biologger, bs="re") + s(fam, bs="re"),
               family = Gamma(link = "log"),
               data = AMfooddataGAMM)

performance(Fasting)
summary(Fasting)

EMM <- emmeans(Fasting, ~ start_type*food, data=AMfooddataGAMM) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)

predict.Fasting <- ggpredict(Fasting, terms = c("Date", "start_type", "food" ), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.Fasting)

## Again, some sort of difference in the high-food group that is not relevant
## No sign of different activity between morning with food vs morning without food.
## Conclusion: no direct effect of food availability on general activity
