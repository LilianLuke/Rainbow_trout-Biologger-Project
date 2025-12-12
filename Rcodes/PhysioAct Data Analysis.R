### This Script is for looking at the effect of activity on physiological parameters 

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

source("path_file_lilian.R")


Full_Data <- read_xlsx(paste0(path.data, "Biologger Final data.xlsx")) 

Full_Data <- Full_Data %>%
  select(
    !Season
    )%>%
  rename(
    Season = Tempgroup
  )%>%
  mutate(
    Season=recode_factor(
    Season, "LowTemp" = "Winter",  "HighTemp" = "Spring")
    )


### Force the plots to show winter or low-temp on the left and spring or high-temp on the right
Full_Data$Season <- factor(Full_Data$Season, levels = c("Winter", "Spring"))

Xray_data_clean <- read_xlsx(paste0(path.data,"Biologger Physio data.xlsx"))

str(Xray_data_clean$fatpercent)
str(Xray_data_clean$fatmass)


Full_Data_ActnPhysio <- Full_Data %>%
  left_join(Xray_data_clean, by = "Biologger")


Full_Data_ActnPhysio <- Full_Data_ActnPhysio %>%
  mutate(
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
    Date = ymd(Date),
    Time = hms(Time),
    BurstScore = as.numeric(BurstScore),
    Swimstyle = as.factor(Swimstyle),
    Season = as.factor(Season),
    Light = as.factor(Light),
    feeders = as.factor(feeders),
    fatmass = as.numeric(fatmass),
    fatpercent = as.numeric(fatpercent),
    WeightMay = as.numeric(WeightMay),
    WeightMayDXA = as.numeric(WeightMayDXA),
    NOAA_Sunrise = hms(NOAA_Sunrise),
    NOAA_Sunset = hms(NOAA_Sunset),
    MeasNB = as.factor(MeasNB)
  )%>%
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins")
  )%>%
  mutate(
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
  )


## make sure factors are read correctly
str(Full_Data_ActnPhysio$Biologger)
str(Full_Data_ActnPhysio$fam)
str(Full_Data_ActnPhysio$greb1l)
str(Full_Data_ActnPhysio$omy05)
str(Full_Data_ActnPhysio$tank)
str(Full_Data_ActnPhysio$Date)
str(Full_Data_ActnPhysio$Time)
str(Full_Data_ActnPhysio$DateTime)
str(Full_Data_ActnPhysio$BurstScore)
str(Full_Data_ActnPhysio$Swimstyle)
str(Full_Data_ActnPhysio$Season)
str(Full_Data_ActnPhysio$Light)
str(Full_Data_ActnPhysio$feeders)
str(Full_Data_ActnPhysio$DayLength)
str(Full_Data_ActnPhysio$NOAA_Sunrise)
str(Full_Data_ActnPhysio$NOAA_Sunset)
str(Full_Data_ActnPhysio$MeasNB)


#write_xlsx(Full_Data_ActnPhysio, paste0(path.data, "/Biologger act&physio data.xlsx"))
#write_csv(Full_Data_ActnPhysio, paste0(path.data, "/Biologger act&physio data.csv"))



Data <- read_xlsx(paste0(path.data, "Biologger act&physio data.xlsx"))

Data <- Data %>%
  mutate(
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
    Date = ymd(Date),
    Time = hms::hms(Time),
    BurstScore = as.numeric(BurstScore),
    Swimstyle = as.factor(Swimstyle),
    Season = as.factor(Season),
    Light = as.factor(Light),
    feeders = as.factor(feeders),
    fatmass = as.numeric(fatmass),
    fatpercent = as.numeric(fatpercent),
    WeightMay = as.numeric(WeightMay),
    WeightMayDXA = as.numeric(WeightMayDXA),
    NOAA_Sunrise = hms(NOAA_Sunrise),
    NOAA_Sunset = hms(NOAA_Sunset),
    MeasNB = as.numeric(MeasNB),
  )%>%
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins"),
    MeasNB = as.factor(MeasNB),
  )%>%
  mutate(
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
  )



str(Data$Biologger)
str(Data$fam)
str(Data$greb1l)
str(Data$omy05)
str(Data$tank)
str(Data$Date)
str(Data$Time)
str(Data$DateTime)
str(Data$BurstScore)
str(Data$Swimstyle)
str(Data$Season)
str(Data$Light)
str(Data$feeders)
str(Data$DayLength)
str(Data$MeasNB)


Data$Season <- factor(Data$Season, levels = c("Winter", "Spring"))


ggplot(Data, aes(x = food, y = WeightFeb, fill=greb1l)) +
  geom_boxplot()



ggplot(Data, aes(x = DayLength, y = log(VAR))) +
  facet_wrap(~Season)+
  geom_point() +
  geom_smooth(method = "lm")+
  theme_minimal()


PhysioActData <- Data%>%  
  group_by(#Light,
    Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_corrAvgEA = mean(corr.AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    MaxEA = max(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    logBurstScore = as.numeric(log(count_BurstScore)),
    BurstProp = as.numeric(100*mean(BurstScore, na.rm = TRUE)),
    WeightFeb = first(WeightFeb),
    WeightMay = first(WeightMay),
    LengthFeb = first(LengthFeb),
    LengthMay = first(LengthMay),
    FultonFeb = first(FultonFeb),
    FultonMay = first(FultonMay),
    PercentDeltaFulton = first(PercentDeltaFulton),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    SGRMass = first(SGRMass),
    LengthFeb = first(LengthFeb),
    LengthMay = first(LengthMay),
    Heartmass = first(`Heart weight (g)`),
    HtoBratio = first(HtoBratio),
    fatpercent = first(fatpercent),
    fatmass = first(fatmass),
    leanmass = first(leanmass),
  )%>%  
  filter(
    Biologger != "UXL0552DAT",#|
    Biologger != "UXL0550DAT",#|
    Biologger != "UXL0500DAT",#|--  Loggers lacking some physiological data 
    Biologger != "UXL0442DAT",#|
    Biologger != "UXL0438DAT",#|
  ) %>%
  mutate(
    greb1l = as.factor(greb1l),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
  )

PhysioActDayta <- PhysioActData %>%
  filter(
    Light == "Day"
  )

PhysioActNight <- PhysioActData %>%
  filter(
    Light == "Night"
  )


ggplot(
  data = `PhysioActData`, 
  mapping = aes(x = Light, y = mean_MaxEA, fill=food)
) +
  geom_boxplot()


str(PhysioActData$Biologger)
str(PhysioActData$fam)
str(PhysioActData$greb1l)
str(PhysioActData$tank)
str(PhysioActData$food)
str(PhysioActData$Date)
str(PhysioActData$Time)
str(PhysioActData$DateTime)
str(PhysioActData$BurstScore)
str(PhysioActData$Swimstyle)
str(PhysioActData$Season)


## test if greb, food or activity affects lean mass

leanm_burst  <- lme(leanmass ~ BurstProp,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_var    <- lme(leanmass ~ mean_VAR,    random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_avgEA  <- lme(leanmass ~ mean_AvgEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_maxEA  <- lme(leanmass ~ mean_MaxEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_corrAvgEA  <- lme(leanmass ~ mean_corrAvgEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_MaxEA  <- lme(leanmass ~ MaxEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(leanm_burst, leanm_var, leanm_avgEA, leanm_maxEA, leanm_corrAvgEA, leanm_MaxEA)



Modellean <- lme(leanmass ~ log(mean_MaxEA)*greb1l+food, #<= BEST MODEL
                 method="ML",
                 random=list(fam=~1, tank=~1),
                 data = PhysioActData)

performance(Modellean)

hist(resid(Modellean))
qqPlot(resid(Modellean))
summary(Modellean)
anova(Modellean)

predict.Modellean <- ggpredict(Modellean, terms = c("mean_MaxEA", "greb1l","food"), ci.lvl = 0.95)
plot(predict.Modellean, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)


ggplot(
  data = PhysioActDayta, 
  mapping = aes(x = mean_MaxEA, y = leanmass, col=greb1l)
) +
  facet_wrap(~food)+
  geom_point()+
  geom_smooth()


## fat mass correlates with body size (lean mass)

ModelFat <- lme(log(fatmass) ~ log(leanmass),
             method = "REML",
             random = list(tank=~1, fam=~1),
              data = PhysioActData)

hist(resid(ModelFat))
qqPlot(resid(ModelFat))
summary(ModelFat)
anova(ModelFat)
PhysioActData$resid_fat <- resid(ModelFat)

ggplot(
  data = `PhysioActDayta`, 
  mapping = aes(x = log(leanmass), y = log(fatmass), col=food)
) +
  geom_point()+
  geom_smooth(method=lm)

predict.ModelFat <- ggpredict(ModelFat, terms = c("leanmass"), ci.lvl = 0.95)
plot(predict.ModelFat, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)




fatm_burst  <- lme(logFM ~ count_BurstScore,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
fatm_var    <- lme(logFM ~ mean_VAR,    random = list(fam = ~1, tank = ~1), data = PhysioActData)
fatm_avgEA  <- lme(logFM ~ mean_corrAvgEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
fatm_maxEA  <- lme(logFM ~ mean_MaxEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(fatm_burst, fatm_var, fatm_avgEA, fatm_maxEA)



ModelFatAct <- glmmTMB(resid_fat ~ food + (1|fam)+ (1|tank), # <= Best Model 
                   family = gaussian(),
                   data = PhysioActData)


hist(resid(ModelFatAct))
qqPlot(resid(ModelFatAct))
summary(ModelFatAct)

ggplot(
  data = PhysioActData, 
  mapping = aes(x = food, y = resid_fat , fill=greb1l)
) +
  geom_violin()
### <= greb1l effect not significant in the GLMM, but looks like something is going on in the low food group






Fultonm_burst  <- lme(PercentDeltaFulton ~ log(BurstProp),  random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_var    <- lme(PercentDeltaFulton ~ log(mean_VAR),    random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_avgEA  <- lme(PercentDeltaFulton ~ mean_AvgEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_maxEA  <- lme(PercentDeltaFulton ~ log(mean_MaxEA),  random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(Fultonm_burst, Fultonm_var, Fultonm_avgEA, Fultonm_maxEA)

Fultonm_lean  <- lme(PercentDeltaFulton ~ leanmass,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_fat    <- lme(PercentDeltaFulton ~ fatmass,    random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_fatPer    <- lme(PercentDeltaFulton ~ fatpercent,    random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(Fultonm_lean, Fultonm_fat,Fultonm_fatPer)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = leanmass, y = resid_fat, col=food)
) +
  geom_point()+
  geom_smooth(method=lm)




TestFulton <- lme(resid_fat ~ leanmass,
                   method="ML",
                   random=list(fam=~1, tank=~1),
                   data = PhysioActData)
r2 <- r.squaredGLMM(TestFulton)
r2c <- r2[1, "R2c"]



BestModelFulton <- glmmTMB(PercentDeltaFulton ~ log(BurstProp)+greb1l + (1|fam) + (1| tank), #best model ?
                       family = gaussian(),
                       data = PhysioActData)

hist(resid(BestModelFulton))
qqPlot(resid(BestModelFulton))
summary(BestModelFulton)

r2 <- r.squaredGLMM(BestModelFulton)
r2c <- r2[1, "R2c"]


predict.ModelFulton <- ggpredict(BestModelFulton, terms = c("BurstProp"), ci.lvl = 0.95)
plot(predict.ModelFulton, colors = c("darkgreen",  "palegreen2"), line_size = 1.5)

ggplot(PhysioActData, 
       aes(x=log(BurstProp), y = PercentDeltaFulton))+
  geom_point(size=3, aes(col=food))+
  geom_smooth(method=lm, col="black")+
  ylim(100, 175)+
  labs(x = "log(Proportion of time bursting)", y="Change in initial body condition (%)")+
  theme_bw()+
  scale_fill_manual(values = c("darkgreen", "palegreen2")) +
  scale_colour_manual(values = c("darkgreen", "palegreen2"),
                      labels = c("High food", "Low food"))+
  theme(axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 17, face = "plain"),                        
        panel.grid = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 14, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.88, 0.9),
        )+
  annotate("text",
           x= Inf, y= Inf,
           label = paste0("R²=", round(r2c,2)),
           hjust = 9, vjust=11,
           size=5)
  
       

ggplot(
  data = PhysioActData, 
  mapping = aes(x = logBurstScore, y = PercentDeltaFulton)
) +
  geom_point()+
  geom_smooth(method=lm)






ModelHeart <- lme(Heartmass ~ log(WeightMay):food,
                       method="ML",
                       random=list(fam=~1, tank=~1),
                       data = PhysioActData)



hist(resid(ModelHeart))
qqPlot(resid(ModelHeart))
summary(ModelHeart)
dd<- dredge(ModelHeart)
anova(ModelHeart)

predict.ModelHeart <- ggpredict(ModelHeart, terms = c("fatmass", "food", "greb1l"), ci.lvl = 0.95)
plot(predict.ModelHeart, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = WeightMay, y = Heartmass, col=food)
) +
  geom_point()+
  geom_smooth(method=lm)


