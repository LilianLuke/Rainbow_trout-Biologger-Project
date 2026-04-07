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
    Time = lubridate::hms(Time),
    BurstScore = as.numeric(BurstScore),
    Swimstyle = as.factor(Swimstyle),
    Season = as.factor(Season),
    Light = as.factor(Light),
    feeders = as.factor(feeders),
    fatmass = as.numeric(fatmass),
    fatpercent = as.numeric(fatpercent),
    WeightMay = as.numeric(WeightMay),
    WeightMayDXA = as.numeric(WeightMayDXA),
    NOAA_Sunrise = lubridate::hms(NOAA_Sunrise),
    NOAA_Sunset = lubridate::hms(NOAA_Sunset),
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
    Time = hms::hms(Time),
  )


## make sure factors are read correctly
str(Full_Data_ActnPhysio)


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
    Time = lubridate::hms(Time),
    BurstScore = as.numeric(BurstScore),
    Swimstyle = as.factor(Swimstyle),
    Season = as.factor(Season),
    Light = as.factor(Light),
    feeders = as.factor(feeders),
    fatmass = as.numeric(fatmass),
    fatpercent = as.numeric(fatpercent),
    WeightMay = as.numeric(WeightMay),
    WeightMayDXA = as.numeric(WeightMayDXA),
    NOAA_Sunrise = lubridate::hms(NOAA_Sunrise),
    NOAA_Sunset = lubridate::hms(NOAA_Sunset),
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
    Time = hms::hms(Time),
  )



str(Data)


Data$Season <- factor(Data$Season, levels = c("Winter", "Spring"))


PhysioActDataSwim <- Data%>%  
  group_by(#MeasNB,
    Swimstyle,
    Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_corrAvgEA = mean(corr.AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    MaxEA = max(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    BurstProp = as.numeric(100*mean(BurstScore, na.rm = TRUE)),
    WeightFeb = first(WeightFeb),
    WeightMay = first(WeightMay),
    LengthFeb = first(LengthFeb),
    LengthMay = first(LengthMay),
    FultonFeb = first(FultonFeb),
    FultonMay = first(FultonMay),
    SGRMass = first(SGRMass),
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
    Biologger != "UXL0471DAT", ## This fish was shorter and lighter in may than in february, and I noted it was infected with flavio, so I exlcuded it from the measurements
  ) %>%
  mutate(
    greb1l = as.factor(greb1l),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
  )


PhysioActDataSwim$food <- dplyr::recode(PhysioActDataSwim$food, "high-food" = "Ad Libitum",  "low-food" = "Restricted")


ggplot(data=PhysioActData, aes(y=mean_AvgEA, x=MeasNB , col=greb1l, group=Biologger))+
  facet_wrap(~tank)+
  geom_point()+
  geom_path()+
  scale_color_manual(values = c("gold2", "royalblue"))



ActBodyCorr <- glmmTMB(SGRMass ~ mean_AvgEA+food + (1| tank)+ (1| fam), # <= best model 
                       family= gaussian(),
                       data = PhysioActDataSwim)

performance(ActBodyCorr)
hist(resid(ActBodyCorr))
qqPlot(resid(ActBodyCorr))
summary(ActBodyCorr)



ggplot(data=PhysioActData, aes(y=mean_AvgEA, x=food, fill= greb1l))+
  geom_boxplot()+
  geom_smooth(method=lm)+
  scale_fill_manual(values = c("gold2", "royalblue"))



ggplot(data=PhysioActData, aes(y=SGRMass, x=mean_AvgEA, col= greb1l))+
  facet_wrap(~Swimstyle+food)+
  geom_point()+
  geom_smooth(method=lm)+
  scale_color_manual(values = c("gold2", "royalblue"))

ggplot(data=PhysioActData23, aes(y=SGRMass, x=mean_AvgEA, col= greb1l))+
  geom_point()+
  geom_smooth(method=lm)+
  scale_color_manual(values = c("gold2", "royalblue"))


StdPhysioActData <- PhysioActDataSwim%>%
  filter(
    Swimstyle == "Steady"
  )

StdActSGRCorr <- glmmTMB(SGRMass ~ mean_AvgEA+food + (1 | tank) + (1| fam) , #<= BEST MODEL
                      family= gaussian(),
                      data = StdPhysioActData)

summary(StdActSGRCorr)
performance(StdActSGRCorr)

ggplot(data=StdPhysioActData, aes(y=SGRMass, x=mean_AvgEA))+
  facet_wrap(~food)+
  geom_point(aes(col=greb1l))+
  geom_smooth(method=lm, col="black")+
  scale_color_manual(values = c("gold2", "royalblue"))+
  theme_bw(base_size = 16)+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14,face = "italic"))+
  labs(x="Individual average steady swimming AvgEA", y="Specific growth rate (%/day)")+
  xlim(0,8)
        


BrstPhysioActData <- PhysioActDataSwim%>%
  filter(
    Swimstyle == "Dynamic"
  )

BrstActSGRCorr <- glmmTMB(SGRMass ~ mean_AvgEA+food+ (1 | tank) + (1| fam) , #<= BEST MODEL
                         family= gaussian(),
                         data = BrstPhysioActData)

summary(BrstActSGRCorr)
performance(BrstActSGRCorr)


ggplot(data=BrstPhysioActData, aes(y=SGRMass, x=mean_AvgEA))+
  facet_wrap(~food)+
  geom_point(aes(col=greb1l))+
  geom_smooth(method=lm, col="black")+
  scale_color_manual(values = c("gold2", "royalblue"))+
  theme_bw(base_size = 16)+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14,face = "italic"))+
  labs(x = "Individual average burst swimming AvgEA", y="Specific growth rate (%/day)")



ActSGRCorr <- glmmTMB(SGRMass ~ mean_AvgEA*Swimstyle+ (1 | Biologger) , #<= BEST MODEL
                       family= gaussian(),
                       data = PhysioActData)

summary(ActSGRCorr)
performance(ActSGRCorr)



SteadyActLF <- SteadyActData %>%
  filter(
    food == "low-food"
  )

PhysioActNight <- PhysioActData %>%
  filter(
    Light == "Night"
  )


ggplot(
  data = SteadyActLF, 
  mapping = aes(x = WeightFeb , y = mean_AvgEA)
) +
  facet_wrap(~food)+
  geom_point(aes(col=greb1l))+
  geom_smooth(method="lm")


  
  
ActBodyCorr <- glmmTMB(mean_AvgEA ~ WeightFeb+food*greb1l, #<= BEST MODEL
                     family= gaussian(),
                     data = PhysioActData)

summary(ActBodyCorr)
performance(ActBodyCorr)

predict.ActBodyCorr <- ggpredict(ActBodyCorr, terms = c("WeightFeb"))

ggplot(predict.ActBodyCorr, aes(y= predicted, x=x))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))


ggplot(
  data = PhysioActData, 
  mapping = aes(x = MeasNB , y = log(mean_AvgEA), col=greb1l, group=greb1l)
) +
  facet_wrap(~food)+
  geom_point()+
  #geom_path(aes(group=Biologger))+
  geom_smooth()+
  scale_fill_manual(values = c("gold2", "royalblue"))+
  scale_color_manual(values = c("gold2", "royalblue"))

 ggplot(
  data = PhysioActData, 
  mapping = aes(x = greb1l , y = mean_VAR, fill=greb1l)
) +
  facet_wrap(~food)+
  geom_boxplot()+
  scale_fill_manual(values = c("gold2", "royalblue"))+
  scale_color_manual(values = c("gold2", "royalblue"))


ggplot(
  data = PhysioActDayta, 
  mapping = aes(x = greb1l , y = mean_VAR, fill=greb1l)
) +
  facet_wrap(~food)+
  geom_boxplot()+
  scale_fill_manual(values = c("gold2", "royalblue"))+
  scale_color_manual(values = c("gold2", "royalblue"))




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
str(PhysioActData)
n_distinct(PhysioActData$Biologger)

## test if greb, food or activity affects lean mass


Modellean <- glmmTMB(leanmass ~ log(mean_AvgEA)*food + (1 | tank) + (1| fam), #<= BEST MODEL (actually, best model does not include mean AvgEA but we want to show that p-value)
                     family = gaussian(),
                     data = PhysioActData)

performance(Modellean)
hist(resid(Modellean))
qqPlot(resid(Modellean))
summary(Modellean)

ggplot(PhysioActData, aes(y= leanmass, log(mean_AvgEA), col=greb1l))+
  facet_wrap(~food)+
  geom_point()+
  geom_smooth(method="lm")

ggplot(
  data = PhysioActData, 
  mapping = aes(x = mean_AvgEA, y = leanmass, col=greb1l)
) +
  facet_wrap(~food)+
  geom_point()+
  geom_smooth(method = "lm")


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


ggplot(
  data = PhysioActData, 
  mapping = aes(x = food, y = fatpercent , fill=greb1l)
) +
  geom_boxplot()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("gold2", "royalblue"), name = "greb1l", labels = c("Summer", "Winter"))+
  scale_x_discrete(name = "Food Treatment", labels = c("High Food", "Low Food"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14,face = "italic")
        
  )+
  labs(y="Fat content in May (%)")+
  ylim(0,17)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = food, y = leanmass , fill=greb1l)
) +
  geom_boxplot()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("gold2", "royalblue"), name = "greb1l", labels = c("Summer", "Winter"))+
  scale_x_discrete(name = "Food Treatment", labels = c("High Food", "Low Food"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14,face = "italic")
        
  )+
  labs(y="Lean mass in May (g)")+
  ylim(0, 600)

### <= greb1l effect not significant in the GLMM, but looks like something is going on in the low food group

ModelMassAct <- glmmTMB(leanmass ~(mean_AvgEA+food) * greb1l + (1|tank) + (1|fam) , # <= Best Model 
                       family = gaussian(),
                       data = PhysioActData)

summary(ModelMassAct)


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



BestModelFulton <- glmmTMB(log(LengthMay) ~ log(meanAvgEA)+greb1l + (1|fam) + (1| tank), #best model ?
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


