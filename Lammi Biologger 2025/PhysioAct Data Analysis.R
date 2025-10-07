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

setwd(path.data)

Full_Data_Clean <- read_xlsx("Biologger Clean data.xlsx") 


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
  ) %>%
##Create a log column for each act parameter to be able to run GAMMS on it
  mutate(
    LogAvgEA = as.numeric(log10(`AvgEA`+0.001)),
    LogMaxEA = as.numeric(log10(`MaxEA`+0.001)),
    LogVAR = as.numeric(log10(VAR+0.76)),
  )%>%
##Add a BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
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

### Force the plots to show winter or low-temp on the left and spring or high-temp on the right
Full_Data_Out$Season <- factor(Full_Data_Out$Season, levels = c("Winter", "Spring"))
Full_Data_Out$Tempgroup <- factor(Full_Data_Out$Tempgroup, levels = c("LowTemp", "HighTemp"))

Xray_data_clean <- read_xlsx("Biologger Physio data.xlsx") 

str(Xray_data_clean$fatpercent)
str(Xray_data_clean$fatmass)


Full_Data_ActnPhysio <- Full_Data_Out %>%
  left_join(Xray_data_clean, by = "Biologger")

write_xlsx(Full_Data_ActnPhysio, paste0(path.data, "/Biologger act&physio data.xlsx"))
write_csv(Full_Data_ActnPhysio, paste0(path.data, "/Biologger act&physio data.csv"))



Data <- read_xlsx("Biologger act&physio data.xlsx") 



PhysioActData <- Data%>%  
  group_by(Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    logBurstScore = as.numeric(log(count_BurstScore)),
    BurstProp = as.numeric(100*mean(BurstScore, na.rm = TRUE)),
    WeightFeb = first(WeightFeb),
    WeightMay = first(WeightMayDXA),
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
    `Heart weight (g)` = first(`Heart weight (g)`),
    HtoBratio = first(HtoBratio),
    fatpercent = first(fatpercent),
    fatmass = first(fatmass),
    leanmass = first(leanmass),
    logFM = as.numeric(log(fatmass)),
    logLM = as.numeric(log(leanmass))
  )%>%  
  filter(
    Biologger != "UXL0552DAT",
    Biologger != "UXL0550DAT",
    Biologger != "UXL0500DAT",
    Biologger != "UXL0442DAT",
    Biologger != "UXL0438DAT",
  ) %>%
  mutate(
    greb1l = as.factor(greb1l),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
  )



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
str(PhysioActData$Tempgroup)








## check if fat mass correlates with body size (lean mass)


ModelFat <- lmer(logFM ~ logLM + fam + (1 | tank),
                 data = PhysioActData)


ModelFat <- lme(logFM ~ logLM,
             method = "REML",
             random = list(tank=~1, fam=~1),
              data = PhysioActData)

hist(resid(ModelFat))
qqPlot(resid(ModelFat))
summary(ModelFat)
anova(ModelFat)


ggplot(
  data = `PhysioActData`, 
  mapping = aes(x = leanmass, y = fatmass, col=food)
) +
  geom_point()+
  geom_smooth(method=lm)

predict.ModelFat <- ggpredict(ModelFat, terms = c("logLM", "food"), ci.lvl = 0.95)
plot(predict.ModelFat, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)



leanm_burst  <- lme(leanmass ~ count_BurstScore,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_var    <- lme(leanmass ~ mean_VAR,    random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_avgEA  <- lme(leanmass ~ mean_AvgEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
leanm_maxEA  <- lme(leanmass ~ mean_MaxEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(leanm_burst, leanm_var, leanm_avgEA, leanm_maxEA)



Modellean <- lme(logLM ~ log(WeightFeb)*food,
                method="ML",
                random=list(fam=~1, tank=~1),
                data = PhysioActData)


hist(resid(Modellean))
qqPlot(resid(Modellean))
summary(Modellean)
anova(Modellean)

predict.Modellean <- ggpredict(Modellean, terms = c("mean_MaxEA","greb1l","food"), ci.lvl = 0.95)
plot(predict.Modellean, colors = c("#E96C0CFF",  "#420A68FF", "skyblue2"), line_size = 1.5)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = log(WeightFeb), y = logLM, col=greb1l)
) +
  facet_wrap(~food)+
  geom_point()+
  geom_smooth(method=lm)


fatm_burst  <- lme(logFM ~ count_BurstScore,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
fatm_var    <- lme(logFM ~ mean_VAR,    random = list(fam = ~1, tank = ~1), data = PhysioActData)
fatm_avgEA  <- lme(logFM ~ mean_AvgEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
fatm_maxEA  <- lme(logFM ~ mean_MaxEA,  random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(fatm_burst, fatm_var, fatm_avgEA, fatm_maxEA)



ModelFatAct <- lme(log(fatpercent) ~ logLM + log(count_BurstScore),
                   method="ML",
                   random=list(fam=~1, tank=~1),
                   data = PhysioActData)


hist(resid(ModelFatAct))
qqPlot(resid(ModelFatAct))
summary(ModelFatAct)
anova(ModelFatAct)
dd<- dredge(ModelFatAct, fixed = "logLM")


predict.ModelFatAct <- ggpredict(ModelFatAct, terms = c("logLM", "food"), ci.lvl = 0.95)
plot(predict.ModelFatAct, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)









Fultonm_burst  <- lme(PercentDeltaFulton ~ count_BurstScore,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_var    <- lme(PercentDeltaFulton ~ BurstProp,    random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_avgEA  <- lme(PercentDeltaFulton ~ log(count_BurstScore),  random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_maxEA  <- lme(PercentDeltaFulton ~ log(BurstProp),  random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(Fultonm_burst, Fultonm_var, Fultonm_avgEA, Fultonm_maxEA)

Fultonm_lean  <- lme(PercentDeltaFulton ~ leanmass,  random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_fat    <- lme(PercentDeltaFulton ~ fatmass,    random = list(fam = ~1, tank = ~1), data = PhysioActData)
Fultonm_fatPer    <- lme(PercentDeltaFulton ~ fatpercent,    random = list(fam = ~1, tank = ~1), data = PhysioActData)

AIC(Fultonm_lean, Fultonm_fat,Fultonm_fatPer)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = log(BurstProp), y = fatpercent, col=food)
) +
  geom_point()+
  geom_smooth(method=lm)




ggplot(
  data = PhysioActData, 
  mapping = aes(x = logFM, y = PercentDeltaFulton, col=greb1l)
) +
  facet_wrap(~food)+
  geom_point()+
  geom_smooth(method=lm)


ModelFulton <- lme(PercentDeltaFulton ~ food*log(BurstProp)+greb1l,
                   method="ML",
                   random=list(fam=~1, tank=~1),
                   data = PhysioActData)


hist(resid(ModelFulton))
qqPlot(resid(ModelFulton))
summary(ModelFulton)
anova(ModelFulton)
dd<- dredge(ModelFulton)


BestModelFulton <- lme(PercentDeltaFulton ~ log(BurstProp)+food, #best model
                       method="REML",
                       random=list(fam=~1, tank=~1),
                       data = PhysioActData)



hist(resid(BestModelFulton))
qqPlot(resid(ModeBestModelFultonlFulton))
summary(BestModelFulton)
dd<- dredge(BestModelFulton)
anova(BestModelFulton)

r2 <- r.squaredGLMM(BestModelFulton)
r2c <- r2[1, "R2c"]


predict.ModelFulton <- ggpredict(BestModelFulton, terms = c("BurstProp", "food"), ci.lvl = 0.95)
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






ModelHeart <- lme(log(fatpercent) ~ log(leanmass)+food*greb1l,
                       method="ML",
                       random=list(fam=~1, tank=~1),
                       data = PhysioActData)



hist(resid(ModelHeart))
qqPlot(resid(ModelHeart))
summary(ModelHeart)
dd<- dredge(ModelHeart, fixed="fatmass")
anova(ModelHeart)

predict.ModelHeart <- ggpredict(ModelHeart, terms = c("fatmass", "food", "greb1l"), ci.lvl = 0.95)
plot(predict.ModelHeart, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = count_BurstScore, y = HtoBratio, col=food)
) +
  geom_point()+
  geom_smooth(method=lm)


