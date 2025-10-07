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


Data <- read_xlsx("Biologger act&physio data.xlsx") 



PhysioActData <- Data%>%  
  group_by(Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    mean_BurstScore = mean(BurstScore, na.rm = TRUE),
    WeightFeb = first(WeightFeb),
    WeightMay = first(WeightMayDXA),
    LengthFeb = first(LengthFeb),
    LengthMay = first(LengthMay),
    FultonFeb = first(FultonFeb),
    FultonMay = first(FultonMay),
    PercentDeltaFulton = first(PercentDeltaFulton),
    Tempgroup = first(Tempgroup),
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
  )  

str(PhysioActData$count_BurstScore)


## check if fat mass correlates with body size (lean mass)


ModelFat0 <- lmer(logFM ~ logLM + fam + (1 | tank),
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
  mapping = aes(x = logLM, y = logFM, col=food)
) +
  geom_point()+
  geom_smooth(method=lm)

predict.ModelFat <- ggpredict(ModelFat, terms = c("logLM", "food"), ci.lvl = 0.95)
plot(predict.ModelFat, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)


Modellean <- lme(leanmass ~ food+count_BurstScore+mean_VAR+mean_AvgEA*mean_MaxEA,
                 method="ML",
                 random=list(fam=~1, tank=~1),
                 data = PhysioActData)


hist(resid(Modellean))
qqPlot(resid(Modellean))
summary(Modellean)
dd<- dredge(Modellean)

predict.Modellean <- ggpredict(Modellean, terms = c("count_BurstScore", "mean_VAR", "mean_MaxEA","mean_AvgEA","food"), ci.lvl = 0.95)
plot(predict.Modellean, colors = c("#E96C0CFF",  "#420A68FF", "skyblue2"), line_size = 1.5)


