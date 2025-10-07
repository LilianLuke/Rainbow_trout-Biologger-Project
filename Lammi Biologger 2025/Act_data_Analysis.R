## This script is for analysing the raw activity data, separating winter and spring data

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




## make sure factors are read correctly
str(Full_Data_Out$Biologger)
str(Full_Data_Out$fam)
str(Full_Data_Out$greb1l)
str(Full_Data_Out$omy05)
str(Full_Data_Out$tank)
str(Full_Data_Out$Date)
str(Full_Data_Out$Time)
str(Full_Data_Out$DateTime)





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
  ) %>%
  filter(
    Biologger != "UXL0471DAT"
  )

str(Full_Data_Out$BurstScore)
str(Full_Data_Out$Swimstyle)
str(Full_Data_Out$Tempgroup)

### Force the plots to show winter or low-temp on the left and spring or high-temp on the right
Full_Data_Out$Season <- factor(Full_Data_Out$Season, levels = c("Winter", "Spring"))
Full_Data_Out$Tempgroup <- factor(Full_Data_Out$Tempgroup, levels = c("LowTemp", "HighTemp"))


### Create one df for each season
Full_Data_Spring <- Full_Data_Out %>%
  filter(
    Season == "Spring"
    )

Full_Data_Winter <- Full_Data_Out %>%
  filter(
    Season == "Winter"
  )

### Create one df for each swimstyle

BaselineData <- Full_Data_Out%>%
  filter(
    Swimstyle == "Steady"
  )

FilteredData <- Full_Data_Out%>%
  filter(
    Swimstyle == "Burst",
  )


### TO analyse burst data, use the count data (BurstScore)
### Create a dataset with burst frequency within temperature groups 
### (cannot use count because there are more measurements in lowtemp group, but because BurstScore is either 0 or 1, mean = frequency and is independant of the number of measurements)
TempBurst <- Full_Data_Out%>%  
  group_by(Tempgroup, Biologger) %>%
  summarise(
    mean_BurstScore = as.numeric(100*mean(BurstScore, na.rm = TRUE)),
    logBurstProp = as.numeric(log(mean_BurstScore)),
    Month = first(Month),
    Biologger = first(Biologger),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    Season = first(Season),
    LengthFeb = first(LengthFeb),
    WeightFeb = first(WeightFeb)
  )

str(TempBurst$Biologger)


CountBurstMod <- lme(mean_BurstScore ~ Tempgroup*greb1l*food, ##best model
                 method="ML",
                 random=list(fam=~1, tank=~1),
                 data = TempBurst)

dredge(CountBurstMod)
summary(CountBurstMod)
anova(CountBurstMod)

hist(resid(CountBurstMod))
qqPlot(resid(CountBurstMod))
summary(CountBurstMod)

EMM <- emmeans(CountBurstMod, ~greb1l:food:Tempgroup, data=TempBurst) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp, adjust="none")



predict.CountBurstMod <- ggpredict(CountBurstMod, terms = c("Tempgroup","food","greb1l"), ci.lvl = 0.95)


predict.CountBurstMod$x <- dplyr::recode(predict.CountBurstMod$x, "LowTemp" = "Water < 4.5°C",  "HighTemp" = "Water > 4.5°C")
predict.CountBurstMod$group <- dplyr::recode(predict.CountBurstMod$group, "high-food" = "High Food",  "low-food" = "Low Food")
predict.CountBurstMod$facet <- dplyr::recode(predict.CountBurstMod$facet, "Allele 1" = "Summer",  "Allele 2" = "Winter")
colnames(predict.CountBurstMod)[6] <- "Food"
colnames(predict.CountBurstMod)[7] <- "greb1l"

burst.plot <- ggplot(predict.CountBurstMod, aes(y= predicted, x=x, group = greb1l))+
  facet_grid(~Food)+
  geom_point(position = position_dodge(.4), size= 6, aes(col=greb1l))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, col=greb1l), position = position_dodge(.4),
                width=0.3) +
  scale_color_manual(values = c("gold2", "royalblue"))+
  xlab(expression(bold("Temperature")))+
  ylab(expression(bold("Predicted proportion of burst swimming (95% C.I.)")))+
  ylim(c(0, 6))+
  theme_bw()+
  theme(panel.spacing.x=unit(0, "lines"),
        plot.title = element_text(size =18),
        plot.subtitle =  element_text(size =17),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size=16),
        axis.text.x = element_text(size=15, vjust=0.6),
        panel.grid.minor =element_blank(),
        panel.grid.major=element_blank(),
        strip.text.x = element_text(size = 18, face="italic"),
        legend.position = c(0.1, 0.9) ,
        legend.box.spacing = unit(0, "lines"),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17, face="italic")
  )

burst.plot




ggplot(TempBurst, 
       aes(x=LengthFeb, y = mean_BurstScore, col=greb1l))+
  facet_wrap(~food)+
  #geom_boxplot(aes(fill=greb1l))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="", y="Proportion of time bursting (%)")+
  theme_bw()+
  scale_color_manual(values = c("gold2", "royalblue"),
                    labels = c("Summer runners", "Winter runners")) +
  theme(axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 17, face = "plain"),                        
        panel.grid = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 19, face = "italic"),
        legend.title = element_blank(),
        legend.position = c(0.17,0.9),
  )    



ggplot(TempBurst, 
       aes(x=food, y = mean_BurstScore))+
  facet_wrap(~Tempgroup)+
  geom_boxplot(aes(fill=greb1l))+
  ylim(0, 9)+
  labs(x="", y="Proportion of burst swimming (%)")+
  theme_bw()+
  scale_fill_manual(values = c("gold2", "royalblue"),
                    labels = c("Summer", "Winter")) +
  theme(axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 17, face = "plain"),                        
        panel.grid = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text = element_text(size = 14, face = "italic"),
        legend.title = element_text(size = 17, face = "italic"),
        legend.position = c(0.17,0.9),
  )    






ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = BurstScore),
  ) +
  facet_wrap(Full_Data_Out$Biologger)+
  geom_point(alpha=.5)+
  geom_smooth()


### Maybe try to analyse the differences in burst power? Look at MaxEA using Filtered Data


### Analyse Burst&coast swim data 


FilteredDataGAMM <- FilteredData%>%
  mutate(
    Time = as.numeric(Time),
    DateTime = as.numeric(DateTime),
  )


## For MaxEA, Gamma is best but q-q plot looks weird

ACTmodelMax_F <- gamm(LogMaxEA~ s(Time)+WeightFeb:food+(Tempgroup+greb1l)*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = FilteredDataGAMM)
hist(resid(ACTmodelMax_F$gam))
qqPlot(resid(ACTmodelMax_F$gam))
summary(ACTmodelMax_F$gam)
anova.gam(ACTmodelMax_F$gam)

predict.MaxEA <- ggpredict(ACTmodelMax_F, terms = c("Tempgroup", "greb1l","food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.MaxEA)


ggplot(
  data = FilteredData, 
  mapping = aes(x = Tempgroup, y = LogMaxEA),
) +
  facet_wrap(~food)+
  geom_boxplot(aes(fill=greb1l))




## Then, analyse baseline swimming activity (use Avg, maybe VAR)

### Convert time to numeric for the gamms

BaselineDataGAMM <- BaselineData%>%
  mutate(
    Time = as.numeric(Time),
    DateTime = as.numeric(DateTime),
  )

### DO NOT INCLUDE DATE AS RANDOM, IT MAKES R CRASH

## For AvgEA, gaussian is okay for log data but Gamma on raw data looks better (q-q plot-wise) => keep Gamma on raw

ACTmodelAvg_B <- gamm(AvgEA~ s(Time) + scale(Temp)*greb1l*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

ACTmodelAvg_B2 <- gamm(AvgEA~ s(Time) + Tempgroup*greb1l*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

ACTmodelAvg_B3 <- gamm(AvgEA~ s(Time)+ Tempgroup*greb1l*food,  ### BEST MODEL
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                       data = BaselineDataGAMM)

ACTmodelAvg_B4 <- gamm(AvgEA~ s(Time)+scale(Temp)+greb1l*food, 
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                       data = BaselineDataGAMM)

hist(resid(ACTmodelAvg_B2$gam))
qqPlot(resid(ACTmodelAvg_B2$gam))
summary(ACTmodelAvg_B3$gam)
anova.gam(ACTmodelAvg_B3$gam)

MuMIn::AICc(ACTmodelAvg_B2)

EMM <- emmeans(ACTmodelAvg_B, ~food*greb1l, data=BaselineData, at = list(Temp = c(2, 4, 6))) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)


predict.AvgEA <- ggpredict(ACTmodelAvg_B3, terms = c("Tempgroup","greb1l", "food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.AvgEA)






 ## For MaxEA, Gamma is best but q-q plot looks weird

ACTmodelMax_B <- gamm(LogMaxEA~ s(Time)+scale(LengthFeb)+scale(Temp)*greb1l*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

ACTmodelMax_B2 <- gamm(LogMaxEA~ s(Time)+scale(Temp)*(greb1l+food), 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

ACTmodelMax_B3 <- gamm(LogMaxEA~ s(Time)+scale(LengthFeb):food+Tempgroup*greb1l*food, 
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                       data = BaselineDataGAMM)


hist(resid(ACTmodelMax_B3$gam))
qqPlot(resid(ACTmodelMax_B3$gam))

summary(ACTmodelMax_B3$gam)
anova.gam(ACTmodelMax_B3$gam)

MuMIn::AICc(ACTmodelMax_B3)




EMM <- emmeans(ACTmodelMax_F, ~Temp*food*greb1l, data=BaselineData, at = list(Temp = c(2, 4, 6))) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)


predict.MaxEA <- ggpredict(ACTmodelMax_B3, terms = c("Tempgroup","greb1l", "food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.MaxEA)



 ## For VAR, Gamma on log is best, q-q plot looks alright-ish

ACTmodelVar_B <- gamm(VAR~ s(Time)+Tempgroup*greb1l*food, 
                      random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma,
                      data = BaselineDataGAMM)

hist(resid(ACTmodelVar_B$gam))
qqPlot(resid(ACTmodelVar_B$gam))
summary(ACTmodelVar_B$gam)
anova.gam(ACTmodelVar_B$gam)
EMM <- emmeans(ACTmodelVar_B, ~Temp*greb1l*food, data=BaselineData, at = list(Temp=c(2, 4, 6))) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)


predict.VAR <- ggpredict(ACTmodelVar_B, terms = c("Tempgroup","greb1l","food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.VAR)



 CorTestBL<- lme(LogVAR ~ LogMaxEA,
                method="ML",
                random=~1|fam/tank,
                data = BaselineData)
qqPlot(resid(CorTestBL))
summary(CorTestBL)



