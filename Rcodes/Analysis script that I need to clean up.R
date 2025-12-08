## This script is for analysing biologger data... and doing stuff ?

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
  ## Create a DateTime column to plot all data over the measurement period
  mutate(
    Date = ymd(Date),
    Time = hms(Time),
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




ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = hms::hms(Time), y = Temp, colour=Date),
) +
  geom_point() 


ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = MaxEA, y = AvgEA),
) +
  facet_wrap(~Tempgroup+Season)+
  geom_point()+
  geom_smooth()






ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = `DateTime`, y = Temp),
) +
  geom_point()+
  geom_smooth()



Full_Data_Spring_Test <- Full_Data_Spring %>%
  mutate(
    Avgcor = AvgEA - MaxEA/60,
    VARcor = VAR - ((MaxEA-AvgEA)^2)/60
  )

### Test difference of activity between seasons
dfline <- data.frame(
  x = seq(0, 3500, length.out = 500)
  )
dfline3 <- data.frame(
  x = seq(0, 3500, length.out = 500)
)
dfline$y <- dfline$x^2 / 60
dfline3$y <- dfline3$x^2 / 30


ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = MaxEA, y = VAR),
) +
  geom_point(alpha=.5)+
  geom_line(data = dfline, aes(x = x, y = y), color = "red", linewidth = 1)+
  geom_line(data = dfline3, aes(x = x, y = y), color = "blue", linewidth = 1)




dfline$y <- dfline$x / 60

setwd(path.data)



#calculate mean and sd of points by team
BaselineData_mean_sd <- BaselineData %>%
  group_by(Season) %>%
  summarise_at(vars(VAR), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

#view results
BaselineData_mean_sd

ggplot(
  data = BaselineData_mean_sd, 
  mapping = aes(x = Temp , y = mean, color = Season),
) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.3) +
  geom_point(size=2)

ggplot(
  data = FilteredData, 
  mapping = aes(x = DateTime, y = MaxEA),
) +
  geom_point(alpha=.1)


ggplot(
  data = BaselineData, 
  mapping = aes(x = Temp, y = AvgEA),
) +
  geom_point(alpha=.5)+
  geom_smooth(method = lm)


ggplot(
  data = BaselineData, 
  mapping = aes(x = Temp, y = MaxEA),
) +
  geom_point(alpha=.5)+
  geom_smooth()


ggplot(
  data = BaselineData, 
  mapping = aes(x = Temp, y = VAR),
) +
  geom_point(alpha=.5)+
  geom_smooth(method = lm)

 
LTBaseline <- BaselineData %>%
  filter(
    Temp < 4.5
  )


ggplot(
  data = LTBaseline, 
  mapping = aes(x = Temp, y = VAR),
) +
  geom_point(alpha=.5)+
  geom_smooth(method = lm)



dfline2 <- data.frame(dfline2VAR <- data.frame(
  x = seq(0, 200000, length.out = 500)
)
dfline2$y <- (dfline2$x *60)^2 /60
dfline2$y <- sqrt(dfline2$x *60) /60

ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = VAR, y = AvgEA),
) +
  geom_point(alpha=.5)+
  geom_line(data = dfline2, aes(x = x, y = y), color = "red", linewidth = 1)








##Plot Average Activity data 

##All the data, then turn on the facet_wrap to split between families and food treatments
ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = `greb1l`, y = log(`AvgEA(mg)`), colour= greb1l),
) +
  #facet_wrap(~ food+fam)+
  facet_wrap(~ food+Season)+
  geom_point(alpha=.1) 
#geom_smooth() +
#ylim(0, 100)

ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = `DateTime`, y = log(`VAR`),  colour=greb1l),
) +
  #facet_wrap(~ food+fam)+
  geom_point(alpha=.5) +
  geom_smooth() 




##Plot Maximum Activity data 

##All the data, then turn on the facet_wrap to split between families and food treatments


ggplot(
  data = Full_Data, 
  mapping = aes(x = `DateTime`, y = log(`MaxEA(mg)`+0.5),  colour=greb1l),
) +
  #facet_wrap(~ food+fam)+
  geom_point(alpha=.5) +
  geom_smooth() 

ggplot(
  data = Full_Data, 
  mapping = aes(x = `DateTime`, y = `MaxEA(mg)`,  colour=greb1l),
) +
  #facet_wrap(~ food+fam)+
  geom_point(alpha=.5) +
  geom_smooth() +
  #ylim(0, 3000) nothing visible, zoom in
  ylim(0, 150)


### Create a log column for each act parameter to be able to run GAMMS on it
Full_Data_Out <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    LogAvgEA = as.numeric(log10(`AvgEA`+0.001)),
    LogMaxEA = as.numeric(log10(`MaxEA`+0.001)),
    LogVAR = as.numeric(log10(VAR+0.001)),
  )
  
  
  
  
### Check if the time of measurements start (midnight or noon) affects activity data
  ggplot(
    data = Full_Data_Out, 
    mapping = aes(x = DayTime, y = LogAvgEA, color=Mtype),
  ) +
  facet_wrap(~food)+
  geom_boxplot()


modelAvg <- lme(LogAvgEA~ Season*DayTime*Mtype*food, 
                method="ML",
                random=~1|Biologger,
                data = Full_Data_Out)
dd <- dredge(modelAvg)
modav <- model.avg(dd,delta<3)
summary(modav)


BestmodelAvg <- lme(LogAvgEA~ DayTime*Mtype*food*Season, 
                    method="ML",
                    random=~1|Biologger/fam/tank,
                    data = Full_Data_Out)
summary(BestmodelAvg)


summary(modelAvg)
hist(resid(BestmodelAvg))
qqPlot(resid(BestmodelAvg))



ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DayTime, y = AvgEA),
) +
  facet_wrap(~food+Season)+
  geom_boxplot(aes(fill=Mtype))+
  geom_smooth(method="lm")


##Add a BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
Full_Data_Out <- Full_Data_Out %>%
  mutate(
    BurstScore = as.factor(if_else(
      VAR < 220,
      "steady",
      "burst",
    ))
  ) 


### Force the plots to show winter on the left and spring on the right
Full_Data_Out$Season <- factor(Full_Data_Out$Season, levels = c("Winter", "Spring"))


### TEST  THE RELIABILITY OF THE DATA, PLAY AROUND, SEE WHAT IS WHAT


ggplot(
  data = TestFilter, 
  mapping = aes(x = MaxEA, y = AvgEA),
) +
  geom_point(alpha = .2)+
  geom_smooth()


TestFilter <- Full_Data_Out %>%
  filter(
    VAR > ((MaxEA^2)+59*(4^2))/60,
  )

ggplot(
  data = BaselineData, 
  mapping = aes(x = VAR, y = AvgEA),
) +
  geom_point(alpha = .2)+
  geom_smooth()

ggplot(
  data = FilteredData, 
  mapping = aes(x = VAR, y = AvgEA),
) +
  geom_point(alpha = .2)+
  geom_smooth()


### Split the data in 2 to separate steady swimming from bursts based on Zrini & Gamperl VAR=220

BaselineDataMerge <- BaselineData%>%
  mutate(
    AvgEA_B = AvgEA,
    MaxEA_B = MaxEA,
    VAR_B = VAR,
  ) %>%
  select(Biologger, Number, AvgEA_B, MaxEA_B, VAR_B)

FilteredDataMerge <- FilteredData%>%
  mutate(
    AvgEA_F = AvgEA,
    MaxEA_F = MaxEA,
    VAR_F = VAR,
  )%>%
  select(Biologger, Number, AvgEA_F, MaxEA_F, VAR_F)



Full_Data_Out_Merged <- Full_Data_Out %>%
  left_join(BaselineDataMerge, by = c("Biologger", "Number")) %>%
  left_join(FilteredDataMerge, by = c("Biologger", "Number"))%>%
  summarise(
    mean_AvgEA = mean(AvgEA[Season=="Spring"], na.rm = TRUE),
    mean_MaxEA = mean(MaxEA[Season=="Spring"], na.rm = TRUE),
    mean_VAR = mean(VAR[Season=="Spring"], na.rm = TRUE),
    mean_AvgEA_B = mean(AvgEA_B[Season=="Spring"], na.rm = TRUE),
    mean_MaxEA_B = mean(MaxEA_B[Season=="Spring"], na.rm = TRUE),
    mean_VAR_B = mean(VAR_B[Season=="Spring"], na.rm = TRUE),
    mean_AvgEA_F = mean(AvgEA_F[Season=="Spring"], na.rm = TRUE),
    mean_MaxEA_F = mean(MaxEA_F[Season=="Spring"], na.rm = TRUE),
    mean_VAR_F = mean(VAR_F[Season=="Spring"], na.rm = TRUE),
    PercentDeltaFulton = first(PercentDeltaFulton),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    SGRMass = first(SGRMass),
    `Heart weight (g)` = first(`Heart weight (g)`),
    HtoBratio = first(HtoBratio),
  )




ACTmodelAvg_F <- lme(mean_AvgEA_F~ mean_AvgEA_B*greb1l*food,
                     random=list(fam=~1, tank=~1),
                     data = Full_Data_Out_Merged) 
hist(resid(ACTmodelAvg_F))
qqPlot(resid(ACTmodelAvg_F))
summary(ACTmodelAvg_F)
anova.gam(ACTmodelAvg_F)

predict.AvgEA <- ggpredict(ACTmodelAvg_F, terms = c("mean_AvgEA_B","greb1l"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.AvgEA)


ggplot(
  data = FilteredData, 
  mapping = aes(x = MaxEA, y = log(VAR), color = Biologger)
) +
  theme(legend.position = "none")+
  geom_point(alpha=.5)





ggplot(
  data = Full_Data_Out_Merged, 
  mapping = aes(x = mean_MaxEA_B, y = mean_MaxEA_F ,  colour=greb1l),
) +
  facet_wrap(~ food)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")

ggplot(
  data = Full_Data_Out_Merged, 
  mapping = aes(x = mean_VAR_B, y = mean_VAR_F ,  colour=greb1l),
) +
  facet_wrap(~ food)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")

### Look at the histograms to see if still  needs log transformation

hist(FilteredData$AvgEA, breaks = 100)
hist(FilteredData$LogMaxEA, breaks = 100)
hist(FilteredData$LogVAR, breaks = 1000)


hist(BaselineData$AvgEA, breaks = 50)
hist(BaselineData$LogMaxEA, breaks = 100)
hist(BaselineData$LogVAR, breaks = 1000)

### Plot raw data first
ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = `AvgEA`,  colour=greb1l),
) +
  facet_wrap(~ food+Season)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")+
  ylim(0, 30)

ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = `VAR`,  colour=greb1l),
) +
  facet_wrap(~ food+Season)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")+
  ylim(0, 150)

ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = `MaxEA`,  colour=greb1l),
) +
  facet_wrap(~ food+Season)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")+
  ylim(0, 50)



### plot log transformed data next
ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = log(`AvgEA(mg)`),  colour=greb1l),
) +
  facet_wrap(~ food+Season)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")

ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = log(`VAR`),  colour=greb1l),
) +
  facet_wrap(~ food+Season)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")

ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = log(`MaxEA(mg)`),  colour=greb1l),
) +
  facet_wrap(~ food+Season)+
  geom_point(alpha=.1) +
  geom_smooth(method = "lm")



### Look at body parameters to see if there are any visual differences
PhysioData <- Full_Data_Out%>%
  filter(
    Number == 1
  )





ggplot(
  data = `BaselineData`, 
  mapping = aes(x = greb1l, y = PercentDeltaFulton),
) +
  facet_wrap(~food)+
  geom_boxplot(aes(fill=greb1l))



ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = DateTime, y = LogAvgEA, colour = greb1l),
) +
  facet_wrap(~food)+
  geom_point() +
  geom_smooth(method="lm")


ggplot(
  data = BaselineDataSpring, 
  mapping = aes(x = Temp, y = VAR),
) +
  facet_wrap(~Tempgroup)+
  geom_point(alpha=.01) +
  geom_smooth(method=lm)



Full_Data_Spring <- Full_Data_Out %>%
  filter(
    Season == "Spring"
  )

ggplot(
  data = Full_Data_Spring, 
  mapping = aes(x = food, y = log10(MaxEA)),
) +
  geom_boxplot(aes(fill=greb1l))



ggplot(
  data = `Full_Data_Out`, 
  mapping = aes(x = FultonFeb, y = PercentDeltaFulton),
) +
  geom_point(alpha=.2, cex=2)





### See if activity affects physiology with lme ?


any(is.na(PhysioData$PercentDeltaFulton))


modelFulton <- aov(PhysioData$PercentDeltaFulton ~ greb1l*food, 
                   data = PhysioData)

summary(modelFulton)
hist(resid(modelFulton))
qqPlot(resid(modelFulton))


ggplot(
  data = `PhysioData`, 
  mapping = aes(x = greb1l, y = SGRMass),
) +
  facet_wrap(~food)+
  geom_boxplot(aes(fill=greb1l))

ggplot(
  data = `PhysioData`, 
  mapping = aes(x = greb1l, y = PercentDeltaFulton, color=greb1l),
) +
  facet_wrap(~food)+
  geom_point(alpha=0.5)+
  stat_summary(fun = mean, geom = "point", cex=3,)

PhysioActData <- Full_Data_Out%>%  
  group_by(Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
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
  )%>%  
  filter(
    Biologger != "UXL0552DAT",
  )  

tank_count <- PhysioActData %>%
  group_by(tank, greb1l) %>%
  summarise(Count = n())

PhysioActData %>%
  summarise(n_biologgers = n_distinct(Biologger))


ggplot(
  data = `FilteredPhysioActDataSpring`, 
  mapping = aes(x = food, y = mean_VAR),
) +
  geom_boxplot(aes(fill=greb1l))


ACTmodelMax <- lme(mean_VAR~ LengthFeb*food, 
                      random = list(fam=~1, tank=~1),
                      data = PhysioActData)

hist(resid(ACTmodelVar))
qqPlot(resid(ACTmodelVar))
summary(ACTmodelVar)
anova(ACTmodelVar)
EMM <- emmeans(ACTmodelVar, ~LengthFeb*food, data=BaselineData, at = list(LengthFeb=c(250, 350))) 
EMM_resp <- regrid(EMM)
pairs(EMM_resp)


predict.VAR <- ggpredict(ACTmodelVar, terms = c("LengthFeb", "food"), ci.lvl = 0.95, bias_correction = TRUE)
plot(predict.VAR)








BaselinePhysioActData <- BaselineData%>%  
  group_by(Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    PercentDeltaFulton = first(PercentDeltaFulton),
    FultonFeb = first(FultonFeb),
    FultonMay = first(FultonMay),
    DeltaFulton = first(DeltaFulton),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    SGRMass = first(SGRMass),
    `Heart weight (g)` = first(`Heart weight (g)`),
    HtoBratio = first(HtoBratio),
    LengthFeb = first(LengthFeb),
    LengthMay = first(LengthMay)
  )


BaselinePhysioActData <- BaselinePhysioActData%>%  
  filter(
    Biologger != "UXL0552DAT"
  )

df <- PhysioActData %>%
  group_by(food) %>%
  mutate(x_centered = scale(count_BurstScore, center = TRUE, scale = TRUE))


BmodelAvg <- gamm(PercentDeltaFulton ~ s(count_BurstScore)+food,
                 method="ML",
                 random=list(fam=~1, tank=~1),
                 data = df)

plot(BmodelAvg$gam)
dd<- dredge(BmodelAvg)
modav <- model.avg(dd,delta<2)
summary(BmodelAvg)

anova(BmodelAvg)

hist(resid(BmodelAvg))
qqPlot(resid(BmodelAvg))
summary(BmodelAvg$gam)


EMM <- emmeans(BmodelAvg, ~mean_AvgEA*greb1l*food, data=PhysioActData, at = list(mean_AvgEA = c(3, 5, 7)))
pairs(EMM)

predict.B_AvgEA <- ggpredict(BmodelAvg$gam, terms = c("count_BurstScore","food"), ci.lvl = 0.95)
plot(predict.B_AvgEA, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)

ggplot(
  data = PhysioActData, 
  mapping = aes(x = count_BurstScore, y = PercentDeltaFulton),
) +
  geom_point(aes(color=food))+
  geom_smooth()


hist(PhysioActData$count_BurstScore)

BmodelMax <- lme(PercentDeltaFulton ~ greb1l*food,
                 method="ML",
                 random=~1|fam/tank,
                 data = BaselinePhysioActData)


hist(resid(BmodelMax))
qqPlot(resid(BmodelMax))
summary(BmodelMax)


 EMM <- emmeans(BmodelMax, ~mean_MaxEA*food*greb1l, data=BaselinePhysioActData) 
pairs(EMM)

predict.B_MaxEA <- ggpredict(BmodelMax, terms = c("mean_MaxEA", "food","greb1l"), ci.lvl = 0.95)
plot(predict.B_MaxEA, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)




BmodelVAR <- lme(PercentDeltaFulton ~ mean_VAR*greb1l*food,
                 method="ML",
                 random=~1|fam/tank,
                 data = BaselinePhysioActData)


hist(resid(BmodelVAR))
qqPlot(resid(BmodelVAR))
summary(BmodelVAR)


EMM <- emmeans(BmodelVAR, ~mean_VAR*food*greb1l, data=BaselinePhysioActData) 
pairs(EMM)

predict.B_VAR <- ggpredict(BmodelVAR, terms = c("mean_VAR","greb1l", "food"), ci.lvl = 0.95)
plot(predict.B_VAR, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)










FilteredPhysioActDataSpring <- FilteredData%>%  
  group_by(Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA[Season=="Spring"], na.rm = TRUE),
    mean_MaxEA = mean(MaxEA[Season=="Spring"], na.rm = TRUE),
    mean_VAR = mean(VAR[Season=="Spring"], na.rm = TRUE),
    PercentDeltaFulton = first(PercentDeltaFulton),
    FultonFeb = first(FultonFeb),
    FultonMay = first(FultonMay),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    SGRMass = first(SGRMass),
    `Heart weight (g)` = first(`Heart weight (g)`),
    HtoBratio = first(HtoBratio),
  )

FilteredPhysioActDataSpring <- FilteredPhysioActDataSpring%>%  
  filter(
    Biologger != "UXL0552DAT"
  )




FmodelAvg <- lme(PercentDeltaFulton ~ mean_AvgEA*greb1l*food,
                 method="ML",
                 random=~1|fam/tank,
                 data = FilteredPhysioActDataSpring)


hist(resid(FmodelAvg))
qqPlot(resid(FmodelAvg))
summary(FmodelAvg)


EMM <- emmeans(FmodelAvg, ~mean_AvgEA*food*greb1l, data=FilteredPhysioActDataSpring) 
pairs(EMM)

predict.F_AvgEA <- ggpredict(FmodelAvg, terms = c("mean_AvgEA","food","greb1l"), ci.lvl = 0.95)
plot(predict.F_AvgEA, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)



FmodelMax <- lme(PercentDeltaFulton ~ mean_MaxEA*greb1l*food,
                 method="ML",
                 random=~1|fam/tank/Biologger,
                 data = FilteredPhysioActDataSpring)

hist(resid(FmodelMax))
qqPlot(resid(FmodelMax))
summary(FmodelMax)

EMM <- emmeans(FmodelMax, ~mean_MaxEA*food*greb1l, data=FilteredPhysioActDataSpring) 
pairs(EMM)

predict.F_MaxEA <- ggpredict(FmodelMax, terms = c("mean_MaxEA","greb1l", "food"), ci.lvl = 0.95)
plot(predict.F_MaxEA, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)



FmodelVAR <- lme(PercentDeltaFulton ~ mean_VAR*greb1l*food,
                 method="ML",
                 random=~1|fam/tank,
                 data = PhysioActDataSpring)



hist(resid(FmodelVAR))
qqPlot(resid(FmodelVAR))
summary(FmodelVAR)

EMM <- emmeans(FmodelVAR, ~mean_VAR*food*greb1l, data=FilteredPhysioActDataSpring) 
pairs(EMM)

predict.F_VAR <- ggpredict(FmodelVAR, terms = c("mean_VAR","greb1l","food"), ci.lvl = 0.95)
plot(predict.F_VAR, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)


ggplot(
  data = `BaselinePhysioActData`, 
  mapping = aes(x = mean_VAR, y = SGRMass, color=greb1l)
) +
  facet_wrap(~food)+
  scale_colour_manual(values = c("#E96C0CFF",  "#420A68FF"))+
  geom_point(alpha=0.5)+
  geom_smooth(method = lm)



ggplot(
  data = `BaselinePhysioActDataSpring`, 
  mapping = aes(x = LengthFeb, y = mean_MaxEA, color=food)
) +
  facet_wrap(~greb1l)+
  scale_colour_manual(values = c("#E96C0CFF",  "#420A68FF"))+
  geom_point(alpha=0.5)+
  geom_smooth(method = lm)



modelAvg <- lme(PercentDeltaFulton ~ mean_MaxEA*greb1l*food,
                method="ML",
                random=~1|fam/tank,
                data = FilteredPhysioActDataSpring)



hist(resid(modelAvg))
qqPlot(resid(modelAvg))
summary(modelAvg)

EMM <- emmeans(FmodelAvg, ~mean_AvgEA*food*greb1l, data=PhysioActDataSpring) 
pairs(EMM)

predict.Avg <- ggpredict(modelAvg, terms = c("mean_VAR","food","greb1l"), ci.lvl = 0.95)
plot(predict.Avg, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)




PhysioActDataSpring

any(is.na(FilteredPhysioActDataSpring$PercentDeltaFulton))

### See link between activity and Heart size
HeartFilteredPhysioActDataSpring <- FilteredPhysioActDataSpring %>%
  filter(
    Biologger != "UXL0550DAT",
    Biologger != "UXL0438DAT",
    Biologger != "UXL0442DAT"
  )

HeartBaselinePhysioActDataSpring <- BaselinePhysioActDataSpring %>%
  filter(
    Biologger != "UXL0550DAT",
    Biologger != "UXL0438DAT",
    Biologger != "UXL0442DAT"
  )


HeartPhysioActDataSpring <- PhysioActDataSpring %>%
  filter(
    Biologger != "UXL0550DAT",
    Biologger != "UXL0438DAT",
    Biologger != "UXL0442DAT"
  )

HRTmodelAvg_B <- lme(HtoBratio ~ LengthMay+ mean_VAR*greb1l*food,
                     method="ML",
                     random=~1|fam/tank,
                     data = HeartBaselinePhysioActDataSpring)


hist(resid(HRTmodelAvg_B))
qqPlot(resid(HRTmodelAvg_B))
summary(HRTmodelAvg_B)
anova(HRTmodelAvg_B)

EMM <- emmeans(HRTmodelAvg_B, ~mean_MaxEA*food*greb1l, data=HeartBaselinePhysioActDataSpring) 
pairs(EMM)

predict.F_AvgEA <- ggpredict(HRTmodelAvg_B, terms = c("mean_VAR","greb1l","food"), ci.lvl = 0.95)
plot(predict.F_AvgEA, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)


ggplot(
  data = HeartBaselinePhysioActDataSpring, 
  mapping = aes(x = LengthMay, y = HtoBratio),
) +
  geom_point(alpha=0.5)+
  geom_smooth(method=lm)


FmodelAvg <- lme(HtoBratio ~ mean_VAR*greb1l*food,
                 method="ML",
                 random=~1|fam/tank,
                 data = HeartFilteredPhysioActDataSpring)

summary(FmodelAvg)

## Save the new dataframe to path.data
write_csv(Full_Data, paste0(path.data, "/Biologger full data.csv"))





FmodelAvg <- lme(VAR ~ LengthFeb*Temp*food,
                 method="ML",
                 random=~1|fam/tank/Biologger,
                 data = BaselineData)
summary(FmodelAvg)

predict.F_AvgEA <- ggpredict(FmodelAvg, terms = c("LengthFeb","Temp","food"), ci.lvl = 0.95)
plot(predict.F_AvgEA, colors = c("#E96C0CFF",  "#420A68FF", "#429A38FF"), line_size = 1.5)



ggplot(
  data = Full_Data_Out, 
  mapping = aes(x = food, y = deltaL, color = greb1l)
) +
  geom_boxplot()



