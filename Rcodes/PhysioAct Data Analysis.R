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



PhysioActData <- Data%>%  
  group_by(#MeasNB,
    Swimstyle,
    Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_corrAvgEA = mean(corr.AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    MaxEA = max(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = as.numeric(sum(BurstScore, na.rm = TRUE)),
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
    Swimstyle = as.factor(Swimstyle),
    
  )

ActSGRData <- PhysioActData %>%
  pivot_wider(
    id_cols = c(Biologger, SGRMass, fam, greb1l, tank, food, fatpercent, leanmass),
    names_from = Swimstyle,
    values_from = mean_AvgEA,
    names_prefix = "activity_"
  )


PhysioActData$food <- dplyr::recode(PhysioActData$food, "high-food" = "Ad Libitum",  "low-food" = "Restricted")
ActSGRData$food <- dplyr::recode(ActSGRData$food, "high-food" = "Ad Libitum",  "low-food" = "Restricted")



ggplot(data=PhysioActData, aes(y=mean_AvgEA, x=MeasNB , col=greb1l, group=Biologger))+
  facet_wrap(~tank)+
  geom_point()+
  geom_path()+
  scale_color_manual(values = c("gold2", "royalblue"))

ggplot(data=PhysioActDataSwim, aes(y=SGRMass, x=mean_AvgEA, col= greb1l))+
  facet_wrap(~Swimstyle+food)+
  geom_point()+
  geom_smooth(method=lm)+
  scale_color_manual(values = c("gold2", "royalblue"))



Burstprop <- glmmTMB(BurstProp ~ greb1l*food + (1 | tank) + (1| fam) , #<= BEST MODEL
                         family= gaussian(),
                         data = PhysioActData)

summary(Burstprop)
ggplot(data=PhysioActData, aes(y=BurstProp, x=greb1l, fill= greb1l))+
  facet_wrap(~food)+
  geom_boxplot()+
  scale_fill_manual(values = c("gold2", "royalblue"))

pairs(emmeans(Burstprop, ~ greb1l | food, type = "response"), adjust="sidak")


ActSGRCorr <- glmmTMB(fatpercent ~ (activity_Steady+activity_Dynamic)*greb1l+food + (1 | tank) + (1| fam) , #<= BEST MODEL
                         family= gaussian(),
                         data = ActSGRData)

performance(ActSGRCorr)
summary(ActSGRCorr)
drop1(ActSGRCorr)
step(ActSGRCorr)

BestActSGRCorr <- glmmTMB(SGRMass ~ activity_Steady + food + (1| tank) + (1| fam), #<= BEST MODEL
                         family= gaussian(),
                         data = ActSGRData)


E1 <- resid(BestActSGRCorr)
PhysioActData$E1 <- E1

ggplot(data=PhysioActData, aes(y=E1, x=mean_AvgEA, group = tank, col= tank))+
  geom_point()+
  xlab("AvgEA (mg)") + 
  ylab("Residuals")+ 
  geom_smooth(method = "lm", 
              se = FALSE)



summary(BestActSGRCorr)
performance(BestActSGRCorr)

SGRqr <- simulateResiduals(fittedModel = BestStdActSGRCorr, plot = FALSE)
SGRqr$scaledResiduals
plotQQunif(SGRqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)




ggplot(data=ActSGRData, aes(y=SGRMass, x=activity_Steady))+
  facet_wrap(~food)+
  geom_point()+
  geom_smooth(method="lm")+
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
        

ggplot(data=PhysioActData, aes(y=WeightFeb, x=SGRMass, col=greb1l))+
  facet_wrap(~food)+
  geom_point()
  geom_smooth(method = lm)
  
  
  
BrstPhysioActData <- PhysioActDataSwim%>%
  filter(
    Swimstyle == "Dynamic"
  )

BrstActSGRCorr <- glmmTMB(SGRMass ~ mean_AvgEA+food+ (1 | tank) + (1| fam) , #<= BEST MODEL
                         family= gaussian(),
                         data = BrstPhysioActData)

summary(BrstActSGRCorr)
performance(BrstActSGRCorr)
SGRqr <- simulateResiduals(fittedModel = BrstActSGRCorr, plot = FALSE)
SGRqr$scaledResiduals
plotQQunif(SGRqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)

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

Modellean <- lme(leanmass ~ mean_AvgEA+greb1l*food,#<= BEST MODEL
                method = "REML",
                random = list(tank=~1, fam=~1),
                data = PhysioActData,
                na.action = na.omit)

performance(Modellean)
hist(resid(Modellean))
qqPlot(resid(Modellean))
summary(Modellean)

step(Modellean)

ggplot(BrstPhysioActData, aes(y= fatpercent, x=count_BurstScore, col=greb1l))+
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

ModelFat <- lme(log(fatmass) ~ log(leanmass)+food+greb1l,
                method = "REML",
                random = list(tank=~1, fam=~1),
                data = PhysioActData,
                na.action = na.omit)

hist(resid(ModelFat))
qqPlot(resid(ModelFat))
summary(ModelFat)
anova(ModelFat)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = log(leanmass), y = log(fatmass), col=food)
) +
  geom_point()+
  geom_smooth(method=lm)


ggplot(
  data = PhysioActData, 
  mapping = aes(x = log(leanmass), y = log(fatmass), col=greb1l)
) +
  facet_wrap(~food)+
  geom_point()+
  geom_smooth(method=lm)

predict.ModelFat <- ggpredict(ModelFat, terms = c("food", "greb1l"), ci.lvl = 0.95)
plot(predict.ModelFat, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)

fat <- ggplot(
  data = predict.ModelFat, 
  mapping = aes(x = x, y = predicted, col=group)) +
  geom_point(size=6, 
             position = position_dodge(width = 0.8))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    position = position_dodge(width = 0.8)) +
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, face = "bold", margin = margin(r=15, l=15)),
        axis.text.x = element_text(size=18, margin = margin(b=50)),
        axis.text.y = element_text(size=18),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic"),
        legend.position = "none"
        
  )+
  labs(x="Food treatment", y="Relative Fat mass (g)")+
  ylim(0,50)

lean <- ggplot(
  data = PhysioActData, 
  mapping = aes(x = food, y = leanmass , fill=greb1l)
) +
  geom_boxplot()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, face = "bold", margin = margin(r=15, l=15)),
        axis.text.x = element_text(size=18, margin = margin(b=50)),
        axis.text.y = element_text(size=18),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic"),
        legend.position = "none",
        
  )+
  labs(x="Food treatment", y="Lean mass in May (g)")+
  ylim(0, 600)

plot <- plot_grid(fat, lean, 
          labels = c("A", "B"),
          label_size = 28,
          ncol = 2,
          align = "none"
          )

legend <- get_legend(fat+theme(legend.position = "right"))

legplot <- plot_grid(plot, legend,
                     ncol = 2,
                     rel_widths = c(1, 0.15))

ggdraw(legplot) +
  draw_label(
    "Food treatment",
    fontface = "bold",
    x = 0.47,
    y = 0.06,
    size = 20
  )

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


