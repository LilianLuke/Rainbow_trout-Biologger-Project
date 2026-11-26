### This Script is for looking at the effect of activity on physiological parameters (SGR, lean mass and relative fat mass)

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
library(cowplot)
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


Full_Data <- read_xlsx(paste0(path.data, "Biologger Final data.xlsx")) ## get accelerometery data

Xray_data_clean <- read_xlsx(paste0(path.data,"Biologger Physio data.xlsx")) ## get DXA data

str(Full_Data)
str(Xray_data_clean)


ActnPhysio <- Full_Data %>% ## merge both data sets in one
  left_join(Xray_data_clean, by = "Biologger")


Full_ActnPhysio <- ActnPhysio %>% ## format all the variables 
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
str(Full_ActnPhysio)


#write_xlsx(Full_Data_ActnPhysio, paste0(path.data, "Biologger act&physio data.xlsx"))
#write_csv(Full_Data_ActnPhysio, paste0(path.data, "Biologger act&physio data.csv"))
#Full_ActnPhysio <- read_xlsx(paste0(path.data, "Biologger act&physio data.xlsx"))

Full_ActnPhysio <- Full_ActnPhysio %>%
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



str(Full_ActnPhysio)


Full_ActnPhysio$Season <- factor(Data$Season, levels = c("Winter", "Spring"))



ActnPhysio_Data <- Full_ActnPhysio%>%  ## Create a data set with 1 row for each individual, to avoid confounding individual characteristics with repeated measurements
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

ActnPhysio_Data$food <- dplyr::recode(ActnPhysio_Data$food, "high-food" = "Ad Libitum",  "low-food" = "Restricted") ## A bit fo formating for plots


ActSGRData <- ActnPhysio_Data %>%  ## Get mmean Steady swimming activity and mean Burst Swimming activity for each individuals
  pivot_wider(
    id_cols = c(Biologger, SGRMass, fam, greb1l, tank, food, fatmass, leanmass),
    names_from = Swimstyle,
    values_from = mean_AvgEA,
    names_prefix = "activity_"
  )

str(ActSGRData)

## Test if steady or burst swimming activity affect body parameters

BestActSGRCorr <- glmmTMB(SGRMass ~ activity_Steady + food + (1| tank) + (1| fam), #<= BEST MODEL
                         family= gaussian(),
                         data = ActSGRData)

drop1(BestActSGRCorr) #need to drop tank for model selection to avoid convergence problems

## Model inspection
performance(BestActSGRCorr)
hist(resid(BestActSGRCorr))
qqPlot(resid(BestActSGRCorr))

SGRqr <- simulateResiduals(fittedModel = BestActSGRCorr, plot = FALSE)
SGRqr$scaledResiduals
plotQQunif(SGRqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)

## Results
summary(BestActSGRCorr)


## Plot result
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
  labs(x="Individual average steady swimming AvgEA (mg)", y="Specific growth rate (%/day)")
        

### Quickly check that there is no effect of initial fish size on individual mean AvgEA values 
Weightcheck <- glmmTMB(mean_AvgEA ~ WeightFeb,
                     family= gaussian(),
                     data = PhysioActData)
summary(Weightcheck)
performance(Weightcheck)

Lengthcheck <- glmmTMB(mean_AvgEA ~ LengthFeb,
                       family= gaussian(),
                       data = PhysioActData)

summary(Lengthcheck)
performance(Lengthcheck)


## test if greb, food or activity affects lean mass
Modellean <- glmmTMB(leanmass ~ greb1l+food + (1| fam), #<= BEST MODEL 
                          family= gaussian(),
                          data = ActSGRData)

drop1(Modellean) #need to drop tank to get AIC for stepwise model selection 


Modelleanqr <- simulateResiduals(fittedModel = Modellean, plot = FALSE)
Modelleanqr$scaledResiduals
plotQQunif(Modelleanqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)

performance(Modellean) #need to drop tank for model performance to get all parameters
summary(Modellean)


## fat mass correlates with body size (lean mass) in a log/log relationship.
ModelFat <- glmmTMB(log(fatmass)  ~  log(leanmass) + greb1l + food + (1| tank), #<= BEST MODEL (greb1l was included in the model to show on plots, but was not retained in the best model)
                          family= gaussian(),
                          data = ActSGRData)

drop1(ModelFat) #need to drop tank for model selection to avoid convergence problems

hist(resid(ModelFat))
qqPlot(resid(ModelFat))
performance(ModelFat)
summary(ModelFat)

### The residuals of the log(fat mass)-log(lean mass) relationship should then be relative fat mass, which is what we want to show

predict.ModelFat <- ggpredict(ModelFat, terms = c("food", "greb1l"), ci.lvl = 0.95) ## extract relative fat mass from the model

## Plot relative fat mass
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

## Plot lean mass
lean <- ggplot(
  data = ActSGRData, 
  mapping = aes(x = food , y = leanmass , fill=greb1l)
) +geom_boxplot()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, face = "bold", margin = margin(r=15, l=15)),
        axis.text.x = element_text(size=18, margin = margin(b=50)),
        axis.text.y = element_text(size=18),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic"),
        legend.position = "none",
        
  )+
  labs(x="Food treatment", y="Lean mass (g)")+
  ylim(0, 600)

## Show both plots on the same board
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





