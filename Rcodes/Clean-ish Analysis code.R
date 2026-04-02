
library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(vctrs)
library(ggthemes)
library(plotrix)
library(ggplot2)
library(ggrepel)
library(lsmeans)
library(emmeans)
library(tools)
library(ggeffects)
library(glmmTMB)
library(performance)
library(DHARMa)
library(mgcv)
library(tidygam)
library(hms)

source("path_file_lilian.R")

Full_Data <- read_xlsx(paste0(path.data, "Biologger final data.xlsx"))

str(Full_Data)

## Some variable ar as chr or num when we want them to be factors or dates or whatever, so it needs a bit of formatting

Full_Data <- Full_Data %>%
  mutate(
    Temp = as.numeric(Temp),
    greb1l = as.factor(greb1l),
    omy05 = as.factor(omy05),
    food = as.factor(food),
    fam = as.factor(fam),
    tank = as.factor(tank),
    Biologger = as.factor(Biologger),
    Season = as.factor(Season),
    feeders = as.factor(feeders),
    fasting_day = as.factor(fasting_day),
    Light = as.factor(Light),
    am_pm = as.factor(am_pm),
    MeasNB = as.factor(MeasNB),
    Swimstyle = as.factor(Swimstyle),
    Tempgroup = as.factor(Tempgroup),
    Month = as.factor(Month),
    Date = ymd(Date),
    start_date = ymd(start_date),
    FebMeasDate = ymd(FebMeasDate),
    MayMeasDate = ymd(MayMeasDate),
    Time = lubridate::hms(Time),
    NOAA_Sunrise = lubridate::hms(NOAA_Sunrise),
    NOAA_Sunset = lubridate::hms(NOAA_Sunset),
    Time = hms::hms(Time),
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
  )%>%
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
         NOAA_Sunset = as.numeric(NOAA_Sunset),
         DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins"),
         NOAA_Sunrise = hms::hms(NOAA_Sunrise),
         NOAA_Sunset = hms::hms(NOAA_Sunset),
  )

Full_Data$Light <- factor(Full_Data$Light, levels = c("Night", "Day"))
Full_Data$Season <- factor(Full_Data$Season, levels = c("Winter", "Spring"))
Full_Data$food <- factor(Full_Data$food, levels = c("high-food", "low-food"))

Full_Data$MeasNB <- factor(Full_Data$MeasNB, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"))

Full_Data$food <- dplyr::recode(Full_Data$food, "high-food" = "High Food",  "low-food" = "Low Food")
Full_Data$Swimstyle <- dplyr::recode(Full_Data$Swimstyle, "Dynamic" = "Active Swimming",  "Steady" = "Steady Swimming")



## Plot temperature data over time

ggplot(data=Full_Data, aes(y= Temp, x=MeasNB, col=tank))+
  geom_point(alpha=.01)+
  geom_smooth(method="gam")+
  theme_bw(base_size = 16)+
  theme(axis.title.x = element_text(size=20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
  )+
  labs(y="Temperature (°C)")+
  ylim(0,8)


Avg1 <- glmmTMB(AvgEA ~ Light*Season*food*greb1l+ (1 | tank/Biologger) + (1 | fam),
              family = Gamma(link="log"), 
              data = Full_Data)
#performance(Avg1)
summary(Avg1)


## Test the Light*Season effect 

Avg2 <- glmmTMB(AvgEA ~ Light*Season+food+greb1l+ (1 | tank/Biologger) + (1| fam),
              family = Gamma(link="log"), 
              data = Full_Data)
#performance(Avg2)
summary(Avg2)

ggplot(Full_Data_Out, aes(x = Light, y = log(AvgEA)))+
  facet_wrap(~Season)+
  geom_boxplot()


## No interaction between light and season, no nesting needed

Avg3 <- glmmTMB(AvgEA ~ Light*food*greb1l+ (1 | tank/Biologger) + (1| fam),
              family = Gamma(link="log"), 
              data = Full_Data)

#performance(Avg3)
summary(Avg3)

## Season*food interaction, expected since feeding is different between seasons

## Try GAMM with Date 

## convert Date to numeric and add a food_greb column to test the interaction in gamm

Full_Data_GAMM <- Full_Data %>%
  mutate(
    Date=as.numeric(Date),
    tank = as.factor(tank),
    food_greb=as.ordered(interaction(food, greb1l)),
  )

contrasts(Full_Data_GAMM$food_greb) <- "contr.treatment"
## apparently, bam() is better for big datasets, so try that first
## sources : 
## https://research.reading.ac.uk/psylinglab/wp-content/uploads/sites/25/2023/06/Learn-Generalised-Additive-Mixed-Models.pdf
## https://stats.stackexchange.com/questions/633116/gamm-using-mgcv-in-r-by-participant-random-effects-for-categorical-variables
## https://drmowinckels.io/blog/2018/gamm-random-effects/#:~:text=Explore%20the%20use%20of%20Generalized%20Additive%20Mixed%20Models,walks%20through%20a%20detailed%20example%20with%20simulated%20data.

## I had doubt on whether I should add biologger as a random intercept  s(Biologger, bs = "re") or random smooth s(Date, Biologger, bs = "fs"), so I tried both to compare AICs
## k was set to 20 because there are ~20 measurements periods (20*24hrs)

GAMMAvg1 <- bam(AvgEA ~ food_greb+ s(Date, k=20) + s(Date, by = food_greb, k=20) + s(Date, Biologger, bs = "fs", m=1)+ s(LengthFeb, bs="re") + s(tank, bs="re") + s(fam, bs="re") + Light,
                  family = Gamma(link = "log"),
                  data = Full_Data_GAMM)


GAMMAvg2 <- bam(AvgEA ~ food*greb1l*Swimstyle + Light + s(Date, k=20) + s(Date, by = food_greb, k=20) + s(Biologger, bs = "re") + s(tank, bs="re") + s(fam, bs="re"),
                family = Gamma(link = "log"),
                data = Full_Data_GAMM)

AIC(GAMMAvg1, GAMMAvg2)  ##AIC GAMMAvg1<GAMMAvg2, keep factor smooth interaction?

gam.check(GAMMAvg1)
concurvity(GAMMAvg1)

gam.check(GAMMAvg2)
concurvity(GAMMAvg2) ##Model parameters are much better in GAMMAvg2 (less concurvity)


## Note : I did not find a way to nest tank/Biologger as random, especially with that smooth thing, but I think that's okay as it is?

summary(GAMMAvg2)
AIC(GAMMAvg2)


EMMF2 <- emmeans(GAMMAvg2, ~Swimstyle:greb1l:food, data=Full_Data_GAMM) 
EMM_respF2 <- regrid(EMMF2)
pairs(EMM_respF2, adjust="none")


## The low-food smooth are both different from the reference (here, high food + allele 1) but not high-food allele 2
## and there is a >1 difference in edf between allele 1 and 2 in low-food, which I think means they differ (see source 1, p.24)
  
ggplot(data=Full_Data, aes(y= log(AvgEA), x=Date, col= greb1l))+
  facet_wrap(~food+Swimstyle)+
  geom_point(alpha=.1)+
  geom_smooth()+
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1l", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic")
  )+
  labs(y="log(AvgEA) (mg)")







## Just to make sure that there is an actual difference between allele 1 and 2 in low food, I ran gamm() separately in both food groups

Full_Data_GAMM_HighFood <- Full_Data_GAMM %>% 
  filter(
    food == "High Food"
  )


Full_Data_GAMM_LowFood <- Full_Data_GAMM %>% 
  filter(
    food == "Low Food"
  )


## Let's do low food first


GAMMAvgLF <- bam(AvgEA ~ greb1l*Swimstyle + Light +  s(Date, k=20) + s(Date, by=greb1l, k=20)+ s(Biologger, bs = "re")+ s(tank, bs="re") + s(fam, bs="re"),
                  family = Gamma(link = "log"),
                  data = Full_Data_GAMM_LowFood)

gam.check(GAMMAvgLF)
concurvity(GAMMAvgLF)
summary(GAMMAvgLF)

##Adding the greb*swim interaction improves the model quite a lot

EMMF2 <- emmeans(GAMMAvgHF, ~greb1l:Swimstyle, data=Full_Data_GAMM_HighFood) 
EMM_respF2 <- regrid(EMMF2)
pairs(EMM_respF2)



### Confirms results of the big model: no effect of greb alone in either case, 
## but effect of smooth by greb in low food => what we see on the plot


GAMMAvgHF <- bam(AvgEA ~ greb1l*Swimstyle + Light + s(Date, k=20) + s(Date, by=greb1l, k=20)+ s(Biologger, bs = "re")+ s(tank, bs="re") + s(fam, bs="re"),
                 family = Gamma(link = "log"),
                 data = Full_Data_GAMM_HighFood)

gam.check(GAMMAvgHF)
concurvity(GAMMAvgHF)
summary(GAMMAvgHF)



## Now let's test if the difference in activity is actually in Active or Steady swimming, or both
## Hypothesis: if activity increases because of increased feeding, the difference should be in active swimming

LFdata <- Full_Data %>%
  filter(
    food == "Low Food"
  )


ggplot(data=LFdata, aes(y= log(AvgEA), x=Date, col= greb1l))+
  facet_wrap(~Swimstyle)+
  geom_point(alpha=.1)+
  geom_smooth(method="gam")+
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1l", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14, face = "italic")
  )+
  labs(y="log-transformed Average EA (mg)")

HFdata <- Full_Data %>%
  filter(
    food == "High Food"
  )

ggplot(data=LFdata, aes(y= log(AvgEA), x=Date, col= greb1l))+
  facet_wrap(~Swimstyle)+
  geom_point(alpha=.1)+
  geom_smooth(method="gam")+
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1l", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14, face = "italic")
  )+
  labs(y="log-transformed Average EA (mg)")


### And now it's back to regular glmms I showed in the last meeting


## Look at Light, food and greb1l separately in winter and spring
Full_Data_Winter <- Full_Data %>% 
  filter(
    Season == "Winter"
  )

Full_Data_Spring <- Full_Data %>% 
  filter(
    Season == "Spring"
  )

WinterAvg1 <- glmmTMB(AvgEA ~ Light*food*greb1l+ (1 | tank/Biologger) + (1| fam), ## <= Lowest AIC, light*food*greb1l significant
                    family = Gamma(link="log"), 
                    data = Full_Data_Winter)
#performance(WinterAvg1)
summary(WinterAvg1)

predict.WinterAvg1 <- ggpredict(WinterAvg1, terms = c("Light", "food","greb1l"))
colnames(predict.WinterAvg1)[6] <- "Food"
colnames(predict.WinterAvg1)[7] <- "greb1l"

ggplot(predict.WinterAvg1, aes(y= predicted, x=x, col= greb1l))+
  facet_wrap(~Food)+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))



WinterAvg2 <- glmmTMB(AvgEA ~ Light*food+greb1l+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Winter)
#performance(WinterAvg2)
summary(WinterAvg2)

WinterAvg3 <- glmmTMB(AvgEA ~ Light+food*greb1l+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Winter)
#performance(WinterAvg3)
summary(WinterAvg3)

WinterAvg4 <- glmmTMB(AvgEA ~ Light*greb1l+food+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Winter)
#performance(WinterAvg4)
summary(WinterAvg4)

WinterAvg5 <- glmmTMB(AvgEA ~ Light+greb1l+food+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Winter)
#performance(WinterAvg5)
summary(WinterAvg5)



SpringAvg1 <- glmmTMB(AvgEA ~ Light*food*greb1l+ (1 | tank/Biologger) + (1| fam), ## <= Lowest AIC, light*food*greb1l significant
                    family = Gamma(link="log"), 
                    data = Full_Data_Spring)
#performance(SpringAvg1)
summary(SpringAvg1)

predict.SpringAvg1 <- ggpredict(SpringAvg1, terms = c("Light", "food","greb1l"))
colnames(predict.SpringAvg1)[6] <- "Food"
colnames(predict.SpringAvg1)[7] <- "greb1l"

ggplot(predict.SpringAvg1, aes(y= predicted, x=x, col= greb1l))+
  facet_wrap(~Food)+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))




SpringAvg2 <- glmmTMB(AvgEA ~ Light*food+greb1l+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Spring)
#performance(SpringAvg2)
summary(SpringAvg2)

SpringAvg3 <- glmmTMB(AvgEA ~ Light+food*greb1l+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Spring)
#performance(SpringAvg3)
summary(SpringAvg3)

SpringAvg4 <- glmmTMB(AvgEA ~ Light*greb1l+food+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Spring)
#performance(SpringAvg4)
summary(SpringAvg4)

SpringAvg5 <- glmmTMB(AvgEA ~ Light+food+greb1l+ (1 | tank/Biologger) + (1| fam),
                    family = Gamma(link="log"), 
                    data = Full_Data_Spring)
#performance(SpringAvg5)
summary(SpringAvg5)




####################################################################################

############################# SAME ANALYSIS WITH MAXEA #############################

####################################################################################



Max1 <- glmmTMB(MaxEA ~ Light*Season*food*greb1l+ (1 | tank/Biologger) + (1| fam),
                family = Gamma(link="log"), 
                data = Full_Data)
#performance(Max1)
summary(Max1)


## Test the Light*Season effect 

Max2 <- glmmTMB(MaxEA ~ Light*Season+food+greb1l+ (1 | tank/Biologger) + (1| fam),
                family = Gamma(link="log"), 
                data = Full_Data)
#performance(Max2)
summary(Max2)

ggplot(Full_Data, aes(x = Light, y = log(MaxEA)))+
  facet_wrap(~Season)+
  geom_violin()


## Interaction between light and season, nesting needed ??

Max3 <- glmmTMB(MaxEA ~ Light+Season*food*greb1l+ (1 | tank/Biologger) + (1| fam),
                family = Gamma(link="log"), 
                data = Full_Data)

#performance(Max3)
summary(Max3)

## Season*food interaction, expected since feeding is different between seasons

## Look at Light, food and greb1l separately in winter and spring

WinterMax1 <- glmmTMB(MaxEA ~ Light*food*greb1l+ (1 | tank/Biologger) + (1| fam), 
                      family = Gamma(link="log"), 
                      data = Full_Data_Winter)
#performance(WinterMax1)
summary(WinterMax1)

WinterMax2 <- glmmTMB(MaxEA ~ Light*food+greb1l + (1 | tank/Biologger) + (1| fam), ## <= Lowest AIC, light*food significant
                      family = Gamma(link="log"), 
                      data = Full_Data_Winter)
#performance(WinterMax2)
summary(WinterMax2)


predict.WinterMax2 <- ggpredict(WinterMax2, terms = c("Light", "food","greb1l"))
colnames(predict.WinterMax2)[6] <- "Food"
colnames(predict.WinterMax2)[7] <- "greb1l"

ggplot(predict.WinterMax2, aes(y= predicted, x=x, col= greb1l))+
  facet_wrap(~Food)+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))



WinterMax3 <- glmmTMB(MaxEA ~ Light+food*greb1l+ (1 | tank/Biologger) + (1| fam),
                      family = Gamma(link="log"), 
                      data = Full_Data_Winter)
#performance(WinterMax3)
summary(WinterMax3)

WinterMax4 <- glmmTMB(MaxEA ~ Light*greb1l+food+ (1 | tank/Biologger) + (1| fam),
                      family = Gamma(link="log"), 
                      data = Full_Data_Winter)
#performance(WinterMax4)
summary(WinterMax4)

WinterMax5 <- glmmTMB(MaxEA ~ Light+greb1l+food+ (1 | tank/Biologger) + (1| fam),
                      family = Gamma(link="log"), 
                      data = Full_Data_Winter)
#performance(WinterMax5)
summary(WinterMax5)



SpringMax1 <- glmmTMB(MaxEA ~ Light*food*greb1l+ (1 | tank/Biologger) + (1| fam), ## <= Lowest AIC, light*food*greb1l significant
                      family = Gamma(link="log"), 
                      data = Full_Data_Spring)
#performance(SpringMax1)
summary(SpringMax1)


predict.SpringMax1 <- ggpredict(SpringMax1, terms = c("Light", "food","greb1l"))
colnames(predict.SpringMax1)[6] <- "Food"
colnames(predict.SpringMax1)[7] <- "greb1l"

ggplot(predict.SpringMax1, aes(y= predicted, x=x, col= greb1l))+
  facet_wrap(~Food)+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))




SpringMax2 <- glmmTMB(MaxEA ~ Light*food+greb1l+ (1 | tank/Biologger) + (1| fam),
                      family = Gamma(link="log"), 
                      data = Full_Data_Spring)
#performance(SpringMax2)
summary(SpringMax2)

SpringMax3 <- glmmTMB(MaxEA ~ Light+food*greb1l+ (1 | tank/Biologger) + (1| fam),
                      family = Gamma(link="log"), 
                      data = Full_Data_Spring)
#performance(SpringMax3)
summary(SpringMax3)

SpringMax4 <- glmmTMB(MaxEA ~ Light*greb1l+food+ (1 | tank/Biologger) + (1| fam),
                      family = Gamma(link="log"), 
                      data = Full_Data_Spring)
#performance(SpringMax4)
summary(SpringMax4)

SpringMax5 <- glmmTMB(MaxEA ~ Light+food+greb1l+ (1 | tank/Biologger) + (1| fam),
                      family = Gamma(link="log"), 
                      data = Full_Data_Spring)
#performance(SpringMax5)
summary(SpringMax5)


