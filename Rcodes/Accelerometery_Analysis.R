### This script was used to analyse accelerometry data ###

library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(vctrs)
library(ggthemes)
library(plotrix)
library(ggplot2)
library(ggrepel)
library(emmeans)
library(lsmeans)
library(tools)
library(ggeffects)
library(glmmTMB)
library(performance)
library(DHARMa)
library(mgcv)
library(tidygam)
library(hms)

source("path_file_lilian.R")

Full_Data <- read_xlsx(paste0(path.data, "Biologger final data.xlsx")) #open dataset

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
  )%>%
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins"), #calculate duration of daylight according to NOAA values (previously added to the dataset)
    Time = hms::hms(Time),
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
  )

### Next we do a little more formatting so that plots look ok

Full_Data$Light <- factor(Full_Data$Light, levels = c("Night", "Day"))
Full_Data$Season <- factor(Full_Data$Season, levels = c("Winter", "Spring"))
Full_Data$food <- factor(Full_Data$food, levels = c("high-food", "low-food"))

Full_Data$MeasNB <- factor(Full_Data$MeasNB, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"))

Full_Data$food <- dplyr::recode(Full_Data$food, "high-food" = "Ad Libitum",  "low-food" = "Restricted")
Full_Data$Swimstyle <- dplyr::recode(Full_Data$Swimstyle, "Dynamic" = "Burst Swimming",  "Steady" = "Steady Swimming")


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
## 1) https://research.reading.ac.uk/psylinglab/wp-content/uploads/sites/25/2023/06/Learn-Generalised-Additive-Mixed-Models.pdf
## 2) https://stats.stackexchange.com/questions/633116/gamm-using-mgcv-in-r-by-participant-random-effects-for-categorical-variables
## 3) https://drmowinckels.io/blog/2018/gamm-random-effects/#:~:text=Explore%20the%20use%20of%20Generalized%20Additive%20Mixed%20Models,walks%20through%20a%20detailed%20example%20with%20simulated%20data.

## k was set to 20 because there are ~20 measurements periods (20*24hrs)


GAMMAvg <- bam(AvgEA ~ food*greb1l*Swimstyle + Light + s(Date, k=20) + s(Date, by = food_greb, k=20) + s(Biologger, bs = "re") + s(food,tank, bs="re") + s(fam, bs="re"),
                family = Gamma(link = "log"),
                data = Full_Data_GAMM)

gam.check(GAMMAvg)
concurvity(GAMMAvg, full=FALSE)
performance(GAMMAvg)
### => model performances are okay-ish, the plots don't look that great but I guess accelerometery data is a bit tricky 
## Note : => it is not necessary to nest tank/Biologger if all Biologgers are differents
### Necessary only if fish in different tanks have the same ID, to tell the model, e.g., that fish 1 in tank 1 is not the same as fish 1 in tank 2
### Nesting tank in food doesn't change anything either, so I guess it can stay this way.

summary(GAMMAvg)
AIC(GAMMAvg)

pairs(emmeans(GAMMAvg, ~ greb1l | food * Swimstyle, type = "response"), adjust="sidak")
# the "adjust" argument does not work, so I computed p-values adjustment manually, with sidak method: p-adj = 1-(1-p)^n (for n=4 tests)



## The low-food smooth are both different from the reference (here, high food + allele 1) but not high-food allele 2
## and there is a >1 difference in edf between allele 1 and 2 in low-food, which I think means they differ (see source 1), p.24)
ggplot(data=Full_Data, aes(y= log(AvgEA), x=Date, col= greb1l))+
  facet_wrap(~Swimstyle+food)+
  geom_point(alpha=.06)+
  geom_smooth(linewidth = 1.2, formula = y ~ s(x, k = 20))+
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic")
  )+
  labs(y="log(AvgEA) (mg)")
  #geom_vline(xintercept = c(as.POSIXct("2025-02-03"), as.POSIXct("2025-02-07")), color="lightblue")

### Here I wanted to focus on this Steady Swimming + food Restricted group to see what the possible genotype effect looks like
StdRestr_Data <- Full_Data%>%
  filter(
    food=="Restricted",
    Swimstyle=="Steady Swimming"
  )

ggplot(data=StdRestr_Data, aes(y= AvgEA, x=Date, col= food))+
  geom_point(alpha=.05)+
  geom_smooth(linewidth = 1.2, formula = y ~ s(x, k = 20))+
  theme_bw(base_size = 16)+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 20, face = "italic"),
        legend.text  = element_text(size = 18,face = "italic")
  )+
  labs(y="AvgEA (mg)")
