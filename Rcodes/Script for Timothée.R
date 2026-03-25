### This is basically the models I have run to try and understand what was going on


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

#first, read the data file I shared (change the file's path so that it matches yours)
Full_Data <- read_xlsx(paste0(path.data, "Biologger final data.xlsx"))

## Some variable ar as chr or num when we want them to be factors or dates or whatever, so it needs a bit of formatting
Full_Data <- Full_Data %>%
  mutate(
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
  #for some reasons there are 2 "daylength" variables calculated differently
  #and I cannot remember which one is the right one
  #so I delete them and make a new one
    select(
    -Daylength, -DayLength
  )%>%
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "mins"),
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
  )

#then I make new levels for factors so that is shows in the right order in plots

Full_Data$Light <- factor(Full_Data$Light, levels = c("Night", "Day"))
Full_Data$Season <- factor(Full_Data$Season, levels = c("Winter", "Spring"))
Full_Data$food <- factor(Full_Data$food, levels = c("high-food", "low-food"))

Full_Data$MeasNB <- factor(Full_Data$MeasNB, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21"))

#and slightly change some levels' names, again for nicer plots
Full_Data$food <- dplyr::recode(Full_Data$food, "high-food" = "High Food",  "low-food" = "Low Food")
Full_Data$Swimstyle <- dplyr::recode(Full_Data$Swimstyle, "Dynamic" = "Active Swimming",  "Steady" = "Steady Swimming")


##Now I will run gamms, and for that date needs to be converted to a numeric variable
## and if I want to test an interaction I also need to creat it manually

Full_Data_GAMM <- Full_Data %>%
  mutate(
    Date=as.numeric(Date),
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


GAMMAvg2 <- bam(AvgEA ~ food*greb1l + s(Date, k=20) + s(Date, by = food_greb, k=20) + s(Biologger, bs = "re")+ s(LengthFeb, bs="re") + s(tank, bs="re") + s(fam, bs="re") + Light,
                family = Gamma(link = "log"),
                data = Full_Data_GAMM)

#Then I compare model parameters to estimate which one makes more sense

AIC(GAMMAvg1, GAMMAvg2)  

gam.check(GAMMAvg1)
concurvity(GAMMAvg1)

gam.check(GAMMAvg2)
concurvity(GAMMAvg2) ##Model parameters are much better in GAMMAvg2 (less concurvity)


## Note : I did not find a way to nest tank/Biologger as random, especially with that smooth thing, but I think that's okay as it is?

summary(GAMMAvg2)

##I was not sure what these results meant for greb1l
#so I decided to run one gamm separately for each food treatment

# High Food first :
Full_Data_GAMM_HighFood <- Full_Data_GAMM %>% 
  filter(
    food == "High Food"
  )

GAMMAvgHF <- bam(AvgEA ~ greb1l + s(Date) + s(Date, by=greb1l) + Light+ s(Biologger, bs = "re")+ s(tank, bs="re") + s(fam, bs="re"),
                 family = Gamma(link = "log"),
                 data = Full_Data_GAMM_HighFood)
gam.check(GAMMAvgHF)
concurvity(GAMMAvgHF)
summary(GAMMAvgHF)


# Then Low Food :
Full_Data_GAMM_LowFood <- Full_Data_GAMM %>% 
  filter(
    food == "Low Food"
  )

GAMMAvgLF <- bam(AvgEA ~ greb1l + s(Date) + s(Date, by=greb1l) + Light+ s(Biologger, bs = "re")+ s(tank, bs="re") + s(fam, bs="re"),
                 family = Gamma(link = "log"),
                 data = Full_Data_GAMM_LowFood)
gam.check(GAMMAvgLF)
concurvity(GAMMAvgLF)
summary(GAMMAvgLF)


##Then I plot the data to see what the differences actually are

ggplot(data=Full_Data, aes(y=log(AvgEA), x=Date, col= greb1l))+
  facet_wrap(~food)+
  geom_point(alpha=.2)+
  geom_smooth()+
  scale_color_manual(values = c("gold2", "royalblue"))

#And there seems to be a slight difference between alleles 1 and 2 in low food
#like, not a general effect of greb1l, but smooths are considered different only in the low food group

#But then, out of curiosity, I plotted the same but by tank instead of food treatment
#Note that tanks 21, 23 and 25 =low food, and 22,24 and 26 = high food
ggplot(data=Full_Data, aes(y=log(AvgEA), x=Date, col= greb1l))+
  facet_wrap(~tank)+
  geom_point(alpha=.2)+
  geom_smooth()+
  scale_color_manual(values = c("gold2", "royalblue"))

#Here it looks like the difference is actually only in tank 23
#Is the model taking that into account somehow ?
#Or is the food effect actually a tank23 effect ?
