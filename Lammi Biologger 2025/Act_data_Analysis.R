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
library(DHARMa)
library(glmmTMB)
library(performance)

Full_Data_Clean <- read_xlsx("C:/Users/03278487/OneDrive - Luonnonvarakeskus/ThermoEvo - General/Lilian data/R outputs/Biologgers data/Biologger Clean data.xlsx") 


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

colSums(Full_Data_Out[, "BurstScore"] == 0)                  #' Number of zeros
100 * colSums(Full_Data_Out[, "BurstScore"] == 0) / nrow(Full_Data_Out) #' % of zeros


#idea to linearize time of day effect : convert it to "distance from noon", so you get only one side of the bell curve ?

Full_Data_Out <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    Time = as.numeric(Time),
    TimeToNoon = as.numeric(abs(45000 - Time)), ### 45000 is 12:30, it is the time that performs better as center of the distribution
  )

str(Full_Data_Out$TimeToNoon)

### make the binomial model with these parameters

B1 <- glmmTMB(BurstScore ~ Tempgroup*food*TimeToNoon + (1 |Biologger )+ (1 |tank )+ (1 |fam ),
              family = "binomial", 
              data = Full_Data_Out)



#' Is fStandType significant? Apply a likelihood ratio test.
drop1(B1, test = "Chi")    

#' R^2
performance(B1) #' Be careful with the interpretation!

#' Numerical output
summary(B1)


#' Get scaled quantile residuals.
B1Bernqr <- simulateResiduals(fittedModel = B1, plot = FALSE)

#' Check whether these quantile residuals are uniformly distributed.
par(mfrow = c(1,1), mar = c(5,5,2,2))
plotQQunif(B1Bernqr, 
           testUniformity = TRUE, 
           testOutliers= TRUE,
           testDispersion = FALSE)
#' Looks OK.

#' Plot the scaled quantile residuals versus fitted values.
plotResiduals(B1Bernqr, quantreg = FALSE) 
#' Looks OK?


#' Plot the scaled quantile residuals versus each covariate.
#' BurnAge
plotResiduals(B1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out_TIME$Tempgroup, 
              xlab = "Tempgroup") 

#' fEcozone
plotResiduals(B1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out_TIME$food, 
              xlab = "food") 


#' fStandType
plotResiduals(B1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out_TIME$greb1l, 
              xlab = "greb1l") 


#' fStandType
plotResiduals(B1Bernqr, 
              quantreg = FALSE,
              form = Full_Data_Out_TIME$TimeToNoon, 
              xlab = "Time to noon") 


### NO EFFECT OF GREB1L ON BURST FREQUENCY WITH THE BERNOULLI METHOD ###
### Now, wee test the effect of these factors on burst intensity and steady swimming separately




#determine whether fedeers are ON or OFF based on date and feeders rhythm

start_date <- "2025-01-14"

Full_Data_Out_feed <- Full_Data_Out %>% 
  mutate(
    start_date = ymd(start_date),
    days_since_start = as.numeric(difftime(Date, start_date, units = "days")),
    cycle_pos = days_since_start %% 3,
    feeders = ifelse(food == "high-food",
                     "ON",
                     ifelse(
                       cycle_pos == 1, "OFF",   # feeders OFF
                       "ON"               # feeders ON
                     )
    )
  )


### Because not enough data in the burst data set to add Time as a covariate (some time groups are empty or 1), we will group by day/night instead
###Estimate sunrise and sunset based on https://www.timeanddate.com/sun/finland/lahti data. Nautical twilight was considered to be night.

Initial_Sunrise <- as.numeric(3600*8+60*18)
Initial_Sunset <- as.numeric(3600*16+60*34)

### Test the difftime function
Initial_Daylength <- difftime(Initial_Sunset, Initial_Sunrise, units = "hours")

### daily change in sunset and sunrise were calculated as (|"sunrise time on 14/01" - "sunrise time on 09/05"|)/"number of days between 14/01 and 09/05"
### which were 333/115 for sunrise and 311/115 for sunset in mins/day, so multiply times 60 to get secs/day


### Create the Light column based on this
Full_Data_Out <- Full_Data_Out %>% 
  mutate(
    Initial_Sunrise = hms::hms(Initial_Sunrise),
    Initial_Sunset = hms::hms(Initial_Sunset),
    days_since_start = as.numeric(difftime(Date, start_date, units = "days")),
    Sunrise = as.numeric(Initial_Sunrise-days_since_start*(333*60/115)),
    Sunset = as.numeric(Initial_Sunset+days_since_start*(311*60/115)),
  )

Full_Data_Out <- Full_Data_Out %>% 
  mutate(
    Sunrise = hms::hms(Sunrise),
    Sunset = hms::hms(Sunset),
    Light = as.factor(ifelse(Time < Sunrise, 
                             "Night",
                             ifelse(Time < Sunset,
                                    "Day",
                                    "Night"))
    ),
  )


str(Full_Data_Out$Sunrise)
str(Full_Data_Out$Sunset)
str(Full_Data_Out$Light)


### Create one df for each season (not the best approach here)
#Full_Data_Spring <- Full_Data_Out %>%
#  filter(
#    Season == "Spring"
#    )
#
#Full_Data_Winter <- Full_Data_Out %>%
#  filter(
#    Season == "Winter"
#  )

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


CountBurstMod <- glmmTMB(mean_BurstScore ~ (Tempgroup+greb1l)*food + (1 |tank )+ (1 |fam ), ##best model
                         family= gaussian(),
                         data = TempBurst)

hist(resid(CountBurstMod))
qqPlot(resid(CountBurstMod))
performance(CountBurstMod)
summary(CountBurstMod)

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


## For MaxEA, Gamma is best but q-q plot looks weird

ACTmodelMax_F <- glmmTMB(MaxEA~ (TimeToNoon+Tempgroup+greb1l)*food + (1 |Biologger ),
                         family = Gamma("log"), 
                         data = FilteredData)

hist(resid(ACTmodelMax_F))
qqPlot(resid(ACTmodelMax_F))
performance(ACTmodelMax_F)
summary(ACTmodelMax_F)

ACTmodelVAR_F <- glmmTMB(VAR~ Light*Tempgroup*food + (1 |Biologger ), 
                         family = Gamma("log"), 
                         data = FilteredData)
hist(resid(ACTmodelVAR_F))
qqPlot(resid(ACTmodelVAR_F))
performance(ACTmodelVAR_F)
summary(ACTmodelVAR_F)


### Analyse Steady swim data 



ACTmodelAvg_B <- glmmTMB(AvgEA~ TimeToNoon+Tempgroup*food*greb1l + (1 |Biologger ),
                         family = Gamma("log"), 
                         data = BaselineData)

hist(resid(ACTmodelAvg_B))
qqPlot(resid(ACTmodelAvg_B))
performance(ACTmodelAvg_B)
summary(ACTmodelAvg_B)
drop1(ACTmodelAvg_B, test = "Chi")   


ACTmodelVAR_B <- glmmTMB(VAR~ Light*Tempgroup*food*greb1l + (1 |Biologger ), 
                         family = gaussian, 
                         data = BaselineData)
hist(resid(ACTmodelVAR_F))
qqPlot(resid(ACTmodelVAR_F))
performance(ACTmodelVAR_F)
summary(ACTmodelVAR_F)




### Analyse Steady swim data 


BaselineDataGAMM <- BaselineData%>%
  mutate(
    TimeToNoon = as.numeric(TimeToNoon),
    DateTime = as.numeric(DateTime),
    Date = as.numeric(Date),
  )

BaselineDataGAMM <- droplevels(BaselineDataGAMM)

BaselineDataGAMM_LF_HT <- subset(BaselineDataGAMM, food =="low-food" & Tempgroup =="HighTemp")
BaselineDataGAMM_LF_HT <- droplevels(BaselineDataGAMM_LF_HT)

Baseline2 <- subset(Full_Data_Out, VAR<220)
Baseline_LF <- subset(Baseline2, food =="low-food")
Baseline_LF_HT <- subset(Baseline_LF, Tempgroup =="HighTemp")
Baseline_LF_HT <- droplevels(Baseline_LF_HT)
### For MaxEA, Gamma is best but q-q plot looks weird

ACTmodelAvg_B <- gamm(AvgEA~ s(Time, by=greb1l),
                      random = list(Biologger= ~1),
                      family = Gamma("log"), 
                      data = BaselineDataGAMM_LF_HT)

hist(resid(ACTmodelAvg_B))
qqPlot(resid(ACTmodelAvg_B))
performance(ACTmodelAvg_B$gam)
summary(ACTmodelAvg_B$gam)

ACTmodelVAR_B <- gamm(VAR~ s(Time) + (Light+Tempgroup+greb1l)*food,
                      random = list(Biologger= ~1),
                      family = Gamma("log"), 
                      data = FilteredData)
hist(resid(ACTmodelVAR_B))
qqPlot(resid(ACTmodelVAR_B))
performance(ACTmodelVAR_B$gam)
summary(ACTmodelVAR_B$gam)



ACTmodelAvg_B <- glmmTMB(AvgEA~ (Tempgroup+TimeToNoon+greb1l)*food + (1 |Biologger ),
                         family = gaussian(), 
                         data = Baseline2)

hist(resid(ACTmodelAvg_B))
qqPlot(resid(ACTmodelAvg_B))
performance(ACTmodelAvg_B)
summary(ACTmodelAvg_B)
drop1(ACTmodelAvg_B, test = "Chi")   

ggplot(
  data = Baseline_LF_HT, 
  mapping = aes(x = Time, y = AvgEA, col=greb1l),
) +
  #facet_wrap(~food+Tempgroup)+
  geom_point(alpha=0.2)+
  geom_smooth()




ACTmodelVAR_B <- glmmTMB(VAR~ Light*greb1l + (1 |Biologger ), 
                         family = Gamma("log"), 
                         data = Baseline_LF_HT)
hist(resid(ACTmodelVAR_B))
qqPlot(resid(ACTmodelVAR_B))
performance(ACTmodelVAR_B)
summary(ACTmodelVAR_B)


ggplot(
  data = Baseline_LF_HT, 
  mapping = aes(x = Light, y = AvgEA),
) +
  #facet_wrap(~food+Tempgroup)+
  geom_boxplot(aes(fill=greb1l))
#geom_smooth(method="lm")



###################################           Trying to apply what I learned from the zero-inflated glmm course           ###############################################################################but Ibu

phi.est <- summary(ACTmodelMax_F$gam)$dispersion                        #' phi  
mu1     <- fitted(ACTmodelMax_F$gam)                       #' Exp(Y)
VarY    <- phi.est * mu1 ^ 2            #' var(Y) 
E1      <- (FilteredDataGAMM$LogMaxEA - mu1) / sqrt(VarY) #' Pearson residuals

#' Add the Pearson residuals and the fitted values to the
#' CR2 object.
FilteredDataGAMM$E1  <- E1
FilteredDataGAMM$mu1 <- mu1


ggplot(FilteredDataGAMM, aes(x = mu1, y = E1)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values", y = "Pearson residuals")



M1gam <- gam(LogMaxEA~ s(Time)+WeightFeb:food+(Tempgroup+greb1l)*food,
             family = Gamma(link = identity),
             data = FilteredDataGAMM)

## check with DHARMa
E1GammaQR <- simulateResiduals(fittedModel = M1gam, plot = FALSE)

# Check uniformity and outliers
plotQQunif(E1GammaQR, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)

# Residuals vs fitted
plotResiduals(E1GammaQR, quantreg = TRUE)


### NOTE : RESULTS ARE NOT GOOD
#########################################################################################################################################################################################



#

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
                       random = list(Biologger= ~1, fam=~1, tank=~1), family = Gamma(link = log),
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


###################################           Trying to apply what I learned from the zero-inflated glmm course           ###############################################################################but Ibu

phi.est <- summary(ACTmodelAvg_B3$gam)$dispersion           #' phi  
mu1     <- fitted(ACTmodelAvg_B3$gam)                       #' Exp(Y)
VarY    <- phi.est * mu1 ^ 2            #' var(Y) 
E1      <- (BaselineDataGAMM$AvgEA - mu1) / sqrt(VarY) #' Pearson residuals

#' Add the Pearson residuals and the fitted values to the
#' CR2 object.
BaselineDataGAMM$E1  <- E1
BaselineDataGAMM$mu1 <- mu1


ggplot(BaselineDataGAMM, aes(x = mu1, y = E1)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values", y = "Pearson residuals")



M1gam <- gam(AvgEA~ s(Time)+ Tempgroup*greb1l*food,
             family = Gamma(link = identity),
             data = BaselineDataGAMM)

## check with DHARMa
E1GammaQR <- simulateResiduals(fittedModel = M1gam, plot = FALSE)

# Check uniformity and outliers
plotQQunif(E1GammaQR, testUniformity = TRUE,
           testOutliers = TRUE, testDispersion = FALSE)

# Residuals vs fitted
plotResiduals(E1GammaQR, quantreg = TRUE)


### NOTE : RESULTS ARE NOT GOOD
#########################################################################################################################################################################################




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



