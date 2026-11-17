### This script is to tidy and analyse body measurement data  ###


library(dplyr)
library(lubridate)
library(purrr)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(ggplot2)
library(ggeffects)
library(cowplot)
library(glmmTMB)
library(emmeans)
library(performance)
library(DHARMa)


## Step 1: Load the complete dataset, as it contains meausrements for february and may already.
source("path_file_lilian.R")

Full_Data_Clean <- read_xlsx(paste0(path.data, "Biologger Clean data.xlsx"))

Full_Data_Clean <- Full_Data_Clean %>%
  mutate(
    Biologger = as.factor(Biologger),
    PIT = as.factor(`PIT tag`)
  )


str(Full_Data_Clean$Biologger)
str(Full_Data_Clean$PIT)

## Step 2: load the file with the measurements of November

Nov_Meas <- read_xlsx(paste0(path_meas, "November2024_Trout_Lammi.xlsx"))

Nov_Meas <- Nov_Meas %>%
  mutate(
    LengthNov = as.numeric(Length),
    WeightNov = as.numeric(Weight),
    `PIT tag` = tolower(PIT),
  )

## Step 3: merge the files together based on PIT tag 

Full_Meas_Datatest <- Full_Data_Clean %>%
  left_join(Nov_Meas, by = "PIT tag") %>%
  ##drop unnecessary columns
  dplyr::select(Biologger, `PIT tag`, AvgEA, LengthNov, LengthFeb, LengthMay, WeightNov, WeightFeb, WeightMay, food, greb1l, tank, fam)%>%
  group_by(Biologger)%>%
  summarize(
    AvgEA = mean(AvgEA),
    LengthNov = first(LengthNov),
    WeightNov = first(WeightNov),
    LengthFeb = first(LengthFeb),
    WeightFeb = first(WeightFeb),
    LengthMay = first(LengthMay),
    WeightMay = first(WeightMay),
    food = first(food),
    greb1l = first(greb1l),
    tank = first(tank),
    fam = first(fam)
  )


## Step 4: rearrange data to have only 3 columns (lenght, weight, and month)

Nov_Meas_Data <- Full_Meas_Datatest %>%
  dplyr::select(
    Biologger,
    AvgEA,
    LengthNov, 
    WeightNov,
    food,
    greb1l,
    tank,
    fam
    )%>%
  mutate(
    Month = "Nov",
  )%>%
  rename(
    Length = LengthNov,
    Weight = WeightNov,
  )
  
Feb_Meas_Data <- Full_Meas_Datatest %>%
  dplyr::select(
    Biologger,
    AvgEA,
    LengthFeb, 
    WeightFeb,
    food,
    greb1l,
    tank,
    fam
  )%>%
  mutate(
    Month = "Feb",
  )%>%
  rename(
    Length = LengthFeb,
    Weight = WeightFeb,
  ) 

May_Meas_Data <- Full_Meas_Datatest %>%
  dplyr::select(
    Biologger,
    AvgEA,
    LengthMay, 
    WeightMay,
    food,
    greb1l,
    tank,
    fam
  )%>%
  mutate(
    Month = "May",
  )%>%
  rename(
    Length = LengthMay,
    Weight = WeightMay,
  ) 


Full_Meas_Data_Clean <- Nov_Meas_Data %>%
  bind_rows(Feb_Meas_Data, May_Meas_Data)%>%
  mutate(
    Month = as.character(Month),
    Biologger = as.factor(Biologger),
    greb1l = as.factor(greb1l),
    tank = as.factor(tank),
    fam = as.factor(fam),
    food = as.factor(food),
    
  )%>%
  filter(
    Length != "NA",
    Weight != "NA",
    Biologger != "UXL0471DAT", ## This fish was shorter and lighter in may than in february, and I noted it was infected with flavio, so I exlcuded it from the measurements
  )
    
    
Full_Meas_Data_Clean$Month <- dplyr::recode(Full_Meas_Data_Clean$Month, "Nov" = "2024-11-11",  "Feb" = "2025-02-05",  "May" = "2025-05-17")
    
    
str(Full_Meas_Data_Clean$Month)
str(Full_Meas_Data_Clean$Biologger)

Full_Meas_Data_Clean <- Full_Meas_Data_Clean %>%
  mutate(
    Month = as.Date(Month)
  )

n_distinct(Full_Meas_Data_Clean$Biologger)


## Step 5: Plot the data 

ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = log(Length), col=greb1l, group = Biologger),
) +
  facet_wrap(~food)+
  geom_point(alpha=.5)+
  geom_path()+
  theme(legend.position = "none")


ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = log(Weight), col=greb1l, group = Biologger),
) +
  facet_wrap(~food)+
  geom_point(alpha=.5)+
  geom_path()+
  theme(legend.position = "none")

## Looks like there is a greb1l effect in food-restricted ;/ 

Full_Meas_Data_Clean$greb1l <- dplyr::recode(Full_Meas_Data_Clean$greb1l, "Allele 1" = "Summer",  "Allele 2" = "Winter")
Full_Meas_Data_Clean$food <- dplyr::recode(Full_Meas_Data_Clean$food, "high-food" = "Ad Libitum",  "low-food" = "Restricted")




##There is a trend, see what the GLMM says


Weight <- glmmTMB(log(Weight) ~ Month*greb1l*food + (1| tank) + (1 | fam) + (1| Biologger),
                  family = gaussian(),
                  data = Full_Meas_Data_Clean)

drop1(Weight)

Weight <- glmmTMB(log(Weight) ~ (Month+greb1l)*food + (1| tank) + (1 | fam) + (1| Biologger), ## <= BEST MODEL
                      family = gaussian(),
                      data = Full_Meas_Data_Clean)

performance(Weight)
Weightqr <- simulateResiduals(fittedModel = Weight, plot = FALSE)
Weightqr$scaledResiduals
plotQQunif(Weightqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)

summary(Weight)

## The food*greb1l interaction is not significant, but removing it increases AIC, let's check what's happening here

pairs(regrid(emmeans(Weight, ~ greb1l | food, type = "response")), ajust="Tukey")

## Looks like there is quite a difference in the food-restricted group

## Same analysis with Length in cm

Length <- glmmTMB(log(Length/10) ~ Month*greb1l*food + (1| tank) + (1 | fam) + (1| Biologger),
                  family = gaussian(),
                  data = Full_Meas_Data_Clean)

drop1(Length) ## stepwise model selection based on AIC, rerun after droping each term

Length <- glmmTMB(log(Length/10) ~ (Month+greb1l)*food + (1 | tank) + (1 | fam) + (1| Biologger), ## <= BEST MODEL
                  family = gaussian(), 
                  data = Full_Meas_Data_Clean)

performance(Length)
Lengthqr <- simulateResiduals(fittedModel = Length, plot = FALSE)
Lengthqr$scaledResiduals
plotQQunif(Lengthqr, 
           testUniformity = TRUE,   ## Uniformity test significant, not good but not much we can do about that
           testOutliers = TRUE, 
           testDispersion = TRUE)

summary(Length)
## food*greb1l same as with weight, ns but meaningful
pairs(emmeans(Length, ~ greb1l | food, type = "response"), ajust="Sidak")

## plot data

length <- ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Length/10, col=greb1l),
) +
  facet_wrap(~food)+
  geom_point(size=2.3, alpha=.5)+
  #geom_violin(aes(fill=greb1l))+
  scale_color_manual(values = c("gold2", "royalblue"))+
  geom_smooth(method = "lm")+
  theme_bw(base_size = 20)+
  theme(axis.title.x = element_text(size=20, face = "bold", margin = margin(t=15)),
        axis.title.y = element_text(size=20, face = "bold", margin = margin(r=15)),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 18, face = "italic"),
        legend.text  = element_text(size = 16,face = "italic"),
        legend.position = "none"
  )+
  labs(x = "Date", y="Body length (cm)")+
  scale_x_date(limits = as.Date(c("2024-11-01", "2025-05-31")), breaks = as.Date(c("2024-11-01", "2025-02-01", "2025-05-01")), date_labels = "%b %y")

mass <- ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Weight, col=greb1l),
) +
  facet_wrap(~food)+
  geom_point(size=2.3, alpha=.5)+
  #geom_violin(aes(fill=greb1l))+
  scale_color_manual(values = c("gold2", "royalblue"))+
  geom_smooth(method = "lm")+
  theme_bw(base_size = 16)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, face = "bold", margin = margin(r=15)),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 18, face = "italic"),
        legend.text  = element_text(size = 16,face = "italic"),
        legend.position = "none"
  )+
  labs(x = "Date", y="Body mass (g)")+
  scale_x_date(limits = as.Date(c("2024-11-01", "2025-05-31")), breaks = as.Date(c("2024-11-01", "2025-02-01", "2025-05-01")), date_labels = "%b %y")

plot <- plot_grid(mass, length, 
          labels = c("A", "B"),
          label_size = 28,
          nrow = 2,
          align = "hv")

legend <- get_legend(length+theme(legend.position = "right"))

plot_grid(plot, legend,
                     ncol = 2,
                     rel_widths = c(1, 0.15))

