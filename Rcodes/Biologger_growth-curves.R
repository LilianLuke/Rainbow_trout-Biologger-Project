### This script is to tidy body measurement data and plot growth curves ###


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
  mapping = aes(x = Month, y = Length, col=greb1l, group = Biologger),
) +
  facet_wrap(~food)+
  geom_point(alpha=.5)+
  geom_path()+
  theme(legend.position = "none")


ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Weight, col=greb1l, group = Biologger),
) +
  facet_wrap(~food)+
  geom_point(alpha=.5)+
  geom_path()+
  theme(legend.position = "none")

## Looks like there is a greb effect in food-restricted ;/ 

Full_Meas_Data_Clean$greb1l <- dplyr::recode(Full_Meas_Data_Clean$greb1l, "Allele 1" = "Summer",  "Allele 2" = "Winter")
Full_Meas_Data_Clean$food <- dplyr::recode(Full_Meas_Data_Clean$food, "high-food" = "Ad Libitum",  "low-food" = "Restricted")




##There is a trend, see what the GLMM says with month+greb1l*food


Weight <- glmmTMB(log(Weight) ~ Month*greb1l*food,
                  family = gaussian(),
                  data = Full_Meas_Data_Clean)

step(Weight)

Weight <- glmmTMB(log(Weight) ~ (Month+greb1l)*food + (1 | tank) + (1 | fam) + (1| Biologger),
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
pairs(regrid(emmeans(Weight, ~ greb1l | food, type = "response")), ajust="Tukey")



ggplot(data=Full_Meas_Data_Clean, aes(y=Weight, x=Month, group = tank, col= tank))+ 
  facet_wrap(~food)+
  geom_point()+
  xlab("AvgEA (mg)") + 
  ylab("Residuals")+ 
  geom_smooth(method = "lm", 
              se = FALSE)



Length <- glmmTMB(log(Length/10) ~ Month*greb1l*food,
                  family = gaussian(),
                  data = Full_Meas_Data_Clean)

step(Length)
# Step() does not work if I add random factors, so I do it without first and it usually works
# In this case, I find that when including random factors, it is better to keep Month:Food, even though the Step() says it should be dropped
Length <- glmmTMB(log(Length/10) ~ (Month+greb1l)*food + (1 | tank) + (1 | fam) + (1| Biologger),
                  family = gaussian(), 
                  data = Full_Meas_Data_Clean)

performance(Length)
Lengthqr <- simulateResiduals(fittedModel = Length, plot = FALSE)
Lengthqr$scaledResiduals
plotQQunif(Lengthqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)

summary(Length)
pairs(emmeans(Length, ~ greb1l | food, type = "response"),ajust="Tukey")

par(mfrow = c(2, 1)) 

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
## Note: greb1l:food, p = 0.232 and 0.0744 for weight and length, respectively


Full_Meas_Data_LF <- Full_Meas_Data_Clean %>%
  filter(
    food == "Low Food"
  )


Weight <- glmmTMB(log(Weight) ~ Month+greb1l + (1 | tank) + (1 | fam) + (1| Biologger),
                  family = gaussian(),
                  data = Full_Meas_Data_LF)

summary(Weight)

Length <- glmmTMB(log(Length) ~ Month+greb1l + (1 | tank) + (1 | fam) + (1| Biologger),
                  family = gaussian(), 
                  data = Full_Meas_Data_LF)

summary(Length)


Full_Meas_Data_Clean$food <- dplyr::recode(Full_Meas_Data_Clean$food, "High Food" = "Ad Libitum",  "Low Food" = "Food Restricted")


Full_Meas_Data_Clean <- Full_Meas_Data_Clean %>%
  mutate(
    Month = as.factor(Month)
  )

par(mfrow = c(2, 1) ) 

p1 <- ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Weight, fill=greb1l),
) +
  facet_wrap(~food)+
  geom_boxplot()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("gold2", "royalblue"), name = "greb1l", labels = c("Summer", "Winter"))+
  scale_x_discrete(name = "Month", labels = c("Nov 2024", "Feb 2025", "May 2025"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14,face = "italic"),
  )+
  labs(y="Weight (g)")



p2 <- ggplot(
  data = Full_Meas_Data_Clean, 
  mapping = aes(x = Month, y = Length/10, fill=greb1l),
) +
  facet_wrap(~food)+
  geom_boxplot()+
  theme_bw(base_size = 16)+
  scale_fill_manual(values = c("gold2", "royalblue"), name = "greb1l", labels = c("Summer", "Winter"))+
  scale_x_discrete(name = "Month", labels = c("Nov 2024", "Feb 2025", "May 2025"))+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=14),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "italic"),
        legend.text  = element_text(size = 14,face = "italic"),
  )+
  labs(y="Length (cm)")


grid.arrange(p1, p2, ncol = 1)

Gen_Meas_Data <- Full_Meas_Data_Clean%>%
  group_by(greb1l, food, Month)%>%
  summarise(
    Length = mean(Length),
    Weight = mean(Weight),
  )


ggplot(
  data = Gen_Meas_Data, 
  mapping = aes(x = Month, y = Length, col=greb1l, group = greb1l),
) +
  facet_wrap(~food)+
  geom_point(alpha=.5)+
  geom_path()+
  theme(legend.position = "none")+
  scale_color_manual(values = c("gold2", "royalblue"))

