## This script is for plotting distribution histograms for activity data


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
  ## Create a DateTime column to plot all data over the measurement period
  mutate(
    Date = ymd(Date),
    Time = hms(Time),
  )



setwd("C:/Users/03278487/OneDrive - Valtion/General - ThermoEvo/Lilian data/R outputs/Plots")

## plot data distribution histograms for every fish
output_dir <- "Biologger_Histograms"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

setwd("C:/Users/03278487/OneDrive - Valtion/General - ThermoEvo/Lilian data/R outputs/Plots/Biologger_Histograms")

output_dirAvg <- "Histograms_AvgEA"
if (!dir.exists(output_dirAvg)) {
  dir.create(output_dirAvg)
}

loggernb <- unique(Full_Data_Out$Biologger)

for (id in loggernb) {
  # Subset data for the current biologger
  sub_data <- Full_Data_Out[Full_Data_Out$Biologger == id, ]
  
  # create a png file
  png(filename = file.path(output_dirAvg, paste0(id, "_hist.png")), width = 800, height = 600)
  
  # Make AvgEA hist
  hist(sub_data$`AvgEA`,
       main = paste("Histogram for Biologger", id),
       xlab = "AvgEA",
       breaks = 50,
       xlim = c(0,100),
       ylim = c(0,700)
  )
  
  dev.off()
}

hist(Full_Data_Out$`MaxEA`,
     main = paste("MaxEA distribution"),
     xlab = "MaxEA",
     breaks = 50,
     xlim = c(0,3000),
     ylim = c(0, 5000)
       
)

hist(Full_Data_Out$`AvgEA`,
     main = paste("AvgEA distribution"),
     xlab = "AvgEA",
     breaks = 125,
     xlim = c(12,100),
     ylim = c(0, 200)
     
)

