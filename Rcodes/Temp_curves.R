### Get temp curves from the different tanks



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

Temp_Tank22 <- read_xlsx(paste0(path.temp, "lammi2025October_Tank_22.xlsx")) 
Temp_Tank23 <- read_xlsx(paste0(path.temp, "lammi2025October_Tank_23.xlsx")) 
Temp_Tank24 <- read_xlsx(paste0(path.temp, "lammi2025October_Tank_24.xlsx")) 
Temp_Tank25 <- read_xlsx(paste0(path.temp, "lammi2025October_Tank_25.xlsx")) 
Temp_Tank26 <- read_xlsx(paste0(path.temp, "lammi2025October_Tank_26.xlsx"))

Temp_Tank22 <- Temp_Tank22 %>%
  mutate(
    Tank = as.factor(22),
  ) %>%
  rename( 
    Temp = `Temp, °C (LGR S/N: 20291667, SEN S/N: 20291667)`,
    Date_Time = `Date Time, GMT+02:00`
    )

Temp_Tank23 <- Temp_Tank23 %>%
  mutate(
    Tank = as.factor(23),
  ) %>%
  rename( 
    Temp = `Temp, °C (LGR S/N: 20174578, SEN S/N: 20174578)`,
    Date_Time = `Date Time, GMT+02:00`
  )


Temp_Tank24 <- Temp_Tank24 %>%
  mutate(
    Tank = as.factor(24),
  ) %>%
  rename( 
    Temp = `Temp, °C (LGR S/N: 20291686, SEN S/N: 20291686)`,
    Date_Time = `Date Time, GMT+02:00`
  )


Temp_Tank25 <- Temp_Tank25 %>%
  mutate(
    Tank = as.factor(25),
  ) %>%
  rename( 
    Temp = `Temp, °C (LGR S/N: 20174549, SEN S/N: 20174549)`,
    Date_Time = `Date Time, GMT+02:00`
  )


Temp_Tank26 <- Temp_Tank26 %>%
  mutate(
    Tank = as.factor(26),
  ) %>%
  rename( 
    Temp = `Temp, °C (LGR S/N: 20291671, SEN S/N: 20291671)`,
    Date_Time = `Date Time, GMT+02:00`
  )




Full_Temp <- Temp_Tank22 %>%
  bind_rows(Temp_Tank23) %>%
  bind_rows(Temp_Tank24) %>%
  bind_rows(Temp_Tank25) %>%
  bind_rows(Temp_Tank26) %>%
  mutate(
    Date = format(Date_Time,"%Y-%m-%d"),
    Time = format(Date_Time, "%H:%M:%S"),
    Date = as.Date(Date),
    Date_Time = as.POSIXct(Date_Time)
  )%>%
  filter(
    Date > "2024-12-01",
    Date < "2025-05-24"
  )%>%
  relocate(Tank)%>%
  relocate(Date_Time, .after = last_col())

Full_Temp$Tank <- dplyr::recode(Full_Temp$Tank, "23" = "2",  "25" = "3", "22" = "4",  "24" = "5",  "26" = "6")



Full_TempTEST <- Full_Temp %>%
  mutate(
    Date_Time = as.numeric(Date_Time)
  )
  
  
ggplot(
  data = Full_Temp, 
  mapping = aes(x = Date_Time, y = Temp, col=Tank),
) +
  #facet_wrap(~Tank)+
  geom_line()+
  ylim(0,11)+
  theme_bw()+
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%d/%m/%Y")+
  theme(axis.title.x = element_text(size=18, face = "bold"),
        axis.title.y = element_text(size=18, face = "bold"),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=18),
        strip.text   = element_text(size = 20, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 20, face = "bold"),
        legend.text  = element_text(size = 18)
  )+
  scale_color_colorblind(breaks=c(2, 3, 4, 5, 6))+
  labs(y="Water temperature (°C)", x="Date & Time")

