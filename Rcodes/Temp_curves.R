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
Temp_Tank31 <- read_xlsx(paste0(path.temp, "lammi2025October_Tank_31.xlsx"))
Temp_Tank32 <- read_xlsx(paste0(path.temp, "lammi2025October_Tank_32.xlsx"))

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

Temp_Tank31 <- Temp_Tank31 %>%
  mutate(
    Tank = as.factor(31),
  ) %>%
  rename( 
    Temp = `Temp, °C (LGR S/N: 20174562, SEN S/N: 20174562)`,
    Date_Time = `Date Time, GMT+02:00`
  )%>%
  mutate(
    Date = format(Date_Time,"%Y-%m-%d"),
    Time = format(Date_Time, "%H:%M:%S"),
    Date = as.Date(Date),
    Date_Time = as.POSIXct(Date_Time)
  )%>%
  filter(
    Date > "2024-12-12",
    Date < "2025-01-06"
  )

Temp_Tank32 <- Temp_Tank32 %>%
  mutate(
    Tank = as.factor(32),
  ) %>%
  rename( 
    Temp = `Temp, °C (LGR S/N: 20291672, SEN S/N: 20291672)`,
    Date_Time = `Date Time, GMT+02:00`
  )%>%
  mutate(
    Date = format(Date_Time,"%Y-%m-%d"),
    Time = format(Date_Time, "%H:%M:%S"),
    Date = as.Date(Date),
    Date_Time = as.POSIXct(Date_Time)
  )%>%
  filter(
    Date > "2024-12-12",
    Date < "2025-01-06"
  )



Full_Temp <- Temp_Tank22 %>%
  bind_rows(Temp_Tank23) %>%
  bind_rows(Temp_Tank24) %>%
  bind_rows(Temp_Tank25) %>%
  bind_rows(Temp_Tank26) %>%
  #bind_rows(Temp_Tank31) %>%
  #bind_rows(Temp_Tank32) %>%
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

Full_Temp$Tank <- dplyr::recode(Full_Temp$Tank, "23" = "2",  "25" = "3", "22" = "4",  "24" = "5",  "26" = "6",  
                                #"31" = "R1",  "32" = "R2"
                                )

Full_Temp$Tank <- factor(Full_Temp$Tank, levels = c(
  #"R1", "R2", 
  "2", "3", "4", "5","6"))
  
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
  scale_color_colorblind(breaks=c(
    #"R1", "R2", 
    2, 3, 4, 5, 6))+
  labs(x="Date & Time", y="Water temperature (°C)")




### Import suntable obtained from NOAA solar calculator (and formated in excel)
sun_table <- read_xlsx("C:/Users/03278487/OneDrive - Luonnonvarakeskus/PhD projects/Biolgger paper/Manuscript/NOAA_SunTimes.xlsx")
sun_table <- sun_table %>%
  select(Date, Sunrise, Sunset)%>%
  mutate(Date=as.Date(Date))

str(sun_table)
#Add the sunrise and sunset to the main dataframe
Full_Temp_DL <- Full_Temp %>%
  left_join(sun_table, by = "Date")

#Convert times to a time format and rename columns
Full_Temp_DL <- Full_Temp_DL %>% 
  mutate(
    Sunrise = hm(Sunrise),
    Sunset = hm(Sunset),
  )%>%
  rename(
    NOAA_Sunrise = Sunrise,
    NOAA_Sunset = Sunset,
  )

str(Full_Temp_DL$Time)

### Calculate Daylength
Full_Temp_DL <- Full_Temp_DL %>% 
  mutate(
    NOAA_Sunrise = as.numeric(NOAA_Sunrise),
    NOAA_Sunset = as.numeric(NOAA_Sunset),
    DayLength = difftime(NOAA_Sunset, NOAA_Sunrise, units = "hours")
  )%>%
  mutate(
    NOAA_Sunrise = hms::hms(NOAA_Sunrise),
    NOAA_Sunset = hms::hms(NOAA_Sunset),
    Time = hms::hms(hms(Time)),
    Light = as.factor(ifelse(Time < NOAA_Sunrise, 
                             "Night",
                             ifelse(Time < NOAA_Sunset,
                                    "Day",
                                    "Night")
                             )
                      )
  )



str(Full_Temp_DL)


DayLengthPlot <- Full_Temp_DL%>%
  group_by(Date)%>%
  summarise(
    DayLength=first(DayLength),
    NightLength = 24-DayLength,
    Date=first(Date),
  ) %>%
  pivot_longer(
    cols = c(DayLength, NightLength),
    names_to = "Period",
    values_to = "hours"
  )%>%
  filter(
    Date < "2025-05-21"
  )

DayLengthPlot$Period <- dplyr::recode(DayLengthPlot$Period, "DayLength" = "Day",  "NightLength" = "Night")
DayLengthPlot$Period <- factor(DayLengthPlot$Period, levels = c("Night", "Day"))

Full_Temp_DL <- Full_Temp_DL %>%
  filter(
    Date < "2025-05-20"
  )
  

Scale_factor = 23/max(Full_Temp_DL$Temp)


ggplot(DayLengthPlot, aes(x = Date)) +
  geom_col(aes(width=87000, y = hours, fill = Period)) +
  scale_fill_manual(breaks = c("Day", "Night"), values = c("Day" = "#FFEBCD", "Night" = "midnightblue")) +
  geom_line(data=Full_Temp_DL, aes(x = Date_Time, y = Scale_factor*Temp, col = Tank))+
  scale_y_continuous(
    name = "Day length (h)",
    sec.axis=sec_axis(transform = ~./Scale_factor, name = "Temperature (°C)"),
    expand = c(0, 0)
    )+
  theme_bw()+
  scale_x_datetime(date_breaks = "3 weeks", date_labels = "%d/%m/%Y", expand = c(0,0))+
  theme(axis.title.x = element_text(size=18, face = "bold", margin = margin(t=15)),
        axis.title.y = element_text(size=18, face = "bold", margin = margin(r=15)),
        axis.title.y.right = element_text(size=18, face = "bold", margin = margin(l=15)),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=16),
        legend.position = "bottom",
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16)
  )+
  labs(x="Date")+
  geom_vline(xintercept = c(as.POSIXct("2024-12-09"),as.POSIXct("2024-12-12"), as.POSIXct("2025-01-14")), color=c("red", "red", "green"))

