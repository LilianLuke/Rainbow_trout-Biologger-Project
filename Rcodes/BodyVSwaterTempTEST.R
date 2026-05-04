### Quickly check if biologger temp and tank temp match or if fish seem to express some sort of preference



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
  bind_rows(Temp_Tank31) %>%
  bind_rows(Temp_Tank32) %>%
  mutate(
    Date = format(Date_Time,"%Y-%m-%d"),
    Time = format(Date_Time, "%H:%M:%S"),
    Date = as.Date(Date),
  )%>%
  rename(
    DateTime = Date_Time,
    tank = Tank,
    WaterTemp=Temp
  )%>%
  filter(
    Date > "2024-12-01",
    Date < "2025-05-24"
  )%>%
  relocate(tank)%>%
  relocate(DateTime, .after = last_col())%>%
  select(WaterTemp, tank, DateTime)

str(Full_Temp)


Full_Data <- read_xlsx(paste0(path.data, "Biologger final data.xlsx"))

Full_Data <- Full_Data %>%
  mutate(
    greb1l = as.factor(greb1l),
    `PIT tag` = as.factor(`PIT tag`),
    Biologger = as.factor(Biologger),
    Time= lubridate::hms(Time),
  )%>%
  select(Biologger, 
         greb1l, 
         food, 
         `PIT tag`, 
         AvgEA, 
         MaxEA, 
         VAR, 
         SGRMass, 
         Temp, 
         tank,
         fam,
         Date, 
         Time, 
         DateTime,
         )%>%
  rename(
    BodyTemp=Temp,
  )
  
str(Full_Data)




Full_Data_Temp <- Full_Data %>%
  left_join(Full_Temp, by = c("DateTime", "tank"))%>%
  filter(tank != "21")



ggplot(data=Full_Data_Temp, aes(y= BodyTemp, x=WaterTemp))+
  facet_wrap(~tank)+
  geom_point(alpha=.01)+
  geom_smooth(method="lm")


ggplot(Full_Data_Temp, aes(x = DateTime)) +
  facet_wrap(~Date)+
  geom_line(aes(y = WaterTemp))+
  geom_point(alpha=.1, aes(y = BodyTemp, col=greb1l))+
  geom_smooth(method ="gam", aes(y = BodyTemp, col=greb1l))+
  geom_line(aes(y = BodyTemp, col=greb1l, group = Biologger))+
  scale_y_continuous(
    name = "Water Temperature (°C)",
    sec.axis=sec_axis(transform= ~.*1, name = "Body Temperature (°C)")
  )+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))+
  theme_bw()






Temp <- glmmTMB(BodyTemp ~ WaterTemp*greb1l*food + (1 | tank) + (1 | fam) + (1| Biologger),
                  family = gaussian(),
                  data = Full_Data_Temp)
summary(Temp)
Tempqr <- simulateResiduals(fittedModel = Temp, plot = FALSE)
Tempqr$scaledResiduals
plotQQunif(Tempqr, 
           testUniformity = TRUE, 
           testOutliers = TRUE, 
           testDispersion = TRUE)


hist(resid(Temp), breaks = 500)

Full_Data_Temp$corr_BodyTemp <- resid(Temp)
  

ggplot(data=Full_Data_Temp, aes(y= BodyTemp-WaterTemp, x=WaterTemp, col=greb1l))+
  facet_wrap(~food)+
  geom_point(alpha=.01)+
  geom_smooth(method="lm")+
  scale_color_manual(values = c("gold2", "royalblue"), name = "greb1L", labels = c("Summer", "Winter"))
  