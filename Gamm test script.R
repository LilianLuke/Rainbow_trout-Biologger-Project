library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(ggthemes)
library(lubridate)
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


rm(list=ls(all=TRUE));gc()

Loggers <- read_excel("Rtest_results.xlsx")
Loggers$Time <- as.numeric(Loggers$Time)


model <- gamm(Loggers$AvgEA ~ s(Time)+Date*photoperiod*food*greb, 
              random = list(Temp = ~ 1, `Source`= ~1),
              data = Loggers)


summary(model$gam)
summary(model$lm)


dredge(model$lm)

plot(model$gam)
plot(model$lm)

ggplot(
  data = Loggers, 
  mapping = aes(x = `Time`, y = `AvgEA`)) +
  geom_point() +
  geom_smooth() +
  ylim(0, 30)
