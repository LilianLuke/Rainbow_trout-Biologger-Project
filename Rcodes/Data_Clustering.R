### This script is to keep a history of the methods used for clustering ###


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
library(flexmix)
library(mclust)

source("path_file_lilian.R")

Full_Data_Clean <- read_xlsx(paste0(path.data, "Biologger Clean data.xlsx"))


### Convert covariates to factor with n levels
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
    !AvgEA == MaxEA, ### Removes all the data point with 2spm (we only use those with 60spm)
  )



### make sure all covariates are considered correctly
str(Full_Data_Out$Biologger)
str(Full_Data_Out$fam)
str(Full_Data_Out$greb1l)
str(Full_Data_Out$omy05)
str(Full_Data_Out$tank)
str(Full_Data_Out$Date)
str(Full_Data_Out$Time)
str(Full_Data_Out$DateTime)



### Create a log column for each ACT parameter to be able to run GAMMS on it
Full_Data_Out <- Full_Data_Out %>%
  group_by(Biologger) %>%
  mutate(
    LogAvgEA = as.numeric(log10(`AvgEA`+0.001)),
    LogMaxEA = as.numeric(log10(`MaxEA`+0.001)),
    LogVAR = as.numeric(log10(VAR+0.76)),
  )

### Add a BurstScore column to be able to calculate the frequency of individuals bursting or steady-swimming
Full_Data_Out <- Full_Data_Out %>%
  mutate(
    BurstScore = as.factor(if_else(
      VAR < 190, ### ADJUST VALUE
      0,
      1,
    )),
    ##Add a SwimStyle column to be able to split data based on bursting
    Swimstyle = as.factor(if_else(
      VAR < 190,
      "Steady",
      "Dynamic",
    )),
    ## Add a Tempgroup column to be able to split data based on the temp threshold identified when looking at VAR/Temp
    Tempgroup = as.factor(if_else(
      Temp < 4.5,
      "LowTemp",
      "HighTemp"
    )),
    corr.AvgEA = as.numeric(AvgEA-MaxEA/60),
    corr.VAR = as.numeric(VAR-(MaxEA-(MaxEA/60))^2/60),
    th.VAR = as.numeric(((MaxEA-(10))^2+59*(0-(10))^2)/60),
  ) %>%
  filter(
    Biologger != "UXL0471DAT",
  )

str(Full_Data_Out$BurstScore)
str(Full_Data_Out$Swimstyle)
str(Full_Data_Out$Tempgroup)

### Force the plots to show winter or low-temp on the left and spring or high-temp on the right
Full_Data_Out$Season <- factor(Full_Data_Out$Season, levels = c("Winter", "Spring"))
Full_Data_Out$Tempgroup <- factor(Full_Data_Out$Tempgroup, levels = c("LowTemp", "HighTemp"))




ggplot(Full_Data_Out, aes(x = MaxEA, y = AvgEA)) +
  geom_point(alpha=0.5) +
  labs(title = "What do we use to determine swimming type ?", x = "Maximum Acceleration", y = "Average Acceleration") +
  #geom_line(aes(x= MaxEA, y= MaxEA/60, col="red"), linewidth=1)+
  geom_abline(intercept = 0, slope = 1/60, color="red")+
  geom_abline(intercept = 10, slope =  0, color="blue")

ggplot(Full_Data_Out, aes(x = MaxEA, y = AvgEA-MaxEA/60)) +
  geom_point(alpha=0.1) +
  labs(title = "What do we use to determine swimming type ?", x = "Maximum Acceleration", y = "Corrected Average Acceleration") +
  geom_abline(intercept = 0, slope = 0, color="red")+
  geom_abline(intercept = 10, slope = 0, color="blue")


ggplot(Full_Data_Out, aes(x = MaxEA, y = VAR, col=Swimstyle)) +
  geom_point(alpha=0.1) +
  labs(title = "What do we use to determine swimming type ?", x = "Maximum Acceleration", y = "Corrected Average Acceleration")+
  geom_line(aes(x= MaxEA, y= th.VAR), color="red", linewidth=1)





ggplot(Full_Data_Out, aes(x = MaxEA, y = AvgEA, col=Swimstyle)) +
  #facet_wrap(~Biologger)+
  geom_point(alpha=0.1) +
  labs(title = "What do we use to determine swimming type ?", x = "Maximum Acceleration", y = "Average Acceleration") +
  geom_abline(intercept = 0, slope = 1/60, color="red")+
  geom_abline(intercept = 0, slope = 1, color="red")+
  #geom_abline(intercept = 10, slope = 0, color="blue")+
  xlim(0,100)+
  ylim(0,50)





Threshold_Data <- Full_Data_Out %>%
  filter(
    VAR >= 189,
    VAR <= 191,
    
  )

hist(Threshold_Data$VAR,
     xlab = "VAR for AvgEA = 10",
     breaks = 500,
     xlim = c(0,500),
     ylim = c(0,500))


mean(Threshold_Data$AvgEA)

ggplot(Full_Data_Out, aes(x = MaxEA, y = AvgEA, col=BurstScore)) +
  #facet_wrap(~Biologger)+
  geom_point(alpha=0.1) +
  labs(title = "What do we use to determine swimming type ?", x = "Maximum Acceleration", y = "Average Acceleration") +
  geom_abline(intercept = 0, slope = 1/60, color="red")+
  geom_abline(intercept = 0, slope = 1, color="red")+
  #geom_abline(intercept = 10, slope = 0, color="blue")+
  xlim(0,600)+
  ylim(0,40)
#geom_abline(intercept = 10, slope =  1/60, color="blue")


ggplot(Full_Data_Out, aes(x = AvgEA, y = VAR)) +
  geom_point(alpha=0.05) +
  labs(title = "What do we use to determine swimming type ?", x = "Average Acceleration", y = "Variance of acceleration")+
  #ylim(0, 450)+
  #xlim(0,20)
  #geom_abline(intercept = 10, slope =  1/60, color="blue")
  
  library(plotly)
plot_ly(data = Full_Data_Out, x = ~VAR, y = ~MaxEA, z = ~AvgEA, type = "scatter3d", size = 1, mode = "markers")

Clust <- Full_Data_Out[, c(6, 33, 34)]

Clust_scaled<- scale(Clust[, c("VAR", "AvgEA", "MaxEA")])

Clust_log <- log1p(Clust)

# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(clusterScale, centers = i, nstart = 10, iter.max = 100)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")



### Try with gaussian mixture model
gmm <- Mclust(Clust_scaled, G = 3)
summary(gmm)


Full_Data_Out$Cluster <- as.factor(gmm$classification)



ggplot(Full_Data_Out, aes(x = AvgEA, y = VAR, col=Cluster)) +
  #facet_wrap(~Biologger)+
  geom_point(alpha=0.1) +
  labs(title = "What do we use to determine swimming type ?", x = "Average Acceleration", y = "Variance") +
  geom_abline(intercept = 0, slope = 1/60, color="red")+
  geom_abline(intercept = 0, slope = 1, color="red")+
  #geom_abline(intercept = 10, slope = 0, color="blue")+
  xlim(0,30)+
  ylim(0,150)
#geom_abline(intercept = 10, slope =  1/60, color="blue")




### Try with 2 clusters
gmm <- Mclust(Clust_scaled, G = 2)
summary(gmm)


Full_Data_Out$Cluster <- as.factor(gmm$classification)



ggplot(Full_Data_Out, aes(x = MaxEA, y = AvgEA, col=Cluster)) +
  #facet_wrap(~Biologger)+
  geom_point(alpha=0.1) +
  labs(title = "What do we use to determine swimming type ?", x = "Maximum Acceleration", y = "Average Acceleration") +
  geom_abline(intercept = 0, slope = 1/60, color="red")+
  geom_abline(intercept = 0, slope = 1, color="red")+
  #geom_abline(intercept = 10, slope = 0, color="blue")+
  xlim(0,600)+
  ylim(0,40)
#geom_abline(intercept = 10, slope =  1/60, color="blue")




### Test the k-means clustering to estimate the VAR threshold
# Prep dataset for clustering (remove NAs and "factor" covariate)
Full_Data_Out_Cluster <- Full_Data_Out %>%
  ungroup()%>%
  select(
    VAR, MaxEA, AvgEA, 
  )
Full_Data_Out_Cluster_scaled <- scale(Full_Data_Out_Cluster[, c("VAR", "AvgEA", "MaxEA")])

anyNA(Full_Data_Out_Cluster)
any(is.nan(as.matrix(Full_Data_Out_Cluster)))
any(is.infinite(as.matrix(Full_Data_Out_Cluster)))

str(Full_Data_Out_Cluster$AvgEA)
str(Full_Data_Out_Cluster$VAR)
str(Full_Data_Out_Cluster$MaxEA)
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(Full_Data_Out_Cluster_scaled, centers = i, nstart = 10, iter.max = 100)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

#Could be 2 or 3 culsters



### Try with hierarchical clustering

hclust.out <- hclust(dist(Full_Data_Out$VAR)) #=> Error: cannot allocate vector of size 57.2 Gb


### Try with gaussian mixture model
gmm <- Mclust(Full_Data_Out_Cluster, G = 3)

# Add cluster assignment
Full_Data_Out_Cluster$Cluster <- as.factor(gmm$classification)
Full_Data_Out$Cluster <- as.factor(gmm$classification)


#Full_Data_Out_Cluster <- Full_Data_Out %>%
# mutate(
#  DateTime = as.character(DateTime)
# )
# Plot
ggplot(Full_Data_Out_Cluster, aes(x = MaxEA, y = VAR, col=Cluster)) +
  geom_point(alpha=0.2) +
  labs(title = "Gaussian Mixture Model: 3 Clusters", x = "", y = "Variance of Acceleration") +
  theme_minimal()+
  xlim(0,100)+
  ylim(0,200)

summary(gmm)

ggplot(Full_Data_Out, aes(x = MaxEA, y = corr.VAR)) +
  geom_point(alpha=0.2) +
  labs(title = "Gaussian Mixture Model: 3 Clusters", x = "", y = "Variance of Acceleration") +
  theme_minimal()+
  xlim(0,1500)


model2 <- flexmix(VAR ~ 1 | Biologger, data = Full_Data_OutTEST, k = 2, model = list(FLXMCmvcombi(VAR ~ .), FLXMCdist1(VAR ~ ., dist = "gamma")))

model3 <- flexmix(VAR ~ MaxEA | Biologger, data = Full_Data_Out, k = 3, model = list(FLXMCmvcombi(VAR ~ .), FLXMCdist1(VAR ~ ., dist = "gamma"), FLXMCdist1(VAR ~ ., dist = "gamma")))

summary(model2)

summary(model3)
plot(model3)

summary(model5)
