## This script is for testing VAR threshold based on the distrib of count burst data
Full_Data_Out_TEST <- Full_Data_Out %>%
  mutate(
    BurstScore = as.numeric(if_else(
      VAR < 220,
      0,
      1,
    ))
    )
    
    

PhysioActData <- Full_Data_Out%>%  
  group_by(Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    PercentDeltaFulton = first(PercentDeltaFulton),
    Tempgroup = first(Tempgroup),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    SGRMass = first(SGRMass),
    LengthFeb = first(LengthFeb),
    LengthMay = first(LengthMay),
    `Heart weight (g)` = first(`Heart weight (g)`),
    HtoBratio = first(HtoBratio),
  )%>%  
  filter(
    Biologger != "UXL0552DAT",
  ) 


#hist(PhysioActData$count_BurstScore)
plot(PhysioActData$count_BurstScore)







TempBurst <- Full_Data_Out_TEST%>%  
  group_by(Tempgroup, Biologger) %>%
  summarise(
    mean_BurstScore = mean(BurstScore, na.rm = TRUE),
    Month = first(Month),
    Biologger = first(Biologger),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    Season = first(Season),
    LengthFeb = first(LengthFeb)
  )

CountBurstMod <- lme(mean_BurstScore ~ (Tempgroup+greb1l)*food,
                     method="ML",
                     random=list(fam=~1, tank=~1),
                     data = TempBurst)
dd<- dredge(CountBurstMod)
modav <- model.avg(dd,delta<2)
summary(modav)
anova(CountBurstMod)

hist(resid(CountBurstMod))
qqPlot(resid(CountBurstMod))
summary(CountBurstMod)

predict.CountBurstMod <- ggpredict(CountBurstMod, terms = c("Tempgroup","greb1l", "food"), ci.lvl = 0.95)
plot(predict.CountBurstMod, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1)


EMM <- emmeans(CountBurstMod, ~greb1l*food, data=TempBurst)
pairs(EMM)


BmodelAvg <- lme(SGRmass ~ count_BurstScore*food*greb1l,
                  method="ML",
                  random=list(fam=~1, tank=~1),
                  data = PhysioActData)

dd<- dredge(BmodelAvg)
modav <- model.avg(dd,delta<2)
summary(BmodelAvg)

predict.B_AvgEA <- ggpredict(BmodelAvg, terms = c("count_BurstScore","food"), ci.lvl = 0.95)
plot(predict.B_AvgEA, colors = c("#E96C0CFF",  "#420A68FF"), line_size = 1.5)

ggplot(
  data = PhysioActData, 
  mapping = aes(x = count_BurstScore, y = PercentDeltaFulton),
) +
  geom_point(aes(fill = greb1l, color = food), shape = 21, size = 2)+
  scale_fill_manual(values = c("grey20", "gray70")) +  
  scale_color_manual(values = c("blue", "red")) +   
  
  geom_smooth()


ggplot(
  data = PhysioActData, 
  mapping = aes(x = count_BurstScore, y = PercentDeltaFulton),
) +
  geom_point(aes(fill = greb1l, color = food), shape = 21, size = 2)+
  scale_fill_manual(values = c("grey20", "gray70")) +  
  scale_color_manual(values = c("blue", "red")) +   
  geom_smooth()




PhysioActDataLF <- PhysioActData %>%
  filter(
    food=="low-food"
  )

PhysioActDataHF <- PhysioActData %>%
  filter(
    food=="high-food"
  )


HT_Data <- Full_Data_Out %>%
  filter(
    Tempgroup == "LowTemp"
  )

PhysioActDataHT <- HT_Data%>%  
  group_by(Biologger) %>%
  summarise(
    mean_AvgEA = mean(AvgEA, na.rm = TRUE),
    mean_MaxEA = mean(MaxEA, na.rm = TRUE),
    mean_VAR = mean(VAR, na.rm = TRUE),
    count_BurstScore = sum(BurstScore, na.rm = TRUE),
    PercentDeltaFulton = first(PercentDeltaFulton),
    Tempgroup = first(Tempgroup),
    greb1l = first(greb1l),
    food = first(food),
    fam = first(fam),
    tank = first(tank),
    SGRMass = first(SGRMass),
    LengthFeb = first(LengthFeb),
    LengthMay = first(LengthMay),
    `Heart weight (g)` = first(`Heart weight (g)`),
    HtoBratio = first(HtoBratio),
  )%>%  
  filter(
    Biologger != "UXL0552DAT",
  ) 

BmodelAvg <- lme(PercentDeltaFulton ~ count_BurstScore+food, ## best model
                 method="ML",
                 random=list(fam=~1, tank=~1),
                 data = PhysioActData)
summary(BmodelAvg)

t.test(PhysioActDataHF$PercentDeltaFulton~PhysioActDataHF$greb1l, alternative="greater")

ggplot(
  data = PhysioActData, 
  mapping = aes(x = count_BurstScore, y = PercentDeltaFulton, col=greb1l),
) +
  facet_wrap(~food)+
  geom_point(size = 2)+ 
  geom_smooth()




ggplot(
  data = PhysioActData, 
  mapping = aes(x = greb1l, y = PercentDeltaFulton),
) +
  facet_wrap(~food)+
  geom_boxplot(aes(fill=greb1l))
