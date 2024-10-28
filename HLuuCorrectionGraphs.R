# HLuu
# August 23, 2023 

# Graph updated predictions using correct seed production equations 

# load the updated raw data and updated equation coefficients
setwd("/Users/HoangLuu/Desktop/HLCorrectionsForPublication")
library(readr)
LuuPNWSeed800v6b <- read_csv("LuuPNWSeed800v6b.csv")
SeedProductionEquation <- read_csv("SubsetEquationEstimates2023v2.csv")

# fill in NA with 0 in the seed production equation 
SeedProductionEquation[is.na(SeedProductionEquation)] <- 0

# create the prediction input table
MySpeciesList<- unique(SeedProductionEquation$species)

# data frame where DBH is a range and the others are the observed mean 
# create the prediction input table

#unique(MyPNWDF$species)

library(dplyr)

MyDBHPredictionDF <- data.frame()

for (i in 1:12) {
  MyDF <- subset(LuuPNWSeed800v6b, species == MySpeciesList[i])
  
  MyTestData <- data.frame(expand.grid(DBH = seq(min(MyDF$DBH), max(MyDF$DBH),.05)),
                                       SummerMeanTemp =  c(mean(MyDF$SprMinTemp)),
                                       WDa = c(mean(MyDF$WDa)),
                                       SprMinTemp = c(mean(MyDF$SprMinTemp)),
                                       SprminTempDifA = c(mean(MyDF$SprminTempDifA)),
                                       SprminTempDifB = c(mean(MyDF$SprminTempDifB)),
                                       SprminTempDifC = c(mean(MyDF$SprminTempDifC)),
                                       SummerMeanTempDifA = c(mean(MyDF$SummerMeanTempDifA)),
                                       SummerMeanTempDifB = c(mean(MyDF$SummerMeanTempDifB)),
                                       SummerMeanTempDifC = c(mean(MyDF$SummerMeanTempDifC))
                                       
  )
  
  my_estimatesA <- SeedProductionEquation[i,]
  
  # subset MyTestData for DBH >10
  
  MyTestData <- subset(MyTestData, DBH > 10)
  
  MyTestData2 <- MyTestData %>%
    mutate(EqnPred = exp(my_estimatesA$`(Intercept)` +
                           DBH * my_estimatesA$DBH +
                           DBH^2 * my_estimatesA$`I(DBH^2)` +
                           sqrt(DBH) * my_estimatesA$`I(DBH^0.5)` +
                           SummerMeanTemp * my_estimatesA$SummerMeanTemp +
                           SummerMeanTemp^2 * my_estimatesA$`I(SummerMeanTemp^2)` +
                           sqrt(SummerMeanTemp) * my_estimatesA$`I(SummerMeanTemp^0.5)` +
                           SummerMeanTempDifA * my_estimatesA$SummerMeanTempDifA +
                           SummerMeanTempDifB * my_estimatesA$SummerMeanTempDifB +
                           SummerMeanTempDifC * my_estimatesA$SummerMeanTempDifC +
                           SprMinTemp * my_estimatesA$SprMinTemp +
                           SprminTempDifA * my_estimatesA$SprminTempDifA +
                           SprminTempDifB * my_estimatesA$SprminTempDifB +
                           SprminTempDifC * my_estimatesA$SprminTempDifC +
                           WDa * my_estimatesA$WDa )
    )
  
  # Round the seed production numbers 
  MyTestData2$EqnPred <- round(MyTestData2$EqnPred, 1)
  # replace Nan with 0
  MyTestData2[is.na(MyTestData2)] <- 0
  # replace inf with max observed value 
  MyTestData2$EqnPred <- replace(MyTestData2$EqnPred, 
                                 MyTestData2$EqnPred > max(MyDF$SeedEstimate), 
                                 max(MyDF$SeedEstimate))
  
  colnames(MyTestData2)
  
  MyTestData3<- MyTestData2[, c("DBH", "EqnPred")]
  
  MyTestData3$species <-  MySpeciesList[i]
  
  MyDBHPredictionDF <- rbind(MyDBHPredictionDF, MyTestData3)
  
}

library(ggplot2)

library(ggbeeswarm)
colnames(MyDBHPredictionDF)

ggplot( MyDBHPredictionDF , aes(x = DBH, y = EqnPred, color = species )) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree")

# graphs not very good; using the average weather numbers most likely messed up the calculations 

# try using the given weather data to predict seed production and add it to the main DF to graph and compare 

# add the equation coefficients into the DF 
# need to edit labels of coefficients to indicate if it is from an equation or from source

SeedProductionEquationV2 <- SeedProductionEquation
colnames(SeedProductionEquationV2) <- paste0('EQN', colnames(SeedProductionEquationV2))
# make sure species is still species in the EQN data frame 
SeedProductionEquationV2 <- rename(SeedProductionEquationV2, species = EQNspecies)

# add to main DF
LuuPNWSeed800v6c <- left_join(LuuPNWSeed800v6b,SeedProductionEquationV2)

# add predictions to main DF

colnames(LuuPNWSeed800v6c)

MyTestData4 <- LuuPNWSeed800v6c %>%
  mutate(EqnPred = exp(`EQN(Intercept)` +
                         DBH * EQNDBH +
                         DBH^2 * `EQNI(DBH^2)` +
                         sqrt(DBH) * `EQNI(DBH^0.5)` +
                         SummerMeanTemp * EQNSummerMeanTemp +
                         SummerMeanTemp^2 * `EQNI(SummerMeanTemp^2)` +
                         sqrt(SummerMeanTemp) * `EQNI(SummerMeanTemp^0.5)` +
                         SummerMeanTempDifA * EQNSummerMeanTempDifA +
                         SummerMeanTempDifB * EQNSummerMeanTempDifB +
                         SummerMeanTempDifC * EQNSummerMeanTempDifC +
                         SprMinTemp * EQNSprMinTemp +
                         SprminTempDifA * EQNSprminTempDifA +
                         SprminTempDifB * EQNSprminTempDifB +
                         SprminTempDifC * EQNSprminTempDifC +
                         WDa * EQNWDa )
  )



ggplot( MyTestData4 , aes(x = DBH, y = EqnPred, color = species )) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree")


# 8/25 filter the species of interest 

is_match <- MyTestData4$species %in% MySpeciesList

MyTestData5 <- MyTestData4[is_match, ]

ggplot( MyTestData5 , aes(x = DBH, y = EqnPred, color = species )) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree")

# further filter instances where DBH less than then the biological obs

MyTestData6 <- subset(MyTestData5, DBH >= 4)

ggplot( MyTestData6 , aes(x = DBH, y = EqnPred, color = species )) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree")


ggplot( MyTestData6 , aes(x = DBH, y = SeedEstimate, color = species )) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree Data")


################### Code for updated graphs comparing seed production method with the FIA data 

# load the updated simulations results - Results C
Aug2023ResultsCHL <- read_csv("Aug2023ResultsCHL.csv")

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# start with the Variant Simulations 
# load the data 

Aug2023ResultsCHL <- read_csv("Aug2023ResultsCHL.csv")

# keep only time 500 data 

Aug2023ResultsCHL  <- subset(Aug2023ResultsCHL , Time == 500)

# load the data with the corrected FIA data 
May2023ResultsT500withFIAv2 <- read_csv("May2023ResultsT500withFIAv2.csv")

unique(May2023ResultsT500withFIAv2$nat.key2)

filtered_FIA <- May2023ResultsT500withFIAv2[May2023ResultsT500withFIAv2$nat.key2 %in% unique(Aug2023ResultsCHL$nat.key2),  ]
unique(filtered_FIA$nat.key2)
unique(Aug2023ResultsCHL$nat.key2)

# keep nat key and elevation data and add to the main DF 
elevationData <- filtered_FIA %>% select(nat.key2, `elevation (m)`)

elevationData <- unique(elevationData)

filtered_FIA <- filtered_FIA %>% rename(source = variant)

# add to the elevation data to the main DF 
Aug2023ResultsCHL<- left_join(Aug2023ResultsCHL,elevationData)

library(dplyr)
Aug2023ResultsCHL <- Aug2023ResultsCHL %>% dplyr::rename("source" = "variant")

library(plyr)
Aug2023ResultsCHL$source <- as.character(Aug2023ResultsCHL$source)
Aug2023ResultsCHL$source <- revalue(Aug2023ResultsCHL$source, c( "14" = "ForClim"))
Aug2023ResultsCHL$source <- revalue(Aug2023ResultsCHL$source, c( "34" = "DBH and Weather Seed Model"))
Aug2023ResultsCHL$source <- revalue(Aug2023ResultsCHL$source, c( "44" = "DBH Seed Model"))
Aug2023ResultsCHL$source <- revalue(Aug2023ResultsCHL$source, c( "54" = "Weather Seed Model"))
Aug2023ResultsCHL$source <- revalue(Aug2023ResultsCHL$source, c( "64" = "Constant Seed Model"))

unique(Aug2023ResultsCHL$source)

# add the correct FIA data into the updated simulation data 
filtered_FIA2 <- subset(filtered_FIA, variant == 'FIA') # keep the old FIA results 
unique(filtered_FIA2$nat.key2)

filtered_FIA2 <- filtered_FIA2 %>% dplyr::rename("source" = "variant")

Aug2023ResultsCHL2 <- rbind(Aug2023ResultsCHL,filtered_FIA2)

write.csv(Aug2023ResultsCHL2 ,  "Aug2023ResultsCHL2.csv",row.names = FALSE)

# try graphing 

Aug2023ResultsCHL2 <- read_csv("Aug2023ResultsCHL2.csv")

TestData2 <- Aug2023ResultsCHL2 %>%                        # Aggregate values in range
  mutate(ranges = cut(`elevation (m)`,
                      seq(0, 2400, 100))) %>% 
  group_by(source,ranges,species) %>% 
  dplyr::summarize(meanBA = mean(`Basal Area per Hectare`)) %>% 
  mutate(lower = as.numeric( sub("\\((.+),.*", "\\1", ranges) ),
         upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ranges) )) %>%
  as.data.frame()

ggplot(TestData2, aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) +
  facet_wrap(~ source, scales = 'free') +
  theme_bw() 

# Recreate Figure 1 of paper 

# polish the results 
MyMainBASubset <- subset(TestData2, source == 'FIA' |
                           source == 'ForClim' |
                           source == "DBH and Weather Seed Model")

# reorder outputs
MyMainBASubset$source <- factor(MyMainBASubset$source,
                                levels=c("FIA",
                                         "ForClim",
                                         "DBH and Weather Seed Model"))

MyMainBASubset$source <- revalue(MyMainBASubset$source, c( "DBH and Weather Seed Model" = "Updated ForClim"))

ggplot(MyMainBASubset, aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) +
  facet_wrap(~ source) +
  theme_bw() 

ggplot(MyMainBASubset, aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) +
  facet_wrap(species ~ source, scale = 'free') +
  theme_bw() 



# update mean and standard error graph using this data 

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

MySummarySE <- summarySE(Aug2023ResultsCHL2, measurevar="Basal Area per Hectare",
                         groupvars=c("species","source"))

MySummarySE$`Basal Area per Hectare`

ggplot(MySummarySE, aes(x=source, y= `Basal Area per Hectare`, colour= source)) + 
  geom_errorbar(aes(ymin= `Basal Area per Hectare`-se, ymax= `Basal Area per Hectare`+se), width=.1) +
  geom_line() +
  geom_point() +
  theme_bw()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~species,scales = 'free') 

# source name 

MyMainVariantList <- c("FIA", "ForClim","DBH and Weather Seed Model")
MyMainVariantList2 <- c("DBH and Weather Seed Model",
                        "DBH Seed Model",
                        "Weather Seed Model",
                        "Constant Seed Model")

MySummarySE1 <- MySummarySE[MySummarySE$source %in% MyMainVariantList, ]
MySummarySE2 <- MySummarySE[MySummarySE$source %in% MyMainVariantList2, ]

# edit the name of the seed update

MySummarySE1$source <- factor(MySummarySE1$source,
                              levels=c("FIA",
                                       "ForClim",
                                       "DBH and Weather Seed Model"
                              ))


ggplot(MySummarySE1, aes(x=source, y= `Basal Area per Hectare`, colour= source)) + 
  geom_errorbar(aes(ymin= `Basal Area per Hectare`-se, ymax= `Basal Area per Hectare`+se),
                size = 1.5,
                width=.2) +
  geom_point(size = 3) +
  theme_bw() +
  labs( 
    y = bquote('Basal Area'~(m^2 / ha))) +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point() +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab("") + 
  facet_wrap(~species,scales = 'free')+
  theme(strip.text = element_text(face = "italic"))

# reorder outputs
MySummarySE2$source <- factor(MySummarySE2$source,
                              levels=c("DBH and Weather Seed Model",
                                       "DBH Seed Model",
                                       "Weather Seed Model",
                                       "Constant Seed Model"
                              ))


ggplot(MySummarySE2, aes(x=source, y= `Basal Area per Hectare`, colour= source)) + 
  geom_errorbar(aes(ymin= `Basal Area per Hectare`-se, ymax= `Basal Area per Hectare`+se),
                size = 1.5,
                width=.4) +
  geom_point(size = 3) +
  theme_bw() +
  xlab("")+
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point(size = 2) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~species,scales = 'free')+
  theme(strip.text = element_text(face = "italic"))



# diversity index graphs 

# do the same for the diversity data 
# use the data that filtered the FIA data 

library(vegan)
library(dplyr)
library(tidyr)
MyDiversityCalculations <- Aug2023ResultsCHL2 %>%
  dplyr::group_by(source,nat.key2) %>%
  dplyr::summarise(ShannonDiversity = diversity(`Basal Area per Hectare`,'shannon'),
                   SimpsonDiversity = diversity(`Basal Area per Hectare`,'simpson'),
                   SpeciesRichness = specnumber(`Basal Area per Hectare`)
                   #Evenness = diversity(`Basal Area per Hectare`) / log(specnumber(`Basal Area per Hectare`))
  )

MyDiversityCalculations2<- MyDiversityCalculations %>%
  pivot_longer(
    cols = c("ShannonDiversity","SimpsonDiversity","SpeciesRichness"),
    names_to = "Variable",
    values_to = "Value"
  )

MySimpsonDiversitySE <- summarySE(MyDiversityCalculations, measurevar="SimpsonDiversity",
                                  groupvars=c("source"))

# source name 

MySimpsonDiversitySE$source <- factor(MySimpsonDiversitySE$source,
                                      levels=c("FIA",
                                               "ForClim",
                                               "DBH and Weather Seed Model",
                                               "DBH Seed Model",
                                               "Weather Seed Model",
                                               "Constant Seed Model"
                                      ))

ggplot(MySimpsonDiversitySE, aes(x=source, y= `SimpsonDiversity`, colour= source)) + 
  geom_errorbar(aes(ymin= `SimpsonDiversity`-se, ymax= `SimpsonDiversity`+se), 
                size = 1.5,
                width=.4) +
  geom_point(size = 3) +
  theme_bw() +
  ylim(0,1) +
  
  xlab("") +
  ylab("Simpson Diversity") +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point(size = 2) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 


# update sensitivity analysis graphs 
Aug2023ResultsDHL <- read_csv("Aug2023ResultsDHL.csv")

library(corrplot)
library(dplyr)
colnames(Aug2023ResultsDHL)

#edit the DF

MyDF <- Aug2023ResultsDHL %>%
  select(-"nat.key2",-"Time")

cor.test.plus <- function(x) {
  list(x, 
       Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))
}

library(dplyr)
detach(package:plyr)
MyCorrelations<- MyDF %>%
  group_by(species) %>%
  summarize("Seed Initialization Time"=cor(`Basal Area per Hectare`, SeedInitiationTime),
            "Seed Constant"=cor(`Basal Area per Hectare`, SeedConstant),
            "Germination Rate"=cor(`Basal Area per Hectare`, GerminationRate),
            "Sapling Growth Time"=cor(`Basal Area per Hectare`, SaplingGrowthTime),
            "Survival Beta Variance"=cor(`Basal Area per Hectare`, SeedlingSurvivalVariance))

library(tidyr)

MyCorrelation2<- MyCorrelations %>%
  pivot_longer(
    !species,
    names_to = "Variable",
    values_to = "correlation"
  )

# get the standard error of the correlations 

MyCorrelationsSE<- MyDF %>%
  group_by(species) %>%
  summarize("Seed Initialization Time"=  sqrt((1 - cor(`Basal Area per Hectare`, SeedInitiationTime)^2)/(length(`Basal Area per Hectare`) - 2)),
            "Seed Constant"=  sqrt((1 - cor(`Basal Area per Hectare`, SeedConstant)^2)/(length(`Basal Area per Hectare`) - 2)),
            "Germination Rate"= sqrt((1 - cor(`Basal Area per Hectare`, GerminationRate)^2)/(length(`Basal Area per Hectare`) - 2)),
            "Sapling Growth Time"= sqrt((1 - cor(`Basal Area per Hectare`, SaplingGrowthTime)^2)/(length(`Basal Area per Hectare`) - 2)),
            "Survival Beta Variance"=  sqrt((1 - cor(`Basal Area per Hectare`, SeedlingSurvivalVariance)^2)/(length(`Basal Area per Hectare`) - 2)))

MyCorrelationSE2<- MyCorrelationsSE %>%
  pivot_longer(
    !species,
    names_to = "Variable",
    values_to = "SE"
  )

MyCorrelationData <- left_join(MyCorrelation2, MyCorrelationSE2)

library(ggplot2)

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

ggplot(MyCorrelationData, aes(x = species, y = correlation,color = species)) +
  geom_errorbar(aes(ymin= correlation - SE, ymax= correlation + SE),
                size = 1,
                width=.4) +
  geom_point(size = 1.5) +
  theme_bw() +
  scale_colour_manual(values = safe_colorblind_palette)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  facet_wrap(~Variable) +
  xlab("Species")  + 
  scale_color_discrete("Species")  + 
  labs( #title = 'Correlations',
    y = bquote("Correlation")) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 

# get the standardized regression results 

# June 9 variance partition 

# get the SDiv
MyVP1 <- Aug2023ResultsDHL %>%
  dplyr::group_by(nat.key2,SeedInitiationTime,
                  SeedConstant, GerminationRate,SaplingGrowthTime,SeedlingSurvivalVariance) %>%
  dplyr::summarise(
    SimpsonDiversity = diversity(`Basal Area per Hectare`,'simpson'),
    #SpeciesRichness = specnumber(`Basal Area per Hectare`)
    #Evenness = diversity(`Basal Area per Hectare`) / log(specnumber(`Basal Area per Hectare`))
  )

colnames(MyVP1)

fit <- aov(SimpsonDiversity ~ SeedInitiationTime + 
             SeedConstant +
             GerminationRate + 
             SaplingGrowthTime + 
             SeedlingSurvivalVariance, data = MyVP1)


z <- anova(fit)

zss <- z$"Mean Sq"

cbind(z,PctExp=zss/sum(zss)*100)

# save the anova table output z

write.csv(z,"UpdatedANOVAtableLuu.csv")


# create updated seed production equation values tables and min/max numbers for the appendix of publication 

# Manually polished the labels for the seed production equations table 

# use code to extract updated max/min data from LuuPNWSeed800v6c

colnames(LuuPNWSeed800v6c)
unique(LuuPNWSeed800v6c$species)

# MyTestData5 filterd data to keep species of interest
unique(MyTestData6$species)
colnames(MyTestData6)

write.csv(MyTestData6 ,  "MyFilteredSeedDataAndPredictionsV2.csv",row.names = FALSE)

MyMinMaxUpdatedDataV2 <- MyTestData6%>%
  group_by(species) %>%
  summarise("Max DBH" = max(DBH),
            "Min DBH" = min(DBH),
            "Max Summer Mean Temperature" = max(SummerMeanTemp),
            "Min Summer Mean Temperature" = min(SummerMeanTemp),
            "Max Sping Minimum Temperature" = max(SprMinTemp),
            "Min Spring Minimum Temperature" = min(SprMinTemp),
            "Max Wated Deficit" = max(WDa),
            "Min Wated Deficit" = min(WDa),
            "Max Summer Mean Temperature Difference A" = max(SummerMeanTempDifA),
            "Max Summer Mean Temperature Difference B" = max(SummerMeanTempDifB),
            "Max Summer Mean Temperature Difference C" = max(SummerMeanTempDifC),
            "Min Summer Mean Temperature Difference A" = min(SummerMeanTempDifA),
            "Min Summer Mean Temperature Difference B" = min(SummerMeanTempDifB),
            "Min Summer Mean Temperature Difference C" = min(SummerMeanTempDifC),
            
            "Max Spring Minimum Temperature Difference A" = max(SprminTempDifA),
            "Max Spring Minimum Temperature Difference B" = max(SprminTempDifB),
            "Max Spring Minimum Temperature Difference C" = max(SprminTempDifC),
            "Min Spring Minimum Temperature Difference A" = min(SprminTempDifA),
            "Min Spring Minimum Temperature Difference B" = min(SprminTempDifB),
            "Min Spring Minimum Temperature Difference C" = min(SprminTempDifC),
            "Max Seeds Produced" = max(SeedEstimate)
            )


write.csv(MyMinMaxUpdatedDataV2 ,  "MyMinMaxUpdatedDataV2.csv",row.names = FALSE)

# September 7 - check the correlation graph with diversity with the sensitivity analysis 

# get the Simpson Diversity of each simulated site using different Sensitivity analysis values 

MySD <- Aug2023ResultsDHL %>%
  dplyr::group_by(nat.key2,SeedInitiationTime,
                  SeedConstant, GerminationRate,SaplingGrowthTime,SeedlingSurvivalVariance) %>%
  dplyr::summarise(
    SimpsonDiversity = diversity(`Basal Area per Hectare`,'simpson')
  )

cor(MySD$SimpsonDiversity, MySD$SeedInitiationTime)

test <- data.frame(Variable = c("Seed Initiation Time", "Seed Constant", 'Germination Rate',"Sapling Growth Time", "Survival Beta Variance"),
                 Correlation = c(cor(MySD$SimpsonDiversity, MySD$SeedInitiationTime),
                                 cor(MySD$SimpsonDiversity, MySD$SeedConstant),
                                 cor(MySD$SimpsonDiversity, MySD$GerminationRate),
                                 cor(MySD$SimpsonDiversity, MySD$SaplingGrowthTime),
                                 cor(MySD$SimpsonDiversity, MySD$SeedlingSurvivalVariance)),
                 SE = c(sqrt((1 - cor(MySD$SimpsonDiversity, MySD$SeedInitiationTime)^2)/(length(MySD$SimpsonDiversity) - 2)),
                        sqrt((1 - cor(MySD$SimpsonDiversity, MySD$SeedConstant)^2)/(length(MySD$SimpsonDiversity) - 2)),
                        sqrt((1 - cor(MySD$SimpsonDiversity, MySD$GerminationRate)^2)/(length(MySD$SimpsonDiversity) - 2)),
                        sqrt((1 - cor(MySD$SimpsonDiversity, MySD$SaplingGrowthTime)^2)/(length(MySD$SimpsonDiversity) - 2)),
                        sqrt((1 - cor(MySD$SimpsonDiversity, MySD$SeedlingSurvivalVariance)^2)/(length(MySD$SimpsonDiversity) - 2))
                        
                        )
                 
                 )

library(ggplot2)

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

ggplot(test, aes(x = Variable, y = Correlation, color = Variable)) +
  geom_errorbar(aes(ymin= Correlation - SE, ymax= Correlation + SE),
                size = 1,
                width=.4) +
  geom_point(size = 1.5) +
  theme_bw() +
  scale_colour_manual(values = safe_colorblind_palette)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  #xlab("Variable")  + 
  scale_color_discrete("Species")  + 
  labs( #title = 'Correlations',
    y = bquote("Correlation")) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Variable")) +
  theme(legend.text = element_text(face = "italic"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 


# October 31 2023 - Edit of figures to incorporate reviewer suggestions.

# polish graphs of seed production using equations with the observed data 

ggplot( MyTestData6 , aes(x = DBH, y = EqnPred, color = species )) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree")


ggplot( MyTestData6 , aes(x = DBH, y = SeedEstimate, color = species )) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree Data")

# edit the species name 
#

library(plyr)
unique(MyTestData6$species)

unique(Aug2023ResultsCHL2$species)

MyTestData6$species <- as.character(MyTestData6$species)
MyTestData6$species <- revalue(MyTestData6$species, c( "abieAmab" = "Abies amabilis"))
MyTestData6$species <- revalue(MyTestData6$species, c( "pseuMenz" = "Pseudotsuga menziesii"))
MyTestData6$species <- revalue(MyTestData6$species, c(  "tsugHete"   = "Tsuga heterophylla"))
MyTestData6$species <- revalue(MyTestData6$species, c( "abieGran"   = "Abies grandis"))
MyTestData6$species <- revalue(MyTestData6$species, c(  "thujPlic"  = "Thuja plicata"))
MyTestData6$species <- revalue(MyTestData6$species, c( "abieProc"  = "Abies procera"))
MyTestData6$species <- revalue(MyTestData6$species, c( "pinuMont" = "Pinus monticola"))
MyTestData6$species <- revalue(MyTestData6$species, c( "tsugMert" = "Tsuga mertensiana"))
MyTestData6$species <- revalue(MyTestData6$species, c( "abieLasi" = "Abies lasiocarpa"))
MyTestData6$species <- revalue(MyTestData6$species, c( "pinuCont" = "Pinus contorta"))
MyTestData6$species <- revalue(MyTestData6$species, c( "piceEnge" = "Picea engelmannii"))
MyTestData6$species <- revalue(MyTestData6$species, c( "pinuPond" = "Pinus ponderosa"))


MyTestData6$SeedEstimate <- round(MyTestData6$SeedEstimate)
MyTestData6$EqnPred <-round(MyTestData6$EqnPred)


# updated plot of equantion preditions with seed prodcution equations
ggplot( MyTestData6 , aes(x = DBH, y = SeedEstimate )) +
  geom_point(alpha = 0.5) +
  geom_point(data = MyTestData6, aes(DBH, EqnPred), color = "red", alpha = 0.2)+
  theme_bw() +
  facet_wrap(~species, scales = 'free') +
  xlab("DBH") +
  ylab("Seeds Produced per Tree (log)") +
  scale_y_log10() +
  theme(strip.text.x = element_text(face = "italic"))  # Italicize facet labels

# edit on figure 1 

# original figure 1 figure
ggplot(MyMainBASubset, aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) +
  facet_wrap(~ source) +
  theme_bw() 


# break up the smaller subsets


TestData2 <- Aug2023ResultsCHL2 %>%                        # Aggregate values in range
  mutate(ranges = cut(`elevation (m)`,
                      seq(0, 2400, 800))) %>% 
  group_by(source,ranges,species) %>% 
  dplyr::summarize(meanBA = mean(`Basal Area per Hectare`)) %>% 
  mutate(lower = as.numeric( sub("\\((.+),.*", "\\1", ranges) ),
         upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ranges) )) %>%
  as.data.frame()


# polish the results 
TestData2 <- subset(TestData2, source == 'FIA' |
                           source == 'ForClim' |
                           source == "DBH and Weather Seed Model")

# reorder outputs
TestData2$source <- factor(TestData2$source,
                                levels=c("FIA",
                                         "ForClim",
                                         "DBH and Weather Seed Model"))

TestData2$source <- revalue(TestData2$source, c( "DBH and Weather Seed Model" = "Updated ForClim"))

ggplot(TestData2, aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) +
  facet_wrap(~ source, scales = 'free') +
  theme_bw() 

# separate data set where species with small basal area are removed 
ggplot(subset(TestData2,meanBA >=5), aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  facet_wrap(~ source, scales = 'free') +
  theme_bw() +
  theme(legend.text = element_text(face = "italic")) 


ggplot(subset(TestData2), aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  facet_wrap(~ source, scales = 'free') +
  theme_bw() +
  theme(legend.text = element_text(face = "italic")) 

# figure 2 edits 

ggplot(MySummarySE1, aes(x=source, y= `Basal Area per Hectare`, colour= source)) + 
  geom_errorbar(aes(ymin= `Basal Area per Hectare`-se, ymax= `Basal Area per Hectare`+se),
                size = 1.5,
                width=.2) +
  geom_point(size = 3) +
  theme_bw() +
  labs( 
    y = bquote('Basal Area'~(m^2 / ha))) +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point() +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab("") + 
  facet_wrap(~species,scales = 'free')+
  theme(strip.text = element_text(face = "italic"))

# reorder outputs
MySummarySE2$source <- factor(MySummarySE2$source,
                              levels=c("DBH and Weather Seed Model",
                                       "DBH Seed Model",
                                       "Weather Seed Model",
                                       "Constant Seed Model"
                              ))


ggplot(MySummarySE2, aes(x=source, y= `Basal Area per Hectare`, colour= source)) + 
  geom_errorbar(aes(ymin= `Basal Area per Hectare`-se, ymax= `Basal Area per Hectare`+se),
                size = 1.5,
                width=.4) +
  geom_point(size = 3) +
  theme_bw() +
  xlab("")+
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point(size = 2) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~species,scales = 'free')+
  theme(strip.text = element_text(face = "italic"))


# edit the correlation data 

# original graph

# add another column with the species 4 letter code 

MyCorrelationData$speciesCode <- MyCorrelationData$species

MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Abies amabilis" = "ABAM"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c(  "Pseudotsuga menziesii" = "PSME"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Tsuga heterophylla" = "TSHE"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c(  "Abies grandis" = "ABGR"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Thuja plicata" = "THPL"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Abies procera" = "ABPR"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c(  "Pinus monticola" = "PIMO"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Tsuga mertensiana" = "TSME"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Abies lasiocarpa" = "ABLA"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Pinus contorta" = "PICO"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c(  "Picea engelmannii" = "PIEN"))
MyCorrelationData$speciesCode <- revalue(MyCorrelationData$speciesCode, c( "Pinus ponderosa" = "PIPO"))

ggplot(MyCorrelationData, aes(x = speciesCode, y = correlation)) +
  geom_errorbar(aes(ymin= correlation - SE, ymax= correlation + SE),
                size = 1,
                width=.4) +
  geom_point(size = 1.5) +
  theme_bw() +
  #scale_colour_manual(values = safe_colorblind_palette)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
  facet_wrap(~Variable) +
  xlab("Species")  + 
  scale_color_discrete("Species")  + 
  labs( #title = 'Correlations',
    y = bquote("Correlation")) +
  theme(text = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = 0))+
  #guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic"),
        axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels


# Figure S4 edit 
library(ggplot2)
ggplot(MyMainBASubset, aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Elevation (m)")  + 
  scale_fill_discrete("Species")  + 
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) +
  facet_wrap(species ~ source, scale = "free_x") +
  theme(strip.text = element_text(face = "italic"))

# Expand example of seed production changes over time (S7) to 1000 years 

July2023SeedsProducedT1000SeedProduction <- read_csv("July2023SeedsProducedT1000SeedProduction.csv")
July2023ResultsUpdate712T1000BA <- read_csv("July2023ResultsUpdate712T1000BA.csv")


ggplot(subset(July2023SeedsProducedT1000SeedProduction, Time >10), aes(x= Time, y = SeedProduction, color = species))+
  geom_point()+
  theme_bw()+
  ylab("Total Seed Production in stand") +
  xlab("Time (years)")+
  facet_wrap(source~nat.key2, scale = 'free')+
  theme(legend.text = element_text(face = "italic")) 


# need to edit the Model names for the BA data 
library(dplyr)
library(plyr)

unique(July2023ResultsUpdate712T1000BA$source)
unique(July2023SeedsProducedT1000SeedProduction$source)
July2023ResultsUpdate712T1000BA$source <- revalue(July2023ResultsUpdate712T1000BA$source, c( "34HL717" = "DBH and Weather Seed Model"))
July2023ResultsUpdate712T1000BA$source <- revalue(July2023ResultsUpdate712T1000BA$source, c( "44HL717" = "DBH Seed Model"))
July2023ResultsUpdate712T1000BA$source <- revalue(July2023ResultsUpdate712T1000BA$source, c( "54HL717" = "Weather Seed Model"))
July2023ResultsUpdate712T1000BA$source <- revalue(July2023ResultsUpdate712T1000BA$source, c( "64HL717" = "Constant Seed Model"))
# replace the Nat key # with ABCD
 unique(July2023ResultsUpdate712T1000BA$nat.key2)
July2023ResultsUpdate712T1000BA$nat.key2 <- revalue(July2023ResultsUpdate712T1000BA$nat.key2, c( "41.1.59035.2016" = "A"))
July2023ResultsUpdate712T1000BA$nat.key2 <- revalue(July2023ResultsUpdate712T1000BA$nat.key2, c( "41.39.79377.2018" = "B"))
July2023ResultsUpdate712T1000BA$nat.key2 <- revalue(July2023ResultsUpdate712T1000BA$nat.key2, c( "41.59.81663.2012" = "C"))
July2023ResultsUpdate712T1000BA$nat.key2 <- revalue(July2023ResultsUpdate712T1000BA$nat.key2, c( "53.77.68489.2013" = "D"))

ggplot(subset(July2023ResultsUpdate712T1000BA, Time >10), aes(x= Time, y = `Basal Area per Hectare`, color = species))+
  geom_point()+
  theme_bw()+
  ylab("Total Basal Area per Hectare") +
  xlab("Time (years)")+
  facet_wrap(source~nat.key2, scale = 'free')+
  guides(color=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) 


unique(July2023SeedsProducedT1000SeedProduction$nat.key2)
July2023SeedsProducedT1000SeedProduction$nat.key2 <- revalue(July2023SeedsProducedT1000SeedProduction$nat.key2, c( "41.1.59035.2016" = "A"))
July2023SeedsProducedT1000SeedProduction$nat.key2 <- revalue(July2023SeedsProducedT1000SeedProduction$nat.key2, c( "41.39.79377.2018" = "B"))
July2023SeedsProducedT1000SeedProduction$nat.key2 <- revalue(July2023SeedsProducedT1000SeedProduction$nat.key2, c( "41.59.81663.2012" = "C"))
July2023SeedsProducedT1000SeedProduction$nat.key2 <- revalue(July2023SeedsProducedT1000SeedProduction$nat.key2, c( "53.77.68489.2013" = "D"))

July2023SeedsProducedT1000SeedProduction$source <- revalue(July2023SeedsProducedT1000SeedProduction$source, c( "DBH + Weather seed model" = "DBH and Weather seed model"))


ggplot(subset(July2023SeedsProducedT1000SeedProduction, Time >10), aes(x= Time, y = SeedProduction, color = species))+
  geom_point()+
  theme_bw()+
  ylab("Total Seed Produced") +
  xlab("Time (years)")+
  facet_wrap(source~nat.key2, scale = 'free')+
  guides(color=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic")) 


# zoom in for only one test site 

ggplot(subset(July2023ResultsUpdate712T1000BA, Time >10 & source =='DBH and Weather Seed Model' & nat.key2 =="A"), aes(x= Time, y = `Basal Area per Hectare`, color = species))+
  geom_point()+
  theme_bw()+
  ylab("Total Basal Area per Hectare") +
  xlab("Time (years)")+
  facet_wrap(~species, scale = 'free')+
  expand_limits(y = 0) +  # Set minimum value of y-axis to 0
  guides(color=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic"))  +
  theme(strip.text = element_text(face = "italic"))


ggplot(subset(July2023SeedsProducedT1000SeedProduction, Time >10 & source =='DBH and Weather seed model' & nat.key2 =="A"), aes(x= Time, y = SeedProduction, color = species))+
  geom_point()+
  theme_bw()+
  ylab("Total Seed Produced") +
  xlab("Time (years)")+
  facet_wrap(~species, scale = 'free')+
  expand_limits(y = 0) +  # Set minimum value of y-axis to 0
  guides(color=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic"))  +
  theme(strip.text = element_text(face = "italic"))



# S7 alternatives by breaking faucets by species 

# keep the full model vs a constant seed model 

MyS7edit <- subset(July2023SeedsProducedT1000SeedProduction, nat.key2 == "B") # keep only B
unique(MyS7edit$source)
MyS7edit <- subset(MyS7edit, source != "Constant seed model") # take out the constant seed model
MyS7edit <- subset(MyS7edit, source != "Weather seed model") # take out the DBH seed model


ggplot(subset(MyS7edit, Time >10), aes(x= Time, y = SeedProduction))+
  geom_point()+
  theme_bw()+
  ylab("Total Seed Produced") +
  xlab("Time (years)")+
  facet_wrap(species~source, scale = 'free', ncol = 4)+
  expand_limits(y = 0) +  # Set minimum value of y-axis to 0
  #guides(color=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic"))  +
  theme(strip.text = element_text(face = "italic"))

# try brey-curtis 
unique(Aug2023ResultsCHL2$source)
FIAData <- subset(Aug2023ResultsCHL2 ,source == "FIA")
ForClim1Data <- subset(Aug2023ResultsCHL2 ,source == "ForClim")
ForClim2Data <- subset(Aug2023ResultsCHL2 ,source == "DBH and Weather Seed Model")

FIAData<- FIAData[order(FIAData$nat.key2), ]
ForClim1Data<- ForClim1Data[order(ForClim1Data$nat.key2), ]

CombinedData <- rbind(FIAData, ForClim1Data, ForClim2Data)

colnames(CombinedData)
# keep only nay.key, source, and BA 
CombinedData <- CombinedData[, c("source", "nat.key2", "species", "Basal Area per Hectare")]

# Calculate the average for each group using dplyr
library(dplyr)
test <- CombinedData %>%
  group_by(source, species) %>%
  summarize(MeanBA = mean(`Basal Area per Hectare`))

# Use gather to transform two columns into rows
library(tidyr)
test2 <- pivot_wider(test, names_from = "species", values_from =  "MeanBA")

row.names(test2) <- test2$source
test2$source <- NULL
rownames(test2) <-  c("Update", "FIA", "ForClim")

# Install and load the vegan package
library(vegan)

# Set 'Community' as row names
row.names(community_data) <- community_data$Community
community_data$Community <- NULL

# Calculate Euclidean distance
euclidean_distance <- vegdist(test2, method = "euclidean")

# Calculate Manhattan distance
manhattan_distance <- vegdist(test2, method = "manhattan")

# Calculate Bray-Curtis dissimilarity matrix
bray_curtis_matrix <- vegdist(test2, method = "bray")

# Print the dissimilarity matrix
print(bray_curtis_matrix)

# Print the results
print("Euclidean Distance:")
print(euclidean_distance)

print("Manhattan Distance:")
print(manhattan_distance)


# check how increases in the seed constant parameter 

# the MyVP1 tables and the simpson diversity with the different seedconstant values 

colnames(MyVP1)
# edit it

test <- aggregate(cbind(SimpsonDiversity) ~ SeedConstant, data = MyVP1, FUN = mean)

library(ggplot2)
ggplot(test, aes(x= SeedConstant, y = SimpsonDiversity))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

correlation_result <- cor(test$SeedConstant, test$SimpsonDiversity)
correlation_result2 <- cor(MyVP1$SeedConstant, MyVP1$SimpsonDiversity)

# nov 2023 try different colors for figures 

# Start with figure 1 - also try different box sizes 
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


library(RColorBrewer)
# Create a color palette with 12 distinct colors for color-blindness from RColorBrewer
color_palette <- brewer.pal(12, "Set3")

# Nov 22 2023
# edit on figure 1 

# break up the smaller subsets
library(tidyr)
library(dplyr)

TestData2 <- Aug2023ResultsCHL2 %>%                        # Aggregate values in range
  mutate(ranges = cut(`elevation (m)`,
                      seq(0, 2400, 400))) %>%   # change this number to get the number of bars desired 
  group_by(source,ranges,species) %>% 
  dplyr::summarize(meanBA = mean(`Basal Area per Hectare`)) %>% 
  mutate(lower = as.numeric( sub("\\((.+),.*", "\\1", ranges) ),
         upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ranges) )) %>%
  as.data.frame()

# polish the results 
TestData2 <- subset(TestData2, source == 'FIA' |
                      source == 'ForClim' |
                      source == "DBH and Weather Seed Model")

# reorder outputs
TestData2$source <- factor(TestData2$source,
                           levels=c("FIA",
                                    "ForClim",
                                    "DBH and Weather Seed Model"))
library(plyr)
TestData2$source <- revalue(TestData2$source, c( "DBH and Weather Seed Model" = "Updated ForClim"))

color_palette <- brewer.pal(12, "Paired")
# brewer.pal color pallete works well here - opt to use it 

# separate data set where species with small basal area are removed 

ggplot(subset(TestData2), aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity") +
  xlab("Elevation (m)")  + 
  scale_fill_manual(values = color_palette) +
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  facet_wrap(~ source) +
  theme_bw() +
  theme(legend.text = element_text(face = "italic")) 


# Figure S3 
ggplot(subset(TestData2), aes(x = upper, y = meanBA, fill = species)) +
  geom_bar(stat = "identity",color = "black", linewidth = 0.5) +
  xlab("Elevation (m)")  + 
  scale_fill_manual(values = color_palette) +
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(hjust = 0))+
  guides(fill=guide_legend(title="Species")) +
  facet_wrap(species~ source) +
  theme_bw() +
  theme(legend.text = element_text(face = "italic")) +
  theme(strip.text = element_text(face = "italic"))

# Figure 2 edit

RColorBrewer::display.brewer.all()

color_palette2 <- brewer.pal(4, "Dark2")

ggplot(MySummarySE2, aes(x=source, y= `Basal Area per Hectare`, colour= source)) + 
  geom_errorbar(aes(ymin= `Basal Area per Hectare`-se, ymax= `Basal Area per Hectare`+se),
                size = 1.5,
                width=.4) +
  geom_point(size = 3) +
  theme_bw() +
  xlab("")+
  labs( #title = 'FIA results Elevation',
    y = bquote('Basal Area'~(m^2 / ha))) +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point(size = 2) +
  theme(text = element_text(size = 10)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~species,scales = 'free')+
  theme(strip.text = element_text(face = "italic")) +
  scale_color_manual(values = color_palette2)  # Set custom colors

# updated figure S4 

color_palette3 <- brewer.pal(6, "Dark2")
ggplot(MySimpsonDiversitySE, aes(x=source, y= `SimpsonDiversity`, color= source)) + 
  geom_errorbar(aes(ymin= `SimpsonDiversity`-se, ymax= `SimpsonDiversity`+se), 
                size = 1.5,
                width=.4) +
  geom_point(size = 3) +
  theme_bw() +
  ylim(0,1) +

  xlab("") +
  ylab("Simpson Diversity") +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point(size = 2) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_color_manual(values = color_palette3)  # Set custom colors


# figure S5 S6 edit

color_palette <- brewer.pal(12, "Paired")

ggplot(subset(July2023SeedsProducedT1000SeedProduction, Time >10 ), aes(x= Time, y = SeedProduction, color = species))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values = color_palette)+
  ylab("Total Seed Produced") +
  xlab("Time (years)")+
  facet_wrap(source~nat.key2, scale = 'free')+
  expand_limits(y = 0) +  # Set minimum value of y-axis to 0
  guides(color=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic"))  +
  theme(strip.text = element_text(face = "italic")) 

ggplot(subset(July2023ResultsUpdate712T1000BA, Time >10), aes(x= Time, y = `Basal Area per Hectare`, color = species))+
  geom_point()+
  theme_bw()+
  scale_color_manual(values = color_palette)+
  ylab("Total Basal Area per Hectare") +
  xlab("Time (years)")+
  facet_wrap(source~nat.key2, scale = 'free')+
  expand_limits(y = 0) +  # Set minimum value of y-axis to 0
  guides(color=guide_legend(title="Species")) +
  theme(legend.text = element_text(face = "italic"))  +
  theme(strip.text = element_text(face = "italic"))

# Figure S7
ggplot(MySummarySE1, aes(x=source, y= `Basal Area per Hectare`, colour= source)) + 
  geom_errorbar(aes(ymin= `Basal Area per Hectare`-se, ymax= `Basal Area per Hectare`+se),
                size = 1.5,
                width=.2) +
  geom_point(size = 3) +
  theme_bw() +
  labs( 
    y = bquote('Basal Area'~(m^2 / ha))) +
  guides(colour=guide_legend(title="Data Source")) +
  geom_point() +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab("") + 
  facet_wrap(~species,scales = 'free')+
  theme(strip.text = element_text(face = "italic"))+
  scale_color_manual(values = color_palette3)  # Set custom colors


