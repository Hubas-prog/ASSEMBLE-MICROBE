          ####################################################
          ##### Creation of data objects for modelling #######
          ####################################################
library(readxl)
### Cohesive strength meter (CSM) data object creation ###

Erosion <- read_excel("Critical erosion thresholds.xlsx")
CSM <- Erosion[1:12,]
CSM$Seq <- rep(c("T1", "T0", "T1", "T0"), e = 3)
CSM$Disturb <- rep(c("P1", "P1", "P0", "P0"), e = 3)
CSM$Erosion  <- CSM$`Erosion (N m-2)` 

### Chlorophyll data object creation ###

Chlorophyll <- read_excel("Chlorophyll.xlsx")
chlor <- Chlorophyll[1:12,]
chlor$Seq <- rep(c("T1", "T0", "T1", "T0"), e = 3)
chlor$Disturb <- rep(c("P1", "P1", "P0", "P0"), e = 3)

### This script is used to create a singular pulse ampltitude modulation (PAM) data object ###

library(dplyr)
library(lme4)

###

PAM_1 <- read_excel("PAM time 1.xlsx")
PAM_1
PAM_2 <- read_excel("PAM time 2.xlsx")
PAM_2
PAM_3 <- read_excel("PAM time 3.xlsx")
PAM_3

### Identify the replicate mesocosms ###

PAM_1$Time <- 1
PAM_2$Time <- 2
PAM_3$Time <- 3
PAM_1$repli <- rep(1:3, 5)
PAM_2$repli <- rep(1:3, 5)
PAM_3$repli <- rep(1:3, 5)

PAM_FULL <- rbind(PAM_1, PAM_2, PAM_3) # Bind the time points together
PAM_FULL$Treatment <- as.factor(PAM_FULL$Treatment)

### Identify the sequence and disturbance categories for each measurement and replicate

PAM_FULL <- PAM_FULL %>% arrange(Treatment, repli, Time)
PAM_FULL <- PAM_FULL[1:36,]

PAM_FULL$Seq <- rep(c("T1", "T0", "T1", "T0"), e = 9)
PAM_FULL$Disturb <- rep(c("P1", "P1", "P0", "P0"), e = 9)

### Save the objects

saveRDS(PAM_FULL, 'PAM_FULL.rda')
saveRDS(chlor, 'Chlorophyll.rda')
saveRDS(CSM, 'CSM.rda')
