###   Control Models    ###

# Packages
library(rstanarm)
library(readxl)
# Dataframes including the control
PAM_1 <- read_excel("PAM time 1.xlsx")
PAM_1
PAM_2 <- read_excel("PAM time 2.xlsx")
PAM_2
PAM_3 <- read_excel("PAM time 3.xlsx")
PAM_3

PAM_1$Time <- 1
PAM_2$Time <- 2
PAM_3$Time <- 3
PAM_1$repli <- rep(1:3, 5)
PAM_2$repli <- rep(1:3, 5)
PAM_3$repli <- rep(1:3, 5)

ALL_PAM <- rbind(PAM_1, PAM_2, PAM_3)
ALL_PAM$Treatment <- as.factor(ALL_PAM$Treatment)

Chlorophyll <- read_excel("Chlorophyll.xlsx")
Chlorophyll$Treatment <- as.factor(Chlorophyll$Treatment)

Erosion <- read_excel("Critical erosion thresholds.xlsx")
Erosion$Treatment <- as.factor(Erosion$Treatment)
Erosion$Erosion <- Erosion$`Erosion (N m-2)`

      ##############
      ### Models ###
      ##############

# Erosion
eroctl <- stan_glm(formula = Erosion ~ Treatment, 
                      data = Erosion, adapt_delta = 0.99, iter = 3000, prior_intercept = normal(0.2, 0.5, 
                      autoscale = F), prior = normal(location = c(0,0,0,0), scale = c(0.15, 0.15, 0.15, 0.15),
                      autoscale = F), prior_aux = exponential(rate = 17, autoscale = F))

# Chlorophyll 
chlctrl <- stan_glm(formula = Chlorophyll ~ Treatment, 
                    data = Chlorophyll, adapt_delta = 0.99, iter = 3000, prior_intercept = normal(0.8, 5, 
                    autoscale = F), prior = normal(location = c(0,0,0,0), scale = c(0.15, 0.15, 0.15, 0.15),
                    autoscale = F), prior_aux = exponential(rate = 0.51, autoscale = F))

# Alpha
alphctrl <- stan_lmer(formula = Alpha ~ Treatment + Time + (1 | repli), 
                      data = ALL_PAM, adapt_delta = 0.999999, iter = 3000, prior_intercept = normal(0.2, 5, 
                      autoscale = F), prior = normal(location = c(0,0,0,0,0), scale = c(0.079, 0.079, 0.079, 0.079,
                      0.096), autoscale = F), prior_aux = exponential(rate = 32, autoscale = F), 
                      prior_covariance = decov(1, 1, 1, 1))
# Ek
ekctrl <- stan_lmer(formula = Ek ~ Treatment + Time + (1 | repli), 
                    data = ALL_PAM, adapt_delta = 0.99999, iter = 3000, prior_intercept = normal(200, 50,
                    autoscale = F), prior = normal(location = c(0,0,0,0,0), scale = c(124.8, 124.8, 124.8, 124.8,
                    151.2), autoscale = F), prior_aux = exponential(rate = 0.02, autoscale = F),
                    prior_covariance = decov(1,1,1,1))

# rETRmax
etrctrl <- stan_lmer(formula = rETRmax ~ Treatment + Time + (1 | repli), 
                     data = ALL_PAM, adapt_delta = 0.99999, iter = 3000, prior_intercept = normal(60, 20, 
                     autoscale = F), prior = normal(location = c(0,0,0,0,0), scale = c(20.67, 20.67, 20.67, 20.67,
                     25.03), autoscale = F), prior_aux = exponential(rate = 0.12, autoscale = F), 
                     prior_covariance = decov(1,1,1,1))

# Fv/ Fm
fvfmctrl <- stan_lmer(formula = Fv.Fm ~ Treatment + Time + (1 | repli), 
                      data = ALL_PAM, adapt_delta = 0.99999, iter = 3000, prior_intercept = normal(0.6, 10, 
                      autoscale = F), prior = normal(location = c(0,0,0,0,0), scale = c(0.19,0.19,0.19,0.19,0.23),
                      autoscale = F), prior_aux = exponential(rate = 13, autoscale = F), prior_covariance = 
                      decov(1,1,1,1))



