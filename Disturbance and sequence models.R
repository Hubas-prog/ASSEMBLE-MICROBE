                   #######################################
                   ### Disturbance and sequence models ###
                   #######################################

library(rstanarm)

# Read in data file
CSM <- readRDS('CSM.rda')
chlor <- readRDS('Chlorophyll.rda')
PAM_FULL <- readRDS('PAM_FULL.rda')

  ### Cohesive strength meter ###

csmmodel <- stan_glm(formula = Erosion ~ Seq * Disturb, 
                      adapt_delta = 0.999, iter = 3000, prior_intercept = normal(0.2, 0.5, 
                      autoscale = F), prior = normal(location = c(0,0,0), scale = c(0.12, 0.12, 0.12), autoscale = F),
                      prior_aux = exponential(rate = 20, autoscale = F), data = CSM)

  ### Chlorophyll ###

chlmodel <- stan_glm(formula = Chlorophyll ~ Seq + Disturb, 
                 data = chlor, adapt_delta = 0.999, iter = 3000, prior_intercept = normal(0.8, 5, autoscale = F),
                 prior = normal(location = c(0,0), scale = c(3.15, 3.15), autoscale = F),
                 prior_aux = exponential(rate = 0.79, autoscale = F)) 

  ### Alpha ###

alphamodel <- stan_lmer(formula = Alpha ~ Seq + + Disturb + Time + (1 | repli), 
                                   data = PAM_FULL, adapt_delta = 0.9999, iter = 3000, prior_intercept = normal(0.2, 5, autoscale = F),
                                   prior = normal(location = c(0,0,0), scale = c(0.086,0.086,0.104), autoscale = F),
                                   prior_aux = exponential(rate = 29, autoscale = F), prior_covariance = 
                                   decov(regularization = 1, concentration = 1, shape = 1, scale = 1)) 

  ### Ek ###

Ekmodel <- stan_lmer(formula = Ek ~ Seq + + Disturb + Time + (1 | repli), 
                                    data = PAM_FULL, adapt_delta = 0.9999, iter = 3000, prior_intercept = normal(200, 50, autoscale = F),
                                    prior = normal(location = c(0,0,0), scale = c(120.51,120.51,145.53), autoscale = F),
                                    prior_aux = exponential(rate = 0.021, autoscale = F), prior_covariance = 
                                    decov(regularization = 1, concentration = 1, shape = 1, scale = 1)) 

  ### rETRmax ###
        
Ekmodel <- stan_lmer(formula = rETRmax ~ Seq * Disturb + Time + (1 | repli), 
                                 data = PAM_FULL, adapt_delta = 0.9999, iter = 3000, prior_intercept = normal(60, 20, autoscale = F),
                                 prior = normal(location = c(0,0,0, 0), scale = c(18.67,18.67,22.54, 18.67), autoscale = F),
                                 prior_aux = exponential(rate = 0.13, autoscale = F), prior_covariance = 
                                 decov(regularization = 1, concentration = 1, shape = 1, scale = 1)) 

  ### Fv/Fm ###

Fv/Fmmodel <- stan_lmer(formula = Fv.Fm ~ Seq + + Disturb + Time + (1 | repli), 
                                 data = PAM_FULL, adapt_delta = 0.9999, iter = 3000, prior_intercept = normal(0.6, 10, autoscale = F),
                                 prior = normal(location = c(0,0,0), scale = c(0.21,0.21,0.25), autoscale = F),
                                 prior_aux = exponential(rate = 12, autoscale = F), prior_covariance = 
                                 decov(regularization = 1, concentration = 1, shape = 1, scale = 1)) 
                   
                   
                   
                   
      
