# Author: Raffaele Vacca | www.raffaelevacca.com | raffaele.vacca@unimi.it
#
# License:
# Creative Commons Attribution-NonCommercial-ShareAlike 
# CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/


############################################################################## #
###        Setup                                                            ====
############################################################################## #
## ---- mm-setup

# Load packages.
library(tidyverse)
library(lme4)
library(car)
library(magrittr)
library(skimr)
library(janitor)
library(broom.mixed)

# Clear the workspace from all previous objects
rm(list=ls())

# Load the data
load("./Data/data.rda")

# Create data frame object for models (level-1 join)
(model.data <- left_join(alter.attr.all, ego.df, by= "ego_ID"))

## Create variables to be used in multilevel models                         ====
# ============================================================================ =

# Ego-alter age homophily variable
# - - - - - - - - - - - - - - - - - 

# (TRUE if alter and ego are in the same age bracket)
model.data <- model.data |>  
  mutate(alter.same.age = (alter.age.cat==ego.age.cat))  

# See result
tabyl(model.data$alter.same.age)

# Recode: TRUE = Yes, FALSE = No
model.data <- model.data |> 
  mutate(alter.same.age = as.character(alter.same.age),
         alter.same.age = fct_recode(alter.same.age,
                                     Yes = "TRUE", No = "FALSE"))

# See result
tabyl(model.data$alter.same.age)

# Centered/rescaled versions of ego and alter age 
# - - - - - - - - - - - - - - - - - - - - - - - - 

# This is done for easier interpretation of model coefficients
model.data <- model.data |> 
  # Ego age centered around its mean and scaled by 5 (1 unit = 5 years)
  mutate(ego.age.cen = scale(ego.age, scale= 5),
         # Alter age category centered around its mean
         alter.age.cat.cen = scale(as.numeric(alter.age.cat), scale= FALSE))

# Count of family members in ego-network
# - - - - - - - - - - - - - - - - - - - - - - - - 

model.data <- model.data |> 
  group_by(ego_ID) |>
  mutate(net.count.fam = sum(alter.fam=="Yes", na.rm=TRUE)) |>
  ungroup()

# Center and rescale by 5 (+1 unit = 5 more family members in ego-network)
model.data <- model.data |> 
  mutate(net.count.fam.cen = scale(net.count.fam, scale=5)) 


## ---- end-mm-setup
############################################################################## #
###        Random intercept models                                          ====
############################################################################## #
## ---- rand-intercept

## m1: Variance components models                                           ====
# ============================================================================ =

# Variance components model: level 1 is ties, level 2 is egos, random intercept, 
# no predictor
m1 <- glmer(alter.loan ~ # Dependent variable
              (1 | ego_ID), # Intercept (1) varies in level-2 units (ego_ID)
            family = binomial("logit"), # Model class (logistic)
            data = model.data) # Data object

# View results
car::S(m1)


## m2: Add tie characteristics as predictors (level 1)                      ====
# ============================================================================ =

# Add alter.fam and alter.same.age as predictors

# See descriptives for the new predictors
tabyl(model.data$alter.fam)
tabyl(model.data$alter.same.age)

# Estimate the model and view results
m2 <- glmer(alter.loan ~ # Dependent variable
              alter.fam + alter.same.age + # Tie characteristics
              (1 | ego_ID), # Intercept (1) varies in level-2 units (ego_ID)
            family = binomial("logit"), # Model class (logistic)
            data = model.data) # Data object
car::S(m2)

## m3: Add ego characteristics as predictors (level 2)                      ====
# ============================================================================ =

# Add ego age (ego.age.cen), employment status (ego.empl.bin), 
# educational level (ego.edu)

# See descriptives for the new predictors
skim_tee(model.data$ego.age.cen)
tabyl(model.data$ego.empl.bin)
tabyl(model.data$ego.edu)

# Estimate the model and view results
m3 <- glmer(alter.loan ~ alter.fam + alter.same.age + # Tie characteristics
              ego.age.cen + ego.empl.bin + ego.edu + # Ego characteristics
              (1 | ego_ID), # Intercept (1) varies in level-2 units
            family = binomial("logit"), # Model class (logistic)
            data = model.data) # Data
car::S(m3)


## m4: Add alter characteristics as predictors (level 1)                        ====
# ============================================================================ =

# Add alter sex and alter age (centered)

# See descriptives for the new predictors
tabyl(model.data$alter.sex)
skim_tee(model.data$alter.age.cat.cen)

# Estimate the model and view results
m4 <- glmer(alter.loan ~ alter.fam + alter.same.age + # Tie characteristics
              ego.age.cen + ego.empl.bin + ego.edu + # Ego characteristics
              alter.sex + alter.age.cat.cen + # Alter characteristics
              (1 | ego_ID), # Intercept (1) varies in level-2 units
            family = binomial("logit"), # Model class (logistic)
            data= model.data) # Data
car::S(m4)

## m5: Add network characteristics as predictors (level 2)                  ====
# ============================================================================ =

# Add count of family members in network (centered)

# See descriptives for the new predictor
skim_tee(model.data$net.count.fam.cen)

# Estimate model and see results
m5 <- glmer(alter.loan ~ alter.fam + alter.same.age + # Tie characteristics
              ego.age.cen + ego.empl.bin + ego.edu + # Ego characteristics
              alter.sex + alter.age.cat.cen +  # Alter characteristics
              net.count.fam.cen + # Ego-network characteristics
              (1 | ego_ID), # Intercept (1) varies in level-2 units
            family = binomial("logit"), # Model class (logistic)
            data= model.data) # Data
car::S(m5)

# Results as tidy data frame
tidy(m5)

## Plot predictor effects                                                   ====
# ============================================================================ =

library(ggeffects)

# Probability of financial support as a function of alter.fam
ggpredict(m5, "alter.fam") 

# You can convert it to a tidy data frame
ggpredict(m5, "alter.fam") |>
  as_tibble()

# You can also plot the result
ggpredict(m5, "alter.fam") |> 
  plot()

# Probability of financial support as a function of ego.edu
ggpredict(m5, "ego.edu") |> 
  plot()


## ---- end-rand-intercept
############################################################################## #
###        Random slope models                                              ====
############################################################################## #
## ---- rand-slope

## m6: Random slope for alter.fam                                           ====
# ============================================================================ =

# Fit model
set.seed(2707)
m6 <- glmer(alter.loan ~ alter.fam + alter.same.age + # Tie characteristics
              ego.age.cen + ego.empl.bin + ego.edu + # Ego characteristics
              alter.sex + alter.age.cat.cen +  # Alter characteristics
              net.count.fam.cen + # Ego-network characteristics
              (1 + alter.fam | ego_ID), # Both intercept (1) and alter.fam 
            # slppe vary in level-2 units (ego_ID)
            family = binomial("logit"), # Model class (logistic)
            data = model.data) # Data

# Re-fit with starting values from previous fit to address convergence warnings

# Get estimate values from previous fit
ss <- getME(m6, c("theta", "fixef"))

# Refit by setting ss as starting values 
m6 <- glmer(alter.loan ~ alter.fam + alter.same.age + # Tie characteristics
              ego.age.cen + ego.empl.bin + ego.edu + # Ego characteristics
              alter.sex + alter.age.cat.cen +  # Alter characteristics
              net.count.fam.cen + # Ego-network characteristics
              (1 + alter.fam | ego_ID), # Both intercept (1) and alter.fam 
            start= ss, # 
            # slppe vary in level-2 units (ego_ID)
            family = binomial("logit"), # Model class (logistic)
            data= model.data) # Data

# View results
car::S(m6)

# Results as tidy data frame
tidy(m6)

## ---- end-rand-slope
############################################################################## #
###        Test significance of random effects                              ====
############################################################################## #
## ---- test-sign

## Test significance of ego-level random intercept                          ====
# ============================================================================ =

# Test that there is significant clustering by egos, i.e. ego-level variance
# of random intercepts is significantly higher than 0. This means comparing
# the random-intercept null model (i.e. "variance components" model) to the 
# single-level null model.

# First estimate the simpler, single-level null model: m0, which is nested in m1
m0 <- glm(alter.loan ~ 1, family = binomial("logit"), data= model.data)

# Then conduct a LRT comparing deviance of m0 to deviance of m1.

# Difference between deviances.
(val <- -2*logLik(m0)) - (-2*logLik(m1))

# Compare this difference to chi-squared distribution with 1 degree of freedom.
pchisq(val, df= 1, lower.tail = FALSE)

# The same result is obtained using the anova() function
anova(m1, m0, refit=FALSE)


## Test significance of ego-level random slope for alter.fam                ====
# ============================================================================ =

# This is done with a LRT comparing the same model with random slope for alter.fam
# (m6) and without it (m5). Note that m5 is nested in m6, that's why we can 
# use LRT.

# Difference between deviances.
(val <- (-2*logLik(m5)) - (-2*logLik(m6)))

# Compare this difference to chi-squared distribution with 2 degrees of freedom.
pchisq(val, df= 2, lower.tail = FALSE)

# Same results with anova() function
anova(m6, m5, refit=FALSE)

## ---- end-test-sign

