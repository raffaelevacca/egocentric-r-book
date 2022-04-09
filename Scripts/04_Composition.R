# Author: Raffaele Vacca | www.raffaelevacca.com | raffaele.vacca@unimi.it
#
# License:
# Creative Commons Attribution-NonCommercial-ShareAlike 
# CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/


############################################################################## #
###                 MEASURES OF EGO-NETWORK COMPOSITION                     ====
############################################################################## #
## ---- composition

# Load packages.
library(tidyverse)
library(skimr)
library(janitor)
library(magrittr)

# Load data.
load("./Data/data.rda")

# For compositional measures all we need is the alter attribute data frame.
# The data.rda file loaded above includes the alter attribute data frame for
# ego ID 28.
alter.attr.28

# Compositional measures based on a single alter attribute                  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Summary of continuous variable: Average alter closeness.

# Check out the relevant variable
alter.attr.28$alter.clo

# Or, in tidyverse syntax
alter.attr.28 %>%
  pull(alter.clo)

# Battery of descriptive stats.
alter.attr.28 %>%
  skim_tee(alter.clo) 

# Get a single summary measure (useful when writing functions)
mean(alter.attr.28$alter.clo, na.rm = TRUE)

# Summary of categorical variable: Proportion female alters.

# Check out the relevant vector
alter.attr.28$alter.sex

# Get frequencies
alter.attr.28 %>%
  tabyl(alter.sex)

# Same for nationalities
alter.attr.28 %>%
  tabyl(alter.nat)

# Another way to get the proportion of a specific category (this is useful when
# writing functions).
mean(alter.attr.28$alter.sex == "Female")
mean(alter.attr.28$alter.nat == "Sri Lanka")

# The function dplyr::summarise() allows us to calculate multiple measures, name
# them, and put them together in a data frame.
alter.attr.28 %>%
  summarise(
    mean.clo = mean(alter.clo, na.rm=TRUE), 
    prop.fem = mean(alter.sex=="Female"), 
    count.nat.slk = sum(alter.nat=="Sri Lanka"), 
    count.nat.ita = sum(alter.nat=="Italy"), 
    count.nat.oth = sum(alter.nat=="Other")
  )

# What if we want to calculate the same measures for all ego-networks in the data?
# We'll have to use the data frame with all alter attributes from all egos.
alter.attr.all

# dplyr allows us to "group" a data frame by a factor (here, ego IDs) so all
# measures we calculate on that data frame via summarise (means, proportions,
# etc.) are calculated by the groups given by that factor (here, for each ego
# ID).
alter.attr.all %>% 
  group_by(ego_ID) %>% 
  summarise(
    mean.clo = mean(alter.clo, na.rm=TRUE), 
    prop.fem = mean(alter.sex=="Female"), 
    count.nat.slk = sum(alter.nat=="Sri Lanka"), 
    count.nat.ita = sum(alter.nat=="Italy"), 
    count.nat.oth = sum(alter.nat=="Other")
  )

# We'll talk more about this and show more examples in the next sections.


# Compositional measures based on multiple alter attributes                 ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Using indexing we can combine multiple alter attribute variables.

# Mean closeness of alters who are "Friends".

# Check out the relevant vector.
alter.attr.28 %>%
  filter(alter.rel=="Friends") %>%
  pull(alter.clo)

# Get its mean.
alter.attr.28 %>%
  filter(alter.rel=="Friends") %>%
  pull(alter.clo) %>% 
  mean

# Mean closeness of alters who are "Acquaintances".
alter.attr.28 %>%
  filter(alter.rel=="Acquaintances") %>%
  pull(alter.clo) %>% 
  mean

# Equivalently (useful for writing functions)
mean(alter.attr.28$alter.clo[alter.attr.28$alter.rel=="Acquaintances"])

# Count of close family members who live in Sri Lanka vs those who live in Italy.

# In Sri Lanka.
alter.attr.28 %>%
  filter(alter.rel == "Close family", alter.res == "Sri Lanka") %>%
  count

# Equivalently (useful for writing functions)
sum(alter.attr.28$alter.rel == "Close family" & alter.attr.28$alter.res == "Sri Lanka")
# In Italy.
sum(alter.attr.28$alter.rel == "Close family" & alter.attr.28$alter.res == "Italy")

# Again, we can put these measures together into a data frame row with dplyr.
alter.attr.28 %>%
  summarise(
    mean.clo.fr = mean(alter.clo[alter.rel=="Friends"]), 
    mean.clo.acq = mean(alter.clo[alter.rel=="Acquaintances"]),
    count.fam.slk = sum(alter.rel=="Close family" & alter.res=="Sri Lanka"),
    count.fam.ita = sum(alter.rel=="Close family" & alter.res=="Italy")
  )

# Compositional measures of homophily between ego and alters                ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Level-1 join: Bring ego-level data into alter-level data frame for ego 28.
(data.28 <- left_join(alter.attr.28, ego.df, by= "ego_ID"))

# Note the left join: We only retain rows in the left data frame (i.e., alters
# of ego 28), and discard all egos in the right data frame that do not
# correspond to those rows.

# Example: Proportion of alters of the same gender as ego.

# View the relevant data
data.28 %>% 
  dplyr::select(alter_ID, ego_ID, alter.sex, ego.sex)

# First create a vector that is TRUE whenever alter has the same sex as ego in 
# data.28 (the joined data frame for ego 28).
data.28$alter.sex == data.28$ego.sex

# The proportion we're looking for is simply the proportion of TRUE's in this
# vector.
mean(data.28$alter.sex == data.28$ego.sex)

# Similarly: count of alters who are in the same age bracket as the ego.
sum(data.28$alter.age.cat == data.28$ego.age.cat)

# Again, we can put these measures together into a data frame row with dplyr.
data.28 %>%
  summarise(
    prop.same.gender = mean(alter.sex == ego.sex), 
    count.same.age = sum(alter.age.cat == ego.age.cat)
  )

# ---- end-composition
############################################################################## #
###                    COMPOSITION OF MANY EGO-NETWORKS                     ====
############################################################################## #
## ---- split-df


# The summarise function offers a concise syntax to calculate summary 
# statistics on a data frame's variables. 

# Let's see what happens if we apply this function to the alter attribute data
# frame including all alters, without grouping it by ego ID.

# * Mean alter closeness:
alter.attr.all %>%
  summarise(mean.clo = mean(alter.clo, na.rm = TRUE))
# * N of distinct values in the alter nationality variable (i.e., number of 
# distinct nationalities of alters):
alter.attr.all %>%
  summarise(N.nat = n_distinct(alter.nat))
# * N of distinct values in the alter nationality, country of residence, and
# age bracket variables. In this case, we apply the same summarizing function
# to multiple variables (not just one), to be selected via across().
alter.attr.all %>%
  summarise(., across(c(alter.nat, alter.res, alter.age.cat), n_distinct))

# Because we ran this without previously grouping the data frame by ego ID, each
# function is calculated on all alters from all egos pooled (all rows of the
# alter attribute data frame), not on the set of alters of each ego.

# If we group the data frame by ego_ID, each of those summary statistics is
# calculated for each ego:

# * Mean alter closeness:
alter.attr.all %>%
  # Group by ego ID
  group_by(ego_ID) %>%
  # Calculate summary measure
  summarise(mean.clo = mean(alter.clo, na.rm = TRUE))

# * N of distinct values in the alter nationality variable (i.e., number of 
# distinct nationalities of alters):
alter.attr.all %>%
  group_by(ego_ID) %>%
  summarise(N.nat = n_distinct(alter.nat))

# We can also "permanently" group the data frame by ego_ID and then calculate all
# our summary measures by ego ID.
alter.attr.all %<>% 
  group_by(ego_ID)

# * N of distinct values in the alter nationality, country of residence, and
# age bracket variables:
alter.attr.all %>%
  summarise(., across(c(alter.nat, alter.res, alter.age.cat), n_distinct))

# We can also use summarise to run more complex functions on alter attributes
# by ego.

# Imagine we want to count the number of alters who are "Close family", "Other
# family", and "Friends" in an ego-network.

# Let's consider the ego-network of ego ID 28 as an example.
alter.attr.28

# Calculate the number of alters in each relationship type in this ego-network.

# Vector of alter relationship attribute.
alter.attr.28$alter.rel

# Flag with TRUE whenever alter is "Close family"
alter.attr.28$alter.rel=="Close family"

# Count the number of TRUE's
sum(alter.attr.28$alter.rel=="Close family")

# The same can be done for "Other family" and "Friends"
sum(alter.attr.28$alter.rel=="Other family")
sum(alter.attr.28$alter.rel=="Friends")

# With dplyr we can run the same operations for every ego
N.rel <- alter.attr.all %>%
  summarise(N.clo.fam = sum(alter.rel=="Close family"),
            N.oth.fam = sum(alter.rel=="Other family"),
            N.fri = sum(alter.rel=="Friends"))
N.rel

# After getting compositional summary variables for each ego, we might want to
# join them with other ego-level data (level-2 join).

# Merge with summary variables.
ego.df %>%
  left_join(N.rel, by= "ego_ID")

# We can then ungroup alter.attr.all by ego ID to remove the grouping information.
alter.attr.all <- ungroup(alter.attr.all)

# To get the size of each personal network, we can use the count() function,
# which counts the number of rows for each unique value of a variable. The
# number of rows for each unique value of ego_ID in alter.attr.all is the number
# of alters for each ego (personal network size).
alter.attr.all %>% 
  dplyr::count(ego_ID)

# ***** EXERCISES 
#
# (1) Extract the values of alter.attr.all$alter.sex corresponding to ego_ID 28
# (dplyr::filter). Calculate the proportion of "Female" values. Based on this
# code, use summarise() to calculate the proportion of women in every ego's
# personal network. Hint: mean(alter.sex=="Female").
#
# (2) Subset alter.attr.all to the rows corresponding to ego_ID 53 (dplyr::filter).
# Using the resulting data frame, calculate the average closeness ($alter.clo)
# of Italian alters (i.e. $alter.nat=="Italy"). Based on this code, run
# summarise() to calculate the average closeness of Italian alters for all egos.
#
# *****

# ---- end-split-df
