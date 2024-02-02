# Author: Raffaele Vacca | www.raffaelevacca.com | raffaele.vacca@unimi.it
#
# License:
# Creative Commons Attribution-NonCommercial-ShareAlike 
# CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/


############################################################################## #
###             IMPORTING EGOCENTRIC DATA WITH EGOR                         ====
############################################################################## #
## ---- egor

# Load packages
library(tidyverse)
library(igraph)
library(egor)
library(janitor)

# Import the raw csv files into R as data frames.

# Ego attributes
(ego.df <- read_csv("./Data/raw_data/ego_data.csv"))

# Convert all character variables to factor
# Using dplyr's across(), this code takes all ego.df variables that are character
# (where(is.character)) and applies the as.factor() function to each, converting 
# it to a factor variable. The resulting data frame is re-assigned to ego.df.
ego.df <- ego.df |> 
  mutate(across(where(is.character), as.factor))

# Alter attributes (all alters from all egos in same data frame).
(alter.attr.all <- read_csv("./Data/raw_data/alter_attributes.csv"))

# Convert all character variables to factor.
alter.attr.all <- alter.attr.all |> 
  mutate(across(where(is.character), as.factor))

# To make things easier in the rest of the workshop materials, make sure the 
# factors of alter gender and ego gender have the same levels (categories).

# Levels of alter.sex variable.
alter.attr.all |>
  pull(alter.sex) |>
  levels()

# Levels of ego.sex variable: Female doesn't appear because no ego is female in 
# these data.
ego.df |>
  pull(ego.sex) |>
  levels()

ego.df |>
  tabyl(ego.sex)

# Add the Female level to the ego.sex factor.
ego.df <- ego.df |> 
  mutate(ego.sex = fct_expand(ego.sex, "Female"))

# See the result
ego.df |>
  pull(ego.sex) |>
  levels()

# Note that this doesn't alter the actual data in ego.df, just adds "Female" to 
# the list of possible levels (categories) in ego.sex.
ego.df |>
  tabyl(ego.sex)

# Alter-alter ties (single edge list with all alters from all egos).
(alter_ties <- read_csv("./Data/raw_data/alter_ties.csv"))

# Using these 3 data frames, create an egor object.
egor.obj <- egor::threefiles_to_egor(egos= ego.df, 
                               alters.df= alter.attr.all, 
                               edges= alter_ties, 
                               ID.vars = list(ego = "ego_ID", 
                                              alter = "alter_ID", 
                                              source = "from", 
                                              target = "to"))

# The egor object can immediately be converted to a list of igraph networks.
gr.list <- egor::as_igraph(egor.obj)

# See the result
head(gr.list)

# Note that gr.list is a named list, with each element's name being the 
# corresponding ego ID.
names(gr.list)

# In gr.list, the igraph ego-networks do not include the ego node. We can create
# the same list, but now include a node for ego in each igraph ego-network.
gr.list.ego <- egor::as_igraph(egor.obj, include.ego = TRUE)

# Let's look at the same network with and without the ego.

# Without ego (gr.list).
(gr <- gr.list[["28"]])

# Just the vertex sequence.
V(gr)

# With ego (gr.list.ego). 
(gr.ego <- gr.list.ego[["28"]])

# Just the vertex sequence. Note the name of the last node.
V(gr.ego)

# In our ego-network data, alter-alter ties have weights (1 or 2, see the data 
# codebook).
E(gr)$weight

# These weights are obviously not defined for ego-alter ties. So in the 
# ego-network with the ego included, the tie weights are NA for ego-alter ties.
E(gr.ego)$weight
E(gr.ego)[inc("ego")]$weight

# This may create problems in certain functions, for example the plot.igraph()
# function. So let's replace those NA's with a distinctive weight value for 
# ego-alter ties: 3.
E(gr.ego)$weight <- E(gr.ego)$weight |> 
  replace_na(3)

# See the result
E(gr.ego)$weight
E(gr.ego)[inc("ego")]$weight

# Let's now plot the two ego-networks.

# Without ego.
plot(gr, vertex.label = NA)

# With ego.
plot(gr.ego, vertex.label = NA)

# Let's do the weight replacement operation above (NA replaced by 3 for ego-alter 
# tie weights) for all the ego-networks in gr.list.ego.
for (i in seq_along(gr.list.ego)) {
  E(gr.ego)$weight <- E(gr.ego)$weight |> 
    replace_na(3)
}

# The operation above can be done with tidyverse map() functions as well. First
# define the function operating on a single graph.
weight_replace <- function(gr) {
  E(gr)$weight <- E(gr)$weight |> 
    replace_na(3)
  
  # Return value
  gr
}

# Then apply the function to all graphs in the list.
new.list <- gr.list.ego |>
  map(weight_replace)

# Sometimes it's useful to have ego ID as a graph attribute of the ego-network
# in addition to having it as name of the ego-network's element in the list.
# So let's add ego ID as the $ego_ID graph attribute in each list element.

# List of graphs without the ego.
for (i in seq_along(gr.list)) {
  gr.list[[i]]$ego_ID <- names(gr.list)[[i]]
}

# List of graphs with the ego.
for (i in seq_along(gr.list.ego)) {
  gr.list.ego[[i]]$ego_ID <- names(gr.list.ego)[[i]]
}

# See the result 
gr.list[["28"]]
gr.list[["28"]]$ego_ID

# Extract alter attributes and graph for one ego (ego ID 28)

# Alter attribute data frame.
alter.attr.28 <- alter.attr.all |>
  filter(ego_ID==28)

# Ego-network graph.
gr.28 <- gr.list[["28"]]

# Ego-network graph, with ego included.
gr.ego.28 <- gr.list.ego[["28"]]

# Save all data to file.
save(ego.df, alter.attr.all, gr.list, alter.attr.28, gr.28, gr.ego.28, file="./Data/data.rda")

# ---- end-egor