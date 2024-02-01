# Author: Raffaele Vacca | www.raffaelevacca.com | raffaele.vacca@unimi.it
#
# License:
# Creative Commons Attribution-NonCommercial-ShareAlike 
# CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/



############################################################################## #
###                   MEASURES OF EGO-NETWORK STRUCTURE                     ====
############################################################################## #
## ---- structure

# Load packages.
library(tidyverse)
library(igraph)

# Load data.
load("./Data/data.rda")

# For structural measures we need the ego-network as an igraph.

# The data.rda file, which we loaded earlier, includes the igraph object of
# ego ID 28's network.
gr.28

# Let's reassign it to a new, generic object name. This makes the code more 
# generic and more easily re-usable on any ego-network igraph object 
# (of any ego ID).
gr <- gr.28

# Measures based only on the structure of alter-alter ties                  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Structural characteristics of the network.

# Network density.
edge_density(gr)

# Number of components.
components(gr)

# This is a list, we only need its 3rd element (number of components).
components(gr)$no

# Summarization of structural characteristics of the alters.

# Average alter degree.
degree(gr) |> mean()

# Max alter betweenness.
betweenness(gr, weights = NA) |> max()

# Number of "isolate" alters.

# Check out the alter degree vector.
degree(gr)

# Count the number of isolates, i.e. alters with degree==0
sum(degree(gr)==0)

# All sorts of more complicated structural analyses can be run on an ego-network
# using igraph or statnet functions. For example, here are the results of the
# Girvan-Newman community-detection algorithm on the ego-network.
cluster_edge_betweenness(gr, weights= NA)

# What if we want to calculate the same structural measure (e.g. density) on all
# ego-networks? We can take the list of all ego-networks and run the same 
# function on every list element with the purrr package in tidyverse.

# List that contains all our ego-networks.
class(gr.list)
length(gr.list)
head(gr.list)

# We can use purrr::map() to run the same function on every element of the list.
purrr::map_dbl(gr.list, edge_density)

# We'll talk more about this and show more examples in the section about 
# multiple ego-networks.

# Structural measures requiring ego in the network                          ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# We now need the ego-network with ego included. The file data.rda, loaded earlier,
# includes this ego-network for ego ID 28. 
gr.ego.28

# Let's reassign it to another, generic object name.
gr.ego <- gr.ego.28

# Ego's betweenness centrality.
# weights = NA so the function doesn't use the $weight edge attribute to
# calculate weighted betweenness.
betweenness(gr.ego, v = "ego", weights = NA)

# Ego's constraint.
constraint(gr.ego, nodes= "ego")

# Measures combining composition and structure: From structure to composition ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Non-network attribute of alters selected based on structural characteristics.

# Type of relationship with alter with max betweenness.

# Logical index for alter with max betweenness.
ind <- betweenness(gr, weights = NA) == max(betweenness(gr, weights = NA))

# Get that alter.
V(gr)[ind]

# Get type of relation of that alter.
V(gr)[ind]$alter.rel

# Note that there might be multiple alters with the same (maximum) value of 
# betweenness. For that case, we'll need more complicated code (see exercise).

# Measures combining composition and structure: From composition to structure ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Structural characteristics of alters selected based on composition.

# Average degree of Close family.

# Vertex sequence of Close family members.
(clo.fam.vs <- V(gr)[alter.rel=="Close family"])

# Get their average degree.
gr |>
  degree(v = clo.fam.vs) |> 
  mean()

# Count of ties between alters who live in Sri Lanka.

# First get the vertex sequence of alters who live in Sri Lanka.
(alters.sl <- V(gr)[alter.res=="Sri Lanka"])

# Then get the edges among them.
E(gr)[alters.sl %--% alters.sl]

# How many edges are there between alters who live in Sri Lanka?
E(gr)[alters.sl %--% alters.sl] |> 
  length()

# Density between alters who live in Sri Lanka.

# Get subgraph of relevant alters
induced_subgraph(gr, vids = alters.sl)

# Get the density of this subgraph.
induced_subgraph(gr, vids = alters.sl) |> 
  edge_density()


# ---- end-structure
############################################################################## #
###                               R LISTS                                   ====
############################################################################## #
## ---- lists

# Let's get some objects to put in a list.

# A simple numeric vector.
(num <- 1:10)

# A matrix.
(mat <- matrix(1:4, nrow=2, ncol=2))

# A character vector.
(char <- colors()[1:5])

# Create a list that contains all these objects.
L <- list(num, mat, char)

# Display it
L

# Create a named list
L <- list(numbers= num, matrix= mat, colors= char)  

# Display it
L

# Type and class
typeof(L)
class(L)

# Extract the first element of L
# [ ] notation (result is a list containing the element).
L[1]
# [[ ]] notation (result is the element itself, no longer in a list).
L[[1]]

# $ notation (result is the element itself, no longer in a list).
L$numbers

# Name indexing.
L[["numbers"]]

# Types of elements in L.
str(L)


# ---- end-lists
############################################################################## #
###                  SPLIT-APPLY-COMBINE ON LISTS VIA PURRR                 ====
############################################################################## #
## ---- split-list

# A simple structural measure such as network density can be applied to every
# ego-network (i.e., every element of our list), and returns a scalar for each
# network. All the scalars can be put in a vector.
gr.list |>
  purrr::map_dbl(edge_density) 

# Note that the vector names (taken from gr.list names) are the ego IDs.

# If you want the same result as a nice data frame with ego IDs, use enframe().
gr.list |>
  map_dbl(edge_density) |> 
  enframe(name = "ego_ID", value = "density")

# Same thing, with number of components in each ego network. Note the ~ .x 
# syntax
gr.list |>
  map_dbl(~ components(.x)$no) |> 
  enframe()

# With map_dfr() we can calculate multiple structural measures at once on every 
# ego-network, and return the results as a single ego-level data frame.

# This assumes that we are applying an operation that returns one data frame row
# for each ego-network. For example, take one ego-network from our list.
gr <- gr.list[[10]]

# Apply an operation to that ego-network, which returns one data frame row.
tibble(dens = edge_density(gr),
       mean.deg = mean(igraph::degree(gr)),
       mean.bet = mean(igraph::betweenness(gr, weights = NA)),
       deg.centr = centr_degree(gr)$centralization)

# Now we can do the same thing but for all ego-networks at once. The result is a 
# single ego-level data frame, with one row for each ego. 
# Note the .id argument in map_dfr().
gr.list |>
  map_dfr(~ tibble(dens= edge_density(.x),
                   mean.deg= mean(igraph::degree(.x)),
                   mean.bet= mean(igraph::betweenness(.x, weights = NA)),
                   deg.centr= centr_degree(.x)$centralization),
          .id = "ego_ID")


# ***** EXERCISES
#
# (1) Get the graph for ego ID 28 from gr.list. Get the max betweenness of
# Italian alters on this graph. Based on this code, use map() to get the same
# measure that for all egos.
#
# (2) Given a personal network gr, the subgraph of close family in the personal
# network is induced_subgraph(gr, V(gr)[alter.rel=="Close family"]). Use
# purrr::map() to get the family subgraph for every personal network in gr.list,
# and put the results in a new list called gr.list.family. Use the formula
# notation (~ .x).
#
# *****

# ---- end-split-list
