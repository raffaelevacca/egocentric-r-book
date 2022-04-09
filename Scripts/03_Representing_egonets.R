# Author: Raffaele Vacca | www.raffaelevacca.com | raffaele.vacca@unimi.it
#
# License:
# Creative Commons Attribution-NonCommercial-ShareAlike 
# CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/



############################################################################## #
###                      EGO LEVEL VS ALTER LEVEL DATA                      ====
############################################################################## #
## ---- levels

# Load packages.
library(tidyverse)

# Load data.
load("./Data/data.rda")

# The file we just loaded includes the following objects.

# * Ego-level attribute data
ego.df

# * Attribute data for alters and ego-alter ties (all alters and ego-alter ties 
# in one data frame). 
alter.attr.all

# Level-1 join: Ego attributes into alter-level data (one to many).
(data <- left_join(alter.attr.all, ego.df, by= "ego_ID"))

# Note that the new data frame has one row for each alter, and the same ego ID
# and ego attribute value is repeated for all alters belonging to the same ego.
data %>% 
  dplyr::select(alter_ID, ego_ID, ego.age, ego.edu)

# ---- end-levels
############################################################################## #
###              EGO-NETWORKS AS IGRAPH OBJECTS                             ====
############################################################################## #
## ---- igraph

# Load packages.
library(igraph)
library(ggraph)
library(tidygraph)
library(skimr)
library(janitor)

# The data.rda file, which we loaded earlier, also includes the ego-network of 
# ego 28 (as igraph object)
gr.28

# The graph is Undirected, Named, Weighted. It has 45 vertices and 259 edges. It
# has a vertex attribute called "name", and an edge attribute called "weight". 
# We see several vertex attributes (see codebook.xlsx for their meaning).

# Let's reassign it to a new, generic object name. This makes the code more 
# generic and more easily re-usable on any ego-network igraph object 
# (of any ego ID).
gr <- gr.28

# We also have the same network, with ego included: note the different number
# of vertices and edges compared to the previous graph.
gr.ego.28
gr.ego <- gr.ego.28

# Show the graph. Note that ego is not included.
set.seed(607)
ggraph(gr) + 
  geom_edge_link() + # Draw edges
  geom_node_point(size=5, color="blue") + # Draw nodes
  theme_graph(base_family = 'Helvetica') # Set graph theme and font


# An igraph object can be indexed as an adjacency matrix.

# Adjacency row of alter #3.
gr[3,]

# Adjacency row of alter with vertex name (alter ID) "2805".
gr["2805",]

# Adjacency columns of alters 3-5
gr[,3:5]

# Convert and view the ego-network as a tidygraph object: you can now see the 
# attribute data and edge list as tabular datasets.
(gr.tbl <- as_tbl_graph(gr))

# Vertex and edge sequences and attributes                                  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Vertex sequences and edge sequences can be extracted from a graph.

# Vertex sequence of the whole graph. Notice that vertices are displayed by
# "names" (alter IDs in this case).
V(gr)

# Edge sequence of the whole graph. Notice that vertex names (alter IDs) are
# used here too.
E(gr)

# View vertex attributes and calculate statistics on them.

# Closeness of alters to ego.
V(gr)$alter.clo

# Mean closeness of alters to ego. 
mean(V(gr)$alter.clo, na.rm=TRUE)

# More descriptive statistics on alter closeness.
skimr::skim_tee(V(gr)$alter.clo) 

# Alter's country of residence.
V(gr)$alter.res

# Frequencies.
janitor::tabyl(V(gr)$alter.res)

# This is more readable with the pipe operator.
V(gr)$alter.res %>% 
  tabyl()

# Alter IDs are stored in the "name" vertex attribute
V(gr)$name

# We can also create new vertex attributes.
V(gr)$new.attribute <- 1:45

# Now a new attribute is listed for the graph.
gr

# Also edges have attributes, in this case "weight".
E(gr)$weight

# Frequencies of edge weights.
tabyl(E(gr)$weight)

# Also graphs have attributes, and we can query or assign them. For example,
# assign the "ego_ID" attribute for this graph: this is the personal network of
# ego ID 28.
gr$ego_ID <- 28

# Check that the new attribute was created
gr

# Displaying vertex attributes in network visualization                     ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Let's visualize our ego-network, using different node colors for different 
# countries of residence of alters.

# Vertex attribute with alter's country of residence.
V(gr)$alter.res

# Plot with alter.res as node color
set.seed(607)
ggraph(gr) + 
  geom_edge_link() + # Draw edges
  geom_node_point(aes(color= alter.res), size=5) + # Draw nodes setting alter.res 
  # as node color and fixed node size
  theme_graph(base_family = 'Helvetica')

# Plot with alter ID labels instead of circles
set.seed(607)
ggraph(gr) + 
  geom_edge_link() +
  geom_node_label(aes(label= name)) + 
  theme_graph(base_family = 'Helvetica')

# Let's look at the same network, but with ego included
set.seed(607)
ggraph(gr.ego) + 
  geom_edge_link() +
  geom_node_label(aes(label= name)) + 
  theme_graph(base_family = 'Helvetica')


# Indexing vertices and edges based on attributes or network structure      ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# View only female alters.
V(gr)[alter.sex=="Female"]

# View residence country just for female alters.
V(gr)[alter.sex=="Female"]$alter.res

# Frequencies of countries of residence of female alters.
V(gr)[alter.sex=="Female"]$alter.res %>%
  tabyl

# View nationality for alter 2833.
V(gr)[name=="2833"]$alter.nat

# Edit nationality for alter 2833
V(gr)[name=="2833"]$alter.nat <- "Italy"

# View all alters who know alter 2833 (vertices that are adjacent to vertex
# "2833").
V(gr)[nei("2833")]
# View the gender of all alters who know alter 2833.
V(gr)[nei("2833")]$alter.sex

# View all edges that are incident on alter 2833
E(gr)[inc("2833")]

# View the weight of these edges.
E(gr)[inc("2833")]$weight

# Max weight of any edge that is incident on alter 2833. 
E(gr)[inc("2833")]$weight %>% 
  max

# View all edges with weight==2 (alters who "maybe" know each other).
E(gr)[weight==2] 

# Conditions in R indexing can always be combined: view all "maybe" edges
# that are incident on alter 2833.
E(gr)[weight==2 & inc("2833")]


# ***** EXERCISES
# (1) Get the average alter closeness (vertex attribute $alter.clo) of Sri Lankan
# alters in the personal network gr. Use mean().
#
# (2) How many edges in the personal network gr involve at least one close
# family member and have $weight==2 (the two alters "maybe" know each other)?
# Remember that close family members have $relation=="Close family". Use inc().
#
# *****


# ---- end-igraph
