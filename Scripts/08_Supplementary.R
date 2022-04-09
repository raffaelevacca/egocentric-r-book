# Author: Raffaele Vacca | www.raffaelevacca.com | raffaele.vacca@unimi.it
#
# License:
# Creative Commons Attribution-NonCommercial-ShareAlike 
# CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/


############################################################################## #
###                        R TYPES AND CLASSES                              ====
############################################################################## #
## ---- types

# A numeric vector of integers.
n <- 1:100

# Let's check the class and type.
class(n)
typeof(n)

# A character object.
(char <- c("a", "b", "c", "d", "e", "f"))

# Class and type.
class(char)
typeof(char)

# Let's put n in a matrix.
(M <- matrix(n, nrow=10, ncol=10))

# Class/type of this object.
class(M)
# Type and mode tell us that this is an *integer* matrix.
typeof(M)

# There are character and logical matrices too.
char
(C <- matrix(char, nrow=3, ncol= 2))

# Class and type.
class(C)
typeof(C)

# Notice that a matrix can contain numbers but still be stored as character.
(M <- matrix(c("1", "2", "3", "4"), nrow=2, ncol=2))

class(M)
typeof(M)

# Let's convert "char" to factor.
char
(char <- as.factor(char))

# This means that now char is not just a collection of strings, it is a
# categorical variable in R's mind: it is a collection of numbers with character
# labels attached.

# Compare the different behavior of as.numeric(): char as character...
(char <- c("a", "b", "c", "d", "e", "f"))

# Convert to numeric
as.numeric(char)

# ...versus char as factor.
char <- c("a", "b", "c", "d", "e", "f")
(char <- as.factor(char))
as.numeric(char)

# char is a different object in R's mind when it's character vs when
# it's factor. Characters can't be converted to numbers,
# but factors can.

# ***** EXERCISE: 
# Using "as.factor()", convert the "educ" object to factor. Assign the
# result to "educ.fc". Print both "educ" and "educ.fc". Do you notice any
# difference in the way they are printed? Convert "educ" and "educ.fc" to
# numeric. Why is the result different?
# *****

# ---- end-types
############################################################################## #
###                         WRITING R FUNCTIONS                             ====
############################################################################## #
## ---- functions

# Basics of R functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Any piece of code you can write and run in R, you can also put in a function.

# Let's write a trivial function that takes its argument and multiplies it by 2.
times2 <- function(x) {
  x*2
}

# Now we can run the function on any argument.
times2(x= 3)
times2(x= 10)
times2(50)

# A function that takes its argument and prints a sentence with it:
myoutput <- function(word) {
  print(paste("My output is", word))
}

# Let's run the function.
myoutput("cat")
myoutput(word= "table")
myoutput("any word here")
# Not necessarily a useful function...

# Note that the function output is the last object that is printed at the end
# of the function code.
times2 <- function(x) {
  y <- x*2
  y
}
times2(x=4)

# If nothing is printed, then the function returns nothing.
times2 <- function(x) {
  y <- x*2
}
times2(x=4)

# A function will return an error if it's executed on arguments that are not
# suitable for the code inside the function. E.g., R can't multiply "a" by 2...
times2 <- function(x) {
  x*2
}
times2(x= "a")

# Let's then specify that the function's argument must be numeric.
times2 <- function(x) {
  stopifnot(is.numeric(x))  
  x*2
}

# Let's try it now.
times2(x= "a")
# This still throws and error, but it makes the error clearer to the user and 
# it immidiately indicates where the problem is.

# Using if, we can also re-write the function so that it returns NA with a
# warning if its argument is not numeric -- instead of just stopping with an
# error.
times2 <- function(x) {
  # If x is not numeric
  if(!is.numeric(x)) {
    # Give the warning
    warning("Your argument is not numeric!", call. = FALSE)
    # Return missing value
    return(NA)
    # Otherwise, return x*2
  } else {
    return(x*2)
  }
}

# Try the function
times2(2)
times2("a")

# Writing functions that calculate ego-network measures
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Now that we know how to write functions, we can convert any ego-network
# measure we calculated above to a general function.

# This is a function that takes the alter-level data frame for an ego, and
# calculates average closeness of alters.
avg_alt_clo <- function(df) {
  mean(df$alter.clo, na.rm = TRUE) 
}

# Let's run this function on our alter data frame for ego 28
avg_alt_clo(df= alter.attr.28)

# Function that takes the alter-level data frame for an ego, and calculates
# the proportion of female alters.
prop_fem <- function(df) {
  mean(df$alter.sex == "Female") 
}

# Apply the function to our data frame
prop_fem(df= alter.attr.28)

# Function that takes two arguments: the alter-level data frame for an ego, and
# the ego-level data frame; and calculates the proportion of alters of the same
# sex as ego.
same_sex <- function(df, ego.df) {
  
  # Join alter and ego attribute data.
  df <- left_join(df, ego.df, by= "ego_ID")
  
  # Return the measure
  mean(df$alter.sex == df$ego.sex) 
}

# Apply the function
same_sex(df= alter.attr.28, ego.df= ego.df)

# Function that takes the alter-level data frame for an ego, and calculates
# multiple compositional measures.
comp_meas <- function(df) {
  df %>%
    summarise(
      mean.clo = mean(alter.clo, na.rm=TRUE), 
      prop.fem = mean(alter.sex=="Female"), 
      count.nat.slk = sum(alter.nat=="Sri Lanka"), 
      count.nat.ita = sum(alter.nat=="Italy"), 
      count.nat.oth = sum(alter.nat=="Other")
    )
}

# Let's apply the function.
comp_meas(alter.attr.28)

# Function that takes an ego-network as igraph object, and calculates max alter
# betweenness.
max_betw <- function(x) x %>% igraph::betweenness(weights = NA) %>% max

# Apply to our graph.
max_betw(x= gr)

# Function that takes an ego-network as igraph object, and calculates the number
# of components and the number of isolates. 
comp_iso <- function(x) {
  
  # Get N components
  N.comp <- igraph::components(x)$no
  
  # Get N isolates
  N.iso <- sum(igraph::degree(x)==0)
  
  # Return output
  tibble::tibble(N.comp = N.comp, N.iso= N.iso)
}

# Apply the function
comp_iso(gr)

# Function that takes an ego-network as igraph object, and calculates the
# density of ties among alters who live in Sri Lanka.
sl_dens <- function(x) {
  
  # Get the vertex sequence of alters who live in Sri Lanka.
  alters.sl <- V(x)[alter.res=="Sri Lanka"]
  
  # Get the subgraph of those alters
  sl.subg <- induced_subgraph(x, vids = alters.sl) 
  
  # Return the density of this subgraph.
  edge_density(sl.subg)
}

# Run the function on our ego-network of ego 28.
sl_dens(x= gr)


# ***** EXERCISE: 
# Write a function that takes an ego's edge list as argument. The function
# returns the number of "maybe" edges in the corresponding personal network (use
# data frame indexing and sum()). Run this function on the edge lists of ego ID
# 47, 53 and 162. HINT: You should first try the code on one edge list from the
# list elist.all.list An edge is "uncertain" if the corresponding value in
# the edge list is 2. Remember that the ego IDs are in names(elist.all.list).
# *****

# ---- end-functions
############################################################################## #
###     IMPORTING EGOCENTRIC DATA WITH IGRAPH AND BASE R                    ====
############################################################################## #
## ---- import_1

# Alter attribute and tie data for one ego-network
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create graph with ego-network for ego 28

# Get the ego-network of ego ID 28 from an external edge list and a data set
# with alter attributes.

# Read in the edge list for ego 28. This is a personal network edge list.
(elist.28 <- read_csv("./Data/raw_data/alter_ties_028.csv"))

# Read in the alter attribute data for ego 28. 
(alter.attr.28 <- read_csv("./Data/raw_data/alter_attributes_028.csv"))

# Convert to graph with vertex attributes.
gr.28 <- graph_from_data_frame(d= elist.28, vertices= alter.attr.28, directed= FALSE)

# Note directed= FALSE

# A network object can be created from an adjacency matrix too: import adjacency
# matrix for ego-network of ego 28.

# Read the adjacency matrix.
adj.28 <- read.csv("./Data/raw_data/adj_028.csv", row.names=1) %>% 
  as.matrix
# Set column names the same as rownames
colnames(adj.28) <- rownames(adj.28)

# Convert to igraph
graph_from_adjacency_matrix(adj.28, mode="upper")

## ---- end-import_1

## ---- import_2
# All ego and alter attributes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Import all ego and alter attributes

# Import ego-level data.
(ego.df <- read_csv("./Data/raw_data/ego_data.csv"))

# Import the csv file with attributes of all alters from all egos.
(alter.attr.all <- read_csv("./Data/raw_data/alter_attributes.csv"))

# Note the number of rows (number of all alters).

# We can easily split a single data frame into a list of data frames, with one
# separate element for each ego.
alter.attr.list <- split(alter.attr.all, f= alter.attr.all$ego_ID)

# The result is a list of N alter attribute data frames, one for each ego.
length(alter.attr.list)

# The values of "f" in split() (the split factor) are preserved as list names.
# In our case these are the ego IDs.
names(alter.attr.list)

# The list element is the alter attribute data for a single ego. Get the
# attribute data frame for ego ID 94.
alter.attr.list[["94"]]

# The reverse of splitting is also easy to do: if we have N data frames in a
# list, we can easily bind them into a single data frame by stacking their rows
# together.
alter.attr.all.2 <- bind_rows(alter.attr.list)

# The result is a single alter-level data frame where each row is an alter, and
# alters from all egos are in the same data frame.
alter.attr.all.2
# 45 alters x 102 egos = 4590 alters.

# Edge list with alter-alter ties
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Let's import the csv containing a single edge list with alters from all
# egos.
(elist.all <- read_csv("./Data/raw_data/alter_ties.csv"))

# Exactly the same split by ego ID can be done for the edge list with
# alter-alter ties.
elist.all.list <- split(elist.all, f= elist.all$ego_ID)

# See the result.
length(elist.all.list)
names(elist.all.list)
elist.all.list[["94"]]

## ---- end-import_2

## ---- import_3
# Creating a list of ego-networks as igraph objects
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# We have a list of alter edge lists, with each list element being the edge list 
# of one ego.
length(elist.all.list)
names(elist.all.list)
elist.all.list[[1]]

# We saw that we can use igraph to convert one ego-network edge list
# to an igraph object. Let's do it for the first element (i.e., the first ego)
# of elist.all.list.
elist <- elist.all.list[[1]]
(gr <- graph_from_data_frame(d= elist, directed= FALSE))

# With map(), we can do the same thing for all egos at once.
gr.list <- purrr::map(.x= elist.all.list, 
                      ~ graph_from_data_frame(d= .x, directed= FALSE))

# The result is a list of graphs, with each element corresponding to an ego.

# The list has as many elements as egos.
length(gr.list)

# The element names are the ego IDs (taken from the names of elist.all.list).
names(gr.list)

# We can extract each ego's graph.
# By position.
gr.list[[1]]
# By name (ego ID).
gr.list[["94"]]

# If we have a function that takes two arguments, we can use map2(). For
# example, graph_from_data_frame() can take two arguments: the ego's edge list
# and the ego's alter attribute data frame. 

# We now have a list of edge lists, one for each ego.
length(elist.all.list)

# And a lsist of alter attribute data frames, one for each ego.
length(alter.attr.list)

# Let's run graph_from_data_frame() for the first ego, i.e., using the first
# element of each of the two lists.
(gr <- graph_from_data_frame(d= elist.all.list[[1]], 
                             vertices= alter.attr.list[[1]], directed= FALSE))

# We can now run the same function for all egos at once.
gr.list <- purrr::map2(.x= elist.all.list, .y= alter.attr.list, 
                       ~ graph_from_data_frame(d= .x, vertices= .y, directed= FALSE))

# The result is again a list, with each element corresponding to one ego.

# ---- end-import_3
############################################################################## #
###               MORE OPERATIONS WITH igraph                               ====
############################################################################## #
## ---- more_igraph

# Example of network visualization with igraph
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Get graph of one ego-network.
gr <- gr.28

# Vertex attribute with alter's country of residence.
V(gr)$alter.res

# Set red as default vertex color.
color <- rep("red", vcount(gr))
color

# Set color=="blue" when vertex (alter) lives in Italy.
color[V(gr)$alter.res=="Italy"] <- "blue"

# Set color=="green" when vertex (alter) lives in Other country.
color[V(gr)$alter.res=="Other"] <- "green"

# Plot using the "color" vector we just created.
set.seed(607)
plot(gr, vertex.label= NA, vertex.size=7, vertex.color= color)


# Add ego node to igraph object with igraph operations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Use the + operator to add a named vertex, e.g. ego, to a graph.
gr.ego <- gr + "ego"

# The graph has now one more node. It has the same number of edges because we
# haven't added edges between ego and alters.
vcount(gr)
vcount(gr.ego)

ecount(gr)
ecount(gr.ego)

# Ego is the 46th vertex.
V(gr.ego)

# Let's add all edges between ego and the alters.

# Ego's adjacency row. Note that the last (46th) cell is the diagonal cell
# between ego and itself.
gr.ego["ego",]

# Let's select ego's row without the 46th cell: i.e., only cells (ties) between
# ego and the 45 alters. 
gr.ego["ego", -46]

# Note that we can write this in a more general way, which works for any graph
# regardless of the number of vertices. Creating general code that applies to
# different circumstances is essential when writing R functions.
gr.ego["ego",-vcount(gr.ego)]

# Let's set ego's entire adjacency row with alters to 1's. This adds edges
# between ego and all alters.
gr.ego["ego",-vcount(gr.ego)] <- 1

# We now have 45 more edges.
ecount(gr.ego)

# See what the ego-network looks like now.
set.seed(123)
plot(gr.ego)

# ---- end-more_igraph
############################################################################## #
###                          NETWORKS IN STATNET                            ====
############################################################################## #
## ---- statnet

# Load the statnet package "network" (we use suppressMessages() to avoid
# printing verbose loading messages) and intergraph.
library(network)
library(sna)
library(intergraph)

# Let's convert our ego-network to a statnet "network" object.
net.gr <- asNetwork(gr)

# Check out the object's class.
class(net.gr)

# Print the object for basic information. Note that vertex and edge attributes
# are correctly imported.
net.gr

# Alternatively, we can create a network object from an adjacency matrix.

# Adjacency matrix for ego 20
head(adj.28)

# Convert to network object.
net <- as.network(adj.28, matrix.type = "adjacency", directed=FALSE, 
                  ignore.eval=FALSE, names.eval = "weight")

# NOTE: 
# -- ignore.eval=FALSE imports adjacency cell values as edge attribute.
# -- names.eval = "weight" indicates that the resulting edge attribute should be
# called "weight".

# Print the result. Note that there is only "vertex.names" as vertex attribute.
net

# The matrix column names are imported as vertex attribute "vertex name".
net %v% "vertex.names"

# We want to import more vertex attributes. Let's get alter attributes for ego 28.
vert.attr <- alter.attr.28

# We can now take columns from "vert.attr" and set them as attributes. However,
# we first need to make sure that nodes are in the same order in the attribute
# data frame (vert.attr) and in the network object (net).
identical(net %v% "vertex.names", as.character(vert.attr$alter_ID))

# Let's import, for example, the sex alter attribute.
# There's no sex attribute right now in the "net" object.
net %v% "alter.sex"
# Let's set it.
net %v% "alter.sex" <- vert.attr$alter.sex


# Basic statistics: Number of nodes, dyads, edges in the network.
network.size(net)
network.dyadcount(net)
network.edgecount(net)

# Simple plot
set.seed(2106)
plot(net)

# A network object can be indexed as an adjacency matrix.

# Adjacency matrix row for alter #3
net[3,]

# ...for first 3 alters
net[1:3,]

# Get a specific vertex attribute.
net %v% "alter.sex"

# Get a specific edge attribute.
net %e% "weight"

# When both igraph and statnet are loaded, you should call their functions using
# the "::" notation.

# E.g. there is a "degree()" function both in igraph and statnet (sna package).
# Check out the help for degree: ?degree

# If we just call "degree()", R will take it from the most recently loaded 
# package, in this case statnet (sna). This causes an error if degree() is
# applied to an igraph object.
degree(gr)

# Let's specify we want igraph's degree().
igraph::degree(gr)

# Before proceeding, let's un-load the sna package to avoid function name 
# overlaps with igraph.
detach("package:sna", unload=TRUE)

# ---- end-statnet
############################################################################## #
###                   ILLUSTRATIVE EXAMPLE                                  ====
############################################################################## #
## ---- case-study

# Load package
library(igraph)
library(ggraph)

# Load the data
load("./Data/data.rda")

# Visualization
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# We want to produce the same kind of plot for all our personal networks, with
# the following characteristics:
# -- Vertices sized by degree centrality.
# -- Sri Lankan alters in red, Italian alters in blue, Other alters in
# grey.
# -- Edges among Sri Lankans in light red, edges among Italians in light blue,
# all other edges in grey.

# Let's first try this on a single ego-network.

# Get an ego-network's igaph object.
gr <- gr.list[["45"]]

# Vertex sequence of Sri Lankans.
slk <- V(gr)[alter.nat=="Sri Lanka"]
# Vertex sequence of Italians.
ita <- V(gr)[alter.nat=="Italy"]
# Vertex sequence of Other.
oth <- V(gr)[alter.nat=="Other"]

# Edge attribute indicating which group the edge is between.
# Residual value.
E(gr)$edge_between <- "other"
# Overwrite value for edges among Sri Lankans.
E(gr)[slk %--% slk]$edge_between <- "slk-slk"
# Overwrite value for edges among Italians.
E(gr)[ita %--% ita]$edge_between <- "ita-ita"

# Calculate degree centrality and set it as vertex attribute in gr.
igraph::degree(gr)
V(gr)$degree <- igraph::degree(gr)

# Plot
set.seed(613)
ggraph(gr, layout= "fr") + 
  geom_edge_link(aes(color=edge_between), width=1, alpha= 0.7) + # Edges
  geom_node_point(aes(fill=alter.nat, size= degree), shape=21) + # Nodes
  scale_fill_manual(values=c("Italy"="blue", "Sri Lanka"="red", "Other"="grey")) + # Colors for nodes
  scale_edge_color_manual(values=c("ita-ita"="blue", "slk-slk"="red", "other"="grey")) + 
  # Colors for edges
  ggtitle(gr$ego_ID) + # Ego ID as plot title
  theme_graph(base_family = 'Helvetica') # Empty plot theme for network visualizations


# Let's now plot the first 10 personal networks. We won't print the plots in the
# GUI, but we'll export them to an external pdf file.

# Put all the code above in a single function
my.plot <- function(gr) {
  
  # Vertex sequence of Sri Lankans.
  slk <- V(gr)[alter.nat=="Sri Lanka"]
  # Vertex sequence of Italians.
  ita <- V(gr)[alter.nat=="Italy"]
  # Vertex sequence of Other.
  oth <- V(gr)[alter.nat=="Other"]
  
  # Edge attribute indicating which group the edge is between.
  # Residual value.
  E(gr)$edge_between <- "other"
  # Overwrite value for edges among Sri Lankans.
  E(gr)[slk %--% slk]$edge_between <- "slk-slk"
  # Overwrite value for edges among Italians.
  E(gr)[ita %--% ita]$edge_between <- "ita-ita"
  
  # Calculate degree centrality and set it as vertex attribute in gr
  degree(gr)
  V(gr)$degree <- degree(gr)
  
  # Plot
  set.seed(613)
  ggraph(gr, layout= "fr") + 
    geom_edge_link(aes(color=edge_between), width=1, alpha= 0.7) +
    geom_node_point(aes(fill=alter.nat, size= degree), shape=21) +
    scale_fill_manual(values=c("Italy"="blue", "Sri Lanka"="red", "Other"="grey")) + 
    scale_edge_color_manual(values=c("ita-ita"="blue", "slk-slk"="red", "other"="grey")) + 
    ggtitle(gr$ego_ID) +
    theme_graph(base_family = 'Helvetica')
}

# Run the function we just created on each of the first 10 personal networks
# with a for loop.

# Open pdf device
pdf(file = "./Figures/ego_nets_graphs.pdf")

# For loop to make plot for each i in first 10 ego-networks.
for (i in 1:10) {
  
  # Get the graph from list
  gr <- gr.list[[i]]
  
  # Run the plot function and print (needed for ggplot within a for loop).
  my.plot(gr) %>% print
}

# Close pdf device
dev.off()

# Same result, but with tidyverse code:

# Open pdf device
pdf(file = "./Figures/ego_nets_graphs.pdf")

# Get list of ego-networks (first 10 elements)
gr.list[1:10] %>%
  # walk: similar to map(), but calls a function for its side effects (here,
  # producing a plot)
  walk(~ my.plot(.x) %>% print)

# Close pdf device
dev.off()


# Compositional analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Frequency of different nationalities in the personal networks.

# Group the pooled alter attribute data frame by ego_ID.
alter.attr.all %<>% 
  group_by(ego_ID)

# Run the compositional measures.
comp.measures <- dplyr::summarise(alter.attr.all, 
                                  # N Italians
                                  N.ita= sum(alter.nat=="Italy"), 
                                  # N Sri Lankans
                                  N.slk= sum(alter.nat=="Sri Lanka"), 
                                  # N Others
                                  N.oth= sum(alter.nat=="Other"))

# Add the relative frequency of Italians.
comp.measures %<>%
  mutate(prop.ita = N.ita/45)

# Show result.
comp.measures

# Structural analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# We are going to write a function that takes an igraph object as argument and 
# returns 3 values:
# -- The density of ties among Italian alters
# -- The average degree of Italian alters.
# -- The average degree of Italian alters, as a proportion of the overall
# average degree.

# Write the function.
str.ita <- function(gr) {
  
  # Vertex sequence of Italians in the graph.
  ita <- V(gr)[alter.nat=="Italy"]
  
  # Number of Italians.
  n <- length(ita)
  
  # Number of all possible edges among Italians.
  possible <- n*(n-1)/2
  
  # Actual number of edges among Italians.
  actual <- E(gr)[ita %--% ita] %>% length
  # Notice that this is 0 if there are no Italian contacts, i.e. length(ita)==0.
  
  # Density of connections among Italians.
  dens.ita <- actual/possible
  # Notice that this is NaN if there are no Italian contacts.
  
  # Average degree of Italian contacts.
  avg.deg.ita <- igraph::degree(gr, V(gr)[alter.nat=="Italy"]) %>% mean
  # This is also NaN if there are no Italians.
  
  # Average degree of Italians, as a proportion of overall average degree.
  avg.deg.ita.prop <- avg.deg.ita/mean(igraph::degree(gr))
  # This is also NaN if there are no Italians. 
  
  # Return the 3 results as a tibble
  tibble(dens.ita = dens.ita, 
         avg.deg.ita = avg.deg.ita, 
         avg.deg.ita.prop = avg.deg.ita.prop)
}

# Run the function on all personal networks.
(str.ita.df <- map_dfr(gr.list, str.ita, .id= "ego_ID"))

# Convert ego_ID from character to numeric for consistency with ego attribute
# data.
str.ita.df %<>%
  mutate(ego_ID = as.numeric(ego_ID))

# Association with ego-level variables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Ego attribute data
ego.df

# Join ego-level data with compositional variables obtained above.
ego.df %<>% 
  left_join(comp.measures, by= "ego_ID")

# Merge ego-level data with structural variables obtained above.
ego.df %<>%
  left_join(str.ita.df, by= "ego_ID")

# See the result (just a subset of the variables)
ego.df %>%
  dplyr::select(ego_ID, ego.sex, ego.age, N.slk, prop.ita, dens.ita)

# Plot number by mean degree of Italians.
# To avoid warnings from ggplot(), let's remove cases with NA values on the
# relevant variables.
data <- ego.df %>%
  filter(complete.cases(N.ita, avg.deg.ita))

# Set seed for reproducibility (jittering is random).
set.seed(613)
# Get and save plot
p <- ggplot(data= data, aes(x= N.ita, y= avg.deg.ita)) + 
  geom_point(size=1.5, alpha=0.4, position=position_jitter(height=0.5, width=0.5)) + 
  labs(x= "N Italians", y= "Mean degree of Italians")
# Note that we slightly jitter the points to avoid overplotting.
# Print plot.
print(p)

# Same as above, but with ego_IDs as labels to identify the egos.
# Set seed for reproducibility (jittering is random).
set.seed(613)
# Get and save plot
p <- ggplot(data= data, aes(x= N.ita, y= avg.deg.ita)) + 
  geom_text(aes(label= ego_ID), size=3, alpha=0.7, 
            position=position_jitter(height=0.5, width=0.5)) + 
  labs(x= "N Italians", y= "Mean degree of Italians")
# Note that we slightly jitter the labels with position_jitter() so that egoIDs
# don't overlap and are more readable.
# Print plot.
print(p)

# Same as above, with color representing ego's educational level.
# Set seed for reproducibility (jittering is random).
set.seed(613)
# Get and save plot
p <- ggplot(data= data, aes(x= N.ita, y= avg.deg.ita)) + 
  geom_point(size=1.5, aes(color= as.factor(ego.edu)), 
             position=position_jitter(height=0.5, width=0.5)) + 
  theme(legend.position="bottom") + 
  labs(x= "N Italians", y= "Mean degree of Italians", color= "Education")
# Note that we slightly jitter the points to avoid overplotting.
# Print plot.
print(p)

# Do Sri Lankans who arrived to Italy more recently have fewer Italian contacts
# in their personal network?

# To avoid warnings from ggplot(), let's remove cases with NA values on the
# relevant variables.
data <- ego.df %>%
  filter(complete.cases(ego.arr, prop.ita))

# Set seed for reproducibility (jittering is random).
set.seed(613)
# Get and save plot
p <- ggplot(data= data, aes(x= ego.arr, y= prop.ita)) + 
  geom_point(shape= 1, size= 1.5, position= position_jitter(width= 1, height= 0.01)) + 
  geom_smooth(method="loess") + 
  labs(x= "Year of arrival in Italy", y= "Prop Italians in personal network")
# Print plot.
print(p)

# Do Sri Lankans with more Italian contacts in their personal network have higher
# income?

# To avoid warnings from ggplot(), let's remove cases with NA values on the
# relevant variables.
data <- ego.df %>%
  filter(complete.cases(prop.ita, ego.inc))

# Set seed for reproducibility (jittering is random).
set.seed(613)
# Get and save plot
p <- ggplot(data= data, aes(x= prop.ita, y= ego.inc)) + 
  geom_jitter(width= 0.01, height= 50, shape= 1, size= 1.5) + 
  geom_smooth(method="loess") + 
  labs(x= "Prop Italians in personal network", y= "Monthly income")
## Print plot.
print(p)

## ---- end-case-study
