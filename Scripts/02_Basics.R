# Author: Raffaele Vacca | www.raffaelevacca.com | raffaele.vacca@unimi.it
#
# License:
# Creative Commons Attribution-NonCommercial-ShareAlike 
# CC BY-NC-SA
# http://creativecommons.org/licenses/by-nc-sa/3.0/


############################################################################## #
###                          STARTING R                                     ====
############################################################################## #
## ---- starting 

# What's the current working directory? 
# getwd() 
# Un-comment to check your actual working directory.

# Change the working directory. 
# setwd("/my/working/directory") 
# (Delete the leading "#" and type in your actual working directory's path
# instead of "/my/working/directory")

# You should use R projects (.Rproj) to point to a working directory instead of
# manually changing it.

# Suppose that we want to use the package "igraph" in the following code.
library(igraph)

# Note that we can only load a package if we have it installed. In this case, I
# have igraph already installed. Had this not been the case, I would have have
# needed to install it: 
# install.packages("igraph").
# (Packages can also be installed through an RStudio menu item).

# Let's load another suite of packages we'll use in the rest of this script.
library(tidyverse)

# Note that whatever is typed after the "#" sign has no effect but to be printed
# as is in the console: it is a comment.


## ---- end-starting
############################################################################## #
###                         OBJECTS IN R                                    ====
############################################################################## #
## ---- objects 

# Create the object a: assign the value 50 to the name "a"
a <- 50

# Display ("print") the object a.
a

# Let's create another object.
b <- "Mark"

# Display it.
b

# Create and display object at the same time.
(obj <- 10)

# Let's reuse the object we created for a simple operation.
a + 3

# What if we want to save this result?
result <- a + 3

# All objects in the workspace
ls()

# Now we can view that result whenever we need it.
result

# ...and further re-use it
result*2

# Note that R is case-sensitive, "result" is different from "reSult".
reSult

# Let's clear the workspace before proceeding.
rm(list=ls())

# The workspace is now empty.
ls()



# ---- end-objects
############################################################################## #
###                     VECTORS, MATRICES, DATA FRAMES                      ====
############################################################################## #
## ---- vectors 

# Let's create a simple vector.
(x <- c(1, 2, 3, 4))

# Shortcut for the same thing.
(y <- 1:4)

# What's the length of x?
length(x)

# Note that when we print vectors, numbers in square brackets indicate positions
# of the vector elements.

# Create a simple matrix.
adj <- matrix(c(0,1,0, 1,0,0, 1,1,0), nrow= 3, ncol=3)

# This is what our matrix looks like:
adj

# Notice the row and column numbers in square brackets. 

# Normally we create data frames by importing data from external files, for
# example csv files.
ego.df <- read_csv("./Data/raw_data/ego_data.csv")

# View the result.
ego.df

# Note the different pieces of information that are displayed when printing a tibble
# data frame.

# ---- end-vectors
############################################################################## #
###            ARITHMETIC, STATISTICAL, COMPARISON OPERATIONS               ====
############################################################################## #
## ---- operations

# Just a few arithmetic operations between vectors to demonstrate element-wise
# calculations and the recycling rule.

(v1 <- 1:4)
(v2 <- 1:4)

# [1 2 3 4] + [1 2 3 4]
v1 + v2

# [1 2 3 4] + 1
1:4 + 1

# [1 2 3 4] + [1 2]
(v1 <- 1:4)
(v2 <- 1:2)
v1 + v2

# [1 2 3 4] + [1 2 3]
1:4 + 1:3

# Relational operations.

# Let's take a single variable from the ego attribute data: ego's age (in years)
# for the first 10 respondents. Let's put the result in a separate vector. This
# code involves indexing, we'll explain it better below.
age <- ego.df$ego.age[1:10]
age

# Note how the following comparisons are performed element-wise, and the value
# to which age is compared (30) is recycled.

# Is age equal to 30?
age==30

# The resulting logical vector is TRUE for those elements (i.e., respondents)
# who meet the condition.

# Which respondent's age is greater than 40?
age > 40

# Which respondent age values are lower than 40 OR greater than 60?
age < 40 | age > 60

# Which elements of "age" are lower than 40 AND greater than 30?
age < 40 & age > 30

# Notice the difference between OR (|) and AND (&).

# Is 30 in "age"? I.e., is one of the respondents of 30 years of age?
30 %in% age

# Is "age" in c(30, 35)? That is, which values of "age" are either 30 or 35?
age %in% c(30, 35)

# A logical vector can be converted to numeric: TRUE becomes 1 and FALSE becomes
# 0.
age > 45
as.numeric(age > 45)

# This allows us to use the sum() and the mean() functions to get the count and
# proportion of TRUE's in a logical vector.

# Count of TRUE's: Number of respondents (elements of "age") that are older than
# 40.
sum(age > 45)

# How many respondents in the vector are older than 30?
sum(age > 30)

# Proportion of TRUE's: What's the proportion of "age" elements (respondents)
# that are greater than 50?
mean(age > 50)


# ***** EXERCISES
#
# (1) Obtain a logical vector indicating which elements of "age" are smaller than 30
# OR greater than 50. Then obtain a logical vector indicating which elements of
# "age" are smaller than 30 AND greater than 50. Why all elements are FALSE in
# the latter vector?
#
# (2) Using the : shortcut, create a vector that goes from 1 to 100 in steps of 1.
# Obtain a logical vector that is TRUE for the first 10 elements and the last 10
# elements of the vector.
#
# (3) Use the age vector with relational operators and sum/mean to answer these
# questions: How many respondents are younger than 50? What percentage of
# respondents is between 30 and 40 years of age, including 30 and 40? Is there
# any respondent who is younger than 20 OR older than 70 (use any())?
#
# (4) How many elements of the vector 1:100 are greater than the length of that 
# vector divided by 2? Use sum().
#
# *****

# ---- end-operations
############################################################################## #
###                           SUBSETTING                                      ====
############################################################################## #
## ---- indexing

# Numeric subsetting                                                          ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Let's use our vector of ego age values again.
age

# Index its 2nd element: The age of the 2nd respondent.
age[2]

# Its 2nd, 4th and 5th elements.
age[c(2,4,5)]

# Fifth to seventh elements
age[5:7]

# Use indexing to assign (replace) an element.
age[2] <- 45

# The content of x has now changed.
age

# Let's subset the adjacency matrix we created before.
adj

# Its 2,3 cell: Edge from node 2 to node 3.
adj[2,3]
# Its 2nd column: All edges to node 2.
adj[,2]
# Its 2nd and 3rd row: All edges from nodes 2 and 3.
adj[2:3,]

# Logical subsetting                                                          ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Which values of "age" are between 40 and 60?

# Let's create a logical index that flags these values.
(ind <- age > 40 & age < 60)

# Use this index to extract these values from vector "age" via logical subsetting.
age[ind]

# We could also have typed directly:
age[age > 40 & age < 60]


# Subsetting data frames                                                     ---- 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# We'll use our ego-level data frame.
ego.df

# Numeric subsetting works on data frames too: it allows you to index variables.

# The 3rd variable.
ego.df[3]

# Note the difference with the double square bracket.
ego.df[[3]]

# What do you think is the difference?
class(ego.df[3])
class(ego.df[[3]])

# The [[ ]] notation extracts the actual column as a vector, while [ ] keeps
# the data frame class.

# We can also subset data frames as matrices.
# The second and third columns.
ego.df[,2:3]
# Lines 1 to 3
ego.df[1:3,]

# We can use name indexing with data frames, selecting variables by name
ego.df["ego.age"]
ego.df[["ego.age"]]

# The $ notation is very common and concise. It's equivalent to the [[ notation.
ego.df$ego.age

# This is the same as ego.df[[3]] or ego.df[["ego.age"]]
identical(ego.df[[3]], ego.df$ego.age)

# With tidyverse, this type of subsetting syntax is replaced by new "verbs" 
# (see section below):
# * Index data frame rows: filter() instead of []
# * Index data frame columns: select() instead of []
# * Extract data frame variable as a vector: pull() instead of [[]] or $


# ***** EXERCISES 
#
# (1) What's the mean age of respondents whose education is NOT "Primary"? Use
# the ego.age and ego.edu variables. Remember that the "different from"
# comparison operator is !=.
#
# (2) What is the modal education level (i.e., the most common education level)
# of respondents who are older than 40? Use table() or tabyl().
#
# (3) Create the fictitious variable var <- c(1:30, rep(NA, 3), 34:50). Use is.na()
# to index all the NA values in the variable. Then use is.na() to index all
# values that are *not* NA. Hint: Remember the operator used to negate a logical
# vector. Finally, use this indexing to remove all NA values from var.
#
# (4) Use the $ notation to extract the "ego.arr" variable in ego.df. Recode all 
# values equal to 2008 as 99. Hint: Index all values equal to 2008, then replace 
# them with 99 via the assignment operator. 
#
# *****


# ---- end-indexing
############################################################################## #
###                       THE TIDYVERSE SYNTAX                              ====
############################################################################## #
## ---- tidyverse

# The pipe operator                                                         ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Ego education variable 
(edu <- ego.df$ego.edu)

# Frequency of educational categories in the data
table(edu)

# Average frequency
mean(table(edu))

# Let's re-write this with the pipe operator
edu |> 
  table() |> 
  mean()

# Subsetting data frames with dplyr: filter and select                      ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Filter to egos who are older than 40
ego.df |> 
  filter(ego.age > 40)

# Filter to egos with Primary education
ego.df |> 
  filter(ego.edu == "Primary")

# Combine the two conditions: Intersection.
ego.df |> 
  filter(ego.age > 40 & ego.edu == "Primary")

# Combine the two conditions: Union.
ego.df |> 
  filter(ego.age > 40 | ego.edu == "Primary")

# Note that the object ego.df hasn't changed. To re-use any of the filtered data
# frames above, we have to assign them to an object.
ego.df.40 <- ego.df |> 
  filter(ego.age > 40)

ego.df.40

# Select specific variables
ego.df.40 |>
  dplyr::select(ego.sex, ego.age)

# As usual, we can re-assign the result to the same data object
ego.df.40 <- ego.df.40 |> 
  dplyr::select(ego.sex, ego.age)

ego.df.40

# Pull a variable out of a data frame, as a vector
ego.df |>
  pull(ego.age)

# This is the same as
ego.df$ego.age

# ***** EXERCISES 
#
# (1) Use the [ , ] notation to extract the education level and income of egos who
# are younger than 30. Then do the same thing with the dplyr::filter() function.
# 
# *****

# ---- end-tidyverse