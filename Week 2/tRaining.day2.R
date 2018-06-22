#' First things first, clear the workspace
rm(list = ls())


# Sample Function----
?sample
sample(1:10) # will give a random permutation each time you run the vector,
# with length of 10

sample(1:10, size = 4) # random permutation each time, length of 4
sample(1:10, size = 20, replace = T) # must use repalce = T if your size is
# greater than your "x"
# Discrete values----
x <- sample(letters, size = 10, replace = T)
x

# Unique will return unique values in the vector
unique(x) # return in order of encounter in vector, not alphabetical

# similar to unique, "duplicated" asks per element in vector
# "has this occured before?
duplicated(x) # returns a vector of logicals, always as long as its argument

# unique(x) returns the same answer as x[!duplicated(x)]
unique(x)
x[!duplicated(x)]

x <- sample(letters, size = 40, replace = T)
sum(x == "q")
freq <- table(x) # counts the number of occurances of each element (letter)
str(freq) # the class is table

# can index freq like a vector
freq[c("a", "m", "y")]

# now, with numbers
y <- sample(1:15, size = 20, replace = T)
y
freq.y <- table(y) # the "headers" of this table are "quoted"
freq.y["7"] # how many times does 7 occur?
freq.y[7] # how many times does the 7th element occur

# combining two samples and using cross-tabulation
x1 <- sample(1:10, 30, T)
x2 <- sample(letters[1:10], 30, T)
freq12 <- table(x1, x2) # numbers are rows, letters go in columns
freq12

freq12["4", "f"] # indexing based on numbers being rows, letters in columns
freq12["8", "c"]

# Can convert these tables to dataframes
freq12.df <- as.data.frame(freq12)
freq12.df

freq12.df[freq12.df$x1 == "10" & freq12.df$x2 == "g", "Freq"]

# Data Selection and Manipulation----
x <- sample(letters, 10)
"z" %in% x # is z in x?
x
"b" %in% x # return vector is same length as what's on the left of %in%

c("x", "f") %in% x

letters # list all letters in the vector "letters"
letters %in% x # What letters of the vector letters are in x? Logical vector
letters[letters %in% x] # What letters of vector "letters" are in x? returns letters
which(letters %in% x) # returns the index of the letters that are true
# extract indices that match a condition

# Grab the index of the min and max for USArrests$UrbanPop
which.min(USArrests$UrbanPop)
USArrests[45, ]

which.max(USArrests$UrbanPop)
USArrests[5, ]

# Can also combine into one line
USArrests[which.min(USArrests$UrbanPop), ] # returns the first encounter of a max
USArrests[which.max(USArrests$UrbanPop), ] # returns the first encounter of a min

# This doesn't always give the same as below:
USArrests[USArrests$UrbanPop == max(USArrests$UrbanPop), ]
USArrests[USArrests$UrbanPop == min(USArrests$UrbanPop), ]
# This shows all instances of max or min within the dataset, not just 1st encounter


?union
x1 <- sample(letters, 10)
x2 <- sample(letters, 10)
x1
x2
union(x1, x2) # combines the two and discards duplicates
unique(c(x1, x2)) # combines x1 and x2 and returns only the unique values
intersect(x1, x2) # returns only values that both x1 and x2 have
setdiff(x1, x2) # returns elements that x1 has that x2 doesn't
setdiff(x2, x1) # returns elements that x2 has that x2 doesn't

# allows you to index things with names in the same way we'd use the "-" sign
USArrests[setdiff(rownames(USArrests), c("Alabama", "Illinois", "Maryland")), ]
# index and return all rows apart from Alabama, Illinois, Maryland

# Sorting & Ordering Data----
x <- sample(1:5, 10, T)
x
rev(x) # returns x in reverse order
sort(x) # returns sort in order
sort(x, decreasing = T) # returns sort in reverse order

order(x) # returns the indexes of elements as if they were ordered,
# not the elements themselves
x <- sample(letters[1:5], 10, T)
x
order(x)
x[order(x)] # returns the elements, not the indeces

#  better option, use sort(x)
sort(x)
# only works on vectors, not data.frames

# Now, try with a dataframe
order(USArrests$Murder) # returns indices of elements in order
which.min(USArrests$Murder) # the 34th element is the minimum
USArrests[c(34, 19, 29), ]

USArrests[order(USArrests$Murder), ] # returns elements via calling indeces
USArrests[order(USArrests$Murder, USArrests$Assault), ]
# sorts first by murder, then by assault rate, by breaking ties between murder
# by assault
USArrests[order(USArrests$Murder, decreasing = T), ]

# Decreasing murder and increasing assault, make assignments
# for assault, use "-" to increase
USArrests[order(USArrests$Murder, -USArrests$Assault, decreasing = T), ]

# Ordering based on a column/vector that doesn't exist in the dataframe
USArrests[order(USArrests$Murder / USArrests$Assault), ]
# allows you to order by the ratio between murder/assault without making a
# vector or adding a column to the dataframe

# Character Vectors----
x <- c("This is a sentence", "Hello World", "This is the third element")
x
str(x) # it's a character vector
length(x) # it has three elements
nchar(x) # how many characters are in each element
length(nchar(x)) # how many elements

substr(x, 1, 3) # grab the 1st through 3rd character of each element
substr(x, 5, 13) # if your "stop" is longer than the length of characters per
# element, it trunkates the return

# use substr to replace
substr(x, 1, 4) <- "That"
x

# Substr is useful for consistently formatted vectors/ consistent delimeters
# (eg. field IDs)
substr(x, 1, 3) # 1st three characters of each element
substr(x, 4, max(nchar(x))) # from 4th character to end

# String split
strsplit(x, " ") # splits based on spaces, returns a list bc each element
# may not have the same number of " "
x.split <- strsplit(x, " ")
str(x.split)
x.split[[1]]

x.split <- strsplit(x, "") # each character is an element in a list of elements
nchar(x.split[[1]]) # each element in list 1 is 1 character long
length(x.split[[1]]) # list 1 is 18 elements long
nchar(x)

# For example, file names
dir()
strsplit(dir(), "\\.")

# Combining vectors ----
# with paste, separates pasted things with a space by default
x1 <- letters[1:6] # vector with 6 letters
x2 <- 1:6
paste(x1, x2)
paste(x1, 1:2) # pastes while recycling smaller of two
paste(x1, 1:4)
paste(x1, x2, sep = "--")
paste(x1, x2, sep = "") # same as paste0
paste0(x1, x2)

# put everything together! with collapse
paste(x1) # returns the vector
paste(x1, collapse = "*") # collapse argument specifies what's separating
# combined elements
paste(x1, x2, sep = "-", collapse = "*") # pastes x1 and x2 with sep"-",
# then collapse with * in between elements

# Changing Case----
USArrests
str(USArrests) # columns are capitalized, hard for when you're referring back
colnames(USArrests) <- tolower(colnames(USArrests))
str(USArrests)

# Pattern Matching----
x <- c("Here is some text", "This is more text", "I am number 1", "here is your 2nd elememtn")
grep("Here", x, value = T) # return the elements that have "Here"
grep("[[:digit:]]", x) # returns the number of the elements that have digits

# find and replace
gsub("is", "are", x)
# based on pattern matching, it will replace 1st argument with 2nd argument
# ????? Is there a way to use grep or other pattern replace without case sensitivity

# Combining fun----
# Let's bring it all together
x <- iris
colnames(x) <- tolower(colnames(x))
#  give every row a unique identifier, where in the name the species is included
#  e.g. cetosa1, cetosa2, etc...versicola1, versicola2
table(x$species)
rep(1:3, 4) # replicates vector 1:3 ,4 times (1, 2, 3, 1, 2, 3, etc)
rep(1:3, each = 4) # returns 1, 1, 1, 1, 2, 2, etc.
rep(letters[1:3], c(4, 3, 6)) # returns 4 a's, 3 b's, and 6 c's

# want each unique species element to be numbered 1 to it's max
# here, each species all have 50 rows
nums <- rep(1:50, 3) # first step
spp <- rep(unique(x$species), each = 50) # 2nd step
ids <- paste(spp, nums, sep = "_")
ids
length(ids) # length is the same as the number of rows in x (150)

x$ids <- ids # put this vector in the dataframe, now each row has a unique id
x

# sort by IDs (since they're sequential)
x <- x[order(x$ids), ]
head(x) # incorrectly orders, 1, 10, 11, 12, etc

# sort first by length (nchar) of names, then break ties by the value
x <- x[order(nchar(x$ids), x$ids), ]
head(x)
# works best if the 1st part of the id, (e.g. setosa) is same throughout,
# rather than the three species here

# Apply functions----

# 1st, understand sample function
sample(10) # permuted vector of sample of numbers 1 to 10

# __lapply----
# has two arguments
# 1. vector, list, something with elements
# 2. function to apply to each thing sequentially

lapply(1:5, sample) # returns a list, with 5 elements, applies 1, 2, 3, etc to sample
x <- lapply(1:5, sample)

x.mean <- lapply(x, mean) # mean of each element in the list
x.mean # x.mean is a list, where each element is the mean of elements from the list x

# what type of variable each column in dataset iris is?
z <- lapply(iris, class)
z
str(z)

# mean of the numeric values in each column
# but, we know which columns are numeric
m <- lapply(iris[, 1:4], mean)
m

# __sapply----
# says, if everytime a thing gets called and I get the same size return,
# combine into a vector
n <- sapply(iris[, 1:4], mean) # every call to "mean" returns somehting of same length
n

# range, gives min and max (2 numbers)
range(iris$Petal.Length)
o <- sapply(iris[, 1:4], range)
o

lapply(iris[, 1:4], range)

lapply(1:5, sample)
sapply(1:5, sample)

sample(1, size = 10, replace = T)
sample(5, size = 10, replace = T)

# lapply(x, fun, ...) the ... is extra arguments for the function you're using
# the functino sample has extra arguments like size and replace, so we can use those..
lapply(1:5, sample, size = 10, replace = T) # each list is the same length, soooo...
sapply(1:5, sample, size = 10, replace = T) # makes a matrix

# also extra arguments you can pass for the sapply
sapply(1:5, sample, size = 10, replace = T, simplify = F)
# returns the same thing as lapply

# __apply ----
# used on matrices or things with two dimensions
# specift to loop over either rows or columns
i <- sample(1:10, 100, replace = T)
i
mat <- matrix(i, nrow = 20)
mat

sapply(mat, mean) # elements of mat are cells, so it doesn't iterate the way we want it to
colMeans(mat)
rowMeans(mat)
colSums(mat)
rowSums(mat)
# apply has three arguments: x, margin (dimension you want to slice over)
# margins: 1 is rows, 2 is columns

# let's get the column means (gets the same information as colMeans)
apply(mat, 2, mean)

# __tapply----
# mean of sepal length for each species
# Arguments (3)
# 1. vector you want to summarize (e.g. Sepal.length)
# 2. thing that groups it (e.g. species in iris)
# 3. function
tapply(iris$Sepal.Length, iris$Species, mean)

# t apply on two subgroups (e.g. speceis and sex)
subgrp <- sample(c("a", "b"), 150, T)
subgrp
tapply(iris$Sepal.Length, list(iris$Species, subgrp), mean)

loc <- sample(c("n", "s"), 150, T) # now, by species, subgroup, and location
tapply(iris$Sepal.Length, list(iris$Species, subgrp, loc), mean)

# __aggregate----
# returns a dataframe, won't make it multidimensional
# aggregate R object by list of subgoups
aggregate(iris[, 1:4], list(iris$Species), mean)
# to name the first return column Species
aggregate(iris[, 1:4], list(Species = iris$Species), mean)

aggregate(iris[, 1:4], list(Species = iris$Species), range)
# range returns 2 values, but this doesn't make the df multidimensional, instead adds columns
# could change functiont to include .min and .max on column names in return df

# __mapply----
mapply(sample, x = 5:10, size = c(20, 4), replace = T)
# multiple vectors to apply to a function


# __split----
# divides things into groups
# two arguments
# 1. thing you want to split
# 2. set of factors you'll use to split'
x1 <- sample(1:10, 100, T)
x2 <- sample(letters[1:3], 100, T)
table(x2)
x.split <- split(x1, x2)
x.split # items in list are named based on unique values in x2

# could be a dataframe, split iris df by species
iris.split <- split(iris, iris$Species)
iris.split # results in a list with three df's, each a uniqe species

# works with a df in an interative fashion to apply a function over the split things
summary(iris)
# summary for each species
lapply(split(iris, iris$Species), summary)


str(USArrests)

# Functions----
# THree components: input values, body that does stuff to the values, and a return
# sometimes, one of those components is set to nothing
# last line of code is usually the return value
# specifying return() is more useful for if/then statements

# __example function----
# arguments go into function(arguments)
# fucntion can only return one object (usually lists)
isBetween1 <- function(x, a, b){
  gt.a <- x > a
  lt.b <- x < b
  btwn <- gt.a & lt.b
  return(btwn) # btwn is a logical
}
isBetween
str(isBetween)
class(isBetween)

isBetween(x = 6, a = 2, b = 10)
isBetween(6, 2, 10) # same but shorter, returns logical value TRUE or FALSE
gt.a # does not exist in the workspace, only in the workspace of your function while
# running that code

# let's write that in a different way
# develop as script first, then choose what goes in as part of the {environment}
# and what goes in as an (argument)

x <- 6
a <- 2
b <- 10 # these three will end up in argument

gt.a <- x > a
lt.b <- x < b
btwn <- gt.a & lt.b # these three will be in the environment
btwn # is the last line in the function, this is the RETURN line

# option 2
isBetween2 <- function(x, a, b){
  gt.a <- x > a
  lt.b <- x < b
  gt.a & lt.b
}

# option 3
isBetween3 <- function(x, a, b){
  x > a & x < b
}

# option 4
isBetween4 <- function(x, a, b) x > a & x < b

# write out functions long, then compress!
# commenting is good

isBetween4 <- function(x, a, b){
  # check if x is greater than a and less than b
  gt.a <- x > a
  lt.b <- x < b
  btwn <- gt.a & lt.b
  return(btwn) # btwn is a logical
}

# Genus spp Example----
# Working with a dataset
spp <- read.csv("F:/EMTS PUD/R Training/Week 2/tblCodeSpecies.csv", stringsAsFactors = F)

# genus <- spp$GENUS # create genus object
# species <- spp$SPECIES

abbrev0 <- function(genus, species) {
  g <- substr(genus, 1, 1) # want first character from genus
  spp <- substr(species, 1, 3) # and first three characters from species
  paste(g, spp)
}

gspp <- abbrev(spp$GENUS, spp$SPECIES) # these are un-named arguments
head(gspp)
# Since we did not name our argments, it is matched by position to arguments defined
# in function

# let's add another arugment
abbrev1 <- function(genus, species, num.spp) {
  g <- substr(genus, 1, 1) # want first character from genus
  spp <- substr(species, 1, num.spp) # and first three characters from species
  paste(g, spp)
}

abbrev1(spp$GENUS, spp$SPECIES, 5)

# BUT what if you change the order of the arguments
abbrev1(5, spp$GENUS, spp$SPECIES) # NOPE

abbrev1(
  num.spp = 5, 
  genus = spp$GENUS, 
  species = spp$SPECIES
  ) # by naming the arguments, it will run reguardless 
    # of the order you place the arguments

abbrev2 <- function(genus, species, num.g, num.spp) {
  g <- substr(genus, 1, num.g) # want first character from genus
  spp <- substr(species, 1, num.spp) # and first three characters from species
  paste(g, spp)
}

abbrev2(spp$GENUS, spp$SPECIES, 1, 3) # un-named, but given in same order as function
abbrev2(num.spp = 3, num.g = 1, spp$GENUS, spp$SPECIES)
abbrev2(3, 1, g = spp$GENUS, s = spp$SPECIES)
# uses partial mathching to unambiguously match your naming to a named argument
#' genus will match with "g"
#' species will match with "s"
#' num.g will match with "num.g"
#' num.s will match with "num.spp"


abbrev3 <- function(genus, species, num.g = 1, num.spp = 3) { 
  # set deafult value for num.g and num.spp
  g <- substr(genus, 1, num.g) # want first character from genus
  spp <- substr(species, 1, num.spp) # and first three characters from species
  paste(g, spp)
}

abbrev3(g = spp$GENUS, s = spp$SPECIES)
# defaults to 1 character for genus, and 3 characters for species

# if you want to use a value other than the default, you need to define that before running
abbrev3(g = spp$GENUS, s = spp$SPECIES, num.g = 2)


# ...Elipses----
abbrev4 <- function(genus, species, num.g = 1, num.spp = 3, ...) { 
  # set deafult value for num.g and num.spp
  g <- substr(genus, 1, num.g) # want first character from genus
  spp <- substr(species, 1, num.spp) # and first three characters from species
  paste(g, spp, ...) # allows you to pass additional arguments into paste
}

abbrev3(spp$GENUS, spp$SPECIES, size = 5) # will bring up an error
# because there is no argument for this in the function

abbrev4(spp$GENUS, spp$SPECIES, size = 5) # argument works

abbrev4(spp$GENUS, spp$SPECIES, sep = ". ") # extra argument goes to paste function

# Function tips----
#' - lowercase arguments
#' - function names as verbs (e.g. abbreviate)
#' - if function name is long, (abbreviate species) -> abbreviateSpecies

# Iris Dataset----
# Range of all columns in Iris (apart from species)
sapply(iris, range) # error: range not meaningful for factors

# restrict to first 4 columns
x <- sapply(iris[ ,1:4], range) # but, want to name the rows as min and max 
rownames(x) <- c("min", "max")

my.range <- function(x){
  c(min = min(x), max = max(x))
}

# Option 1 to get answer
sapply(iris[ , 1:4], my.range) # not calling a function, defining function name

# Option 2, get same answer without having to define function
# not creating my.range, so this is known as a "disposable function"
sapply(iris[ , 1:4], function(x){
  c(min = min(x), max = max(x))
})

# min, max, median
sapply(iris[ , 1:4], function(x){
  c(min = min(x), 
    max = max(x),
    median = median(x),
    sd = sd(x)
    )
}) # {curly braces} end environment call for function, (parantheses) end call for sapply

# Code Flow----

my.range2 <- function(x){
  if(class(x) == "numeric") {
    c(min = min(x), max = max(x))
  } else {
      c(min = NA, max = NA)
  }
}

sapply(iris, my.range2)

# could also use disposable function
sapply(iris, function(x){
  if(class(x) == "numeric") {
    c(min = min(x), max = max(x))
  } else {
    c(min = NA, max = NA)
  }
})

# Homework----
# Answer all questions in a script (.R) file. Use comments (# or #') to explain steps in code.

#   __1. ----
# In the “tblCodeSpecies.csv” data set, how many named species of the family “Balaenopteridae” are
#   there?

library(readr)
spp <- read.csv("Week 2/tblCodeSpecies.csv", stringsAsFactors = F) # read in the data first
colnames(spp) <- tolower(colnames(spp)) # make the column names lower case
blp <- spp[spp$family == "BALAENOPTERIDAE", ] # extract all rows that are in family Balaenopteridae
blp.spp <- blp$species # take just the column species from the blp dataset
blp.spp <- c(NA, blp.spp) # makes the same thing as line before
sum(blp.spp != "" & !is.na(blp.spp)) # take the sum of things that are not empty and not NA

#  or

sum(blp.spp != "", na.rm = T)

# or

spp$family %in% "BALAENOPTERIDAE" # this is a logical that reads T/F for each entry in 
                                  # the line, but we want the number
sum(spp$family %in% "BALAENOPTERIDAE") # returns 8, because one of the taxa doesn't have species

#     __2.---- 
# In the “tblCodeSpecies.csv” data set, what is the mean number of species per family of Cetaceans
#   (order Cetacea) and Pinnipeds (suborder Pinnipedia)?

# First, an extraction for cetaceans
# run your code through one subset of the dataframe, then you can work to 
cet <- spp[spp$order == "CETACEA", ]
mean(table(cet$family)) # count of the number of entries per family, and take the mean
#  OR

fam.spp <- table(cet$family)
mean(fam.spp)

# get a list of all orders
orders <- split(spp, spp$order) # turn spp into a list of df's, each is an order
# each thing is a data frame, we want to get a rable of rows per family per order
# use sapply and a function
mean.fam <- function(df) {
  fam.spp <- table(df$family)
  mean(fam.spp)
}
# test the function on cet
mean.fam(cet)

# now, pass the function to sapply to run the function over the list
sapply(orders, mean.fam)

# can also write the function within the sapply
sapply(split(spp, spp$order), function(df){ # split used to be making "orders"
  mean(table(df$family)) # used to be the fam.spp from above
})

# same thing for suborder pinnipeds
pin <- spp[spp$suborder == "PINNIPEDIA", ]
fam.spp <- table(pin$family)
mean(fam.spp)

# build a function!
# want to use this as an example
cet <- spp[spp$order == "CETACEA"]

# build the arguments
df <- spp
taxa.level <- "order"
search.taxa <- "CETACEA"

# next, where are lookig
user.df <- df[ df[ , taxa.level] == search.taxa,]
user.df <- df[ df[[taxa.level]] == search.taxa,] # same as above
user.df
# another way
# be able to put in suborder for taxa.level and suborder is pinnipedia
taxa.level <- 
search.taxa <- taxa.level %in% "PINNIPEDIA"
taxa.level <- spp[,x]

meanSppPerFam <- function(df, taxa.level, search.taxa){
  # extract data.frame for search.taxa in taxa.level column
  user.df <- df[ df[[taxa.level]] == search.taxa,] # taken from above
  # count number of rows per family
  fam.spp <- table(user.df$family)
  # return mean of number of rows
  mean(fam.spp)
}

meanSppPerFam(spp, "order", "CETACEA")
meanSppPerFam(spp, "suborder", "PINNIPEDIA")
meanSppPerFam(spp, "order", "Carnivora") # this one has both lowercased, so we need to change the code

# now, it doesn't matter the case that user uses for taxa.level or search taxa
meanSppPerFam2 <- function(df, taxa.level, search.taxa){
  # change case of column names
  colnames(df) <- tolower(colnames(df))
  # lower case of taxa level to match column names
  taxa.level <- tolower(taxa.level)
  
  # lower case of contents of taxa.level vector
  df.taxa.level <- tolower(df[[taxa.level]])
  # lower case of search.taxa to match taxa.level vector
  search.taxa <- tolower(search.taxa)
  print(search.taxa)
  
  # extract data.frame for search.taxa in taxa.level column
  user.df <- df[df.taxa.level == search.taxa, ] # taken from above
  
  # count number of rows per family
  fam.spp <- table(user.df$family)
  
  # return mean of number of rows
  mean(fam.spp)
}

meanSppPerFam2(spp, "ordeR", "CeTacea")

# ___2. Errorchecks----
# writing in error checks along the way in the function
# IMPORTANT----
# look at everything in function(here...), and think about it's *requirements*
# now, it doesn't matter the case that user uses for taxa.level or search taxa
meanSppPerFam3 <- function(df, taxa.level, search.taxa){
  # df MUST be class = data frame
  if(class(df) != "data.frame") { # do all the things in the curly brackets if not df
    stop("'df' is not a dataframe")
  } else { 
    # change case of column names IF df is a dataframe, then pass next check
    colnames(df) <- tolower(colnames(df))
  }
  
  # separate the two arguments for taxa.level so you can be specific with informing user
  # about errors
  # taxa level MUST be class = character vector
  if(class(taxa.level) != "character") { # do all the things in the curly brackets if not df
    stop("'taxa.level' is not a character vector")
  } 
  
  # taxa level MUST be length = 1 
  if(length(taxa.level) != 1) { # do all the things in the curly brackets if not df
    stop("'taxa.level' is not one element")
  } else {
    # lower case of taxa level to match column names
    taxa.level <- tolower(taxa.level)
    # is taxa.level a column name in the data frame (df)
    if(!taxa.level %in% colnames(df)) {
      warning("'taxa.level' is not in 'df")
      return(NA)
    }
  }
  
  # if above checks pass, df.taxa.level will always work
  # lower case of contents of taxa.level vector
  df.taxa.level <- tolower(df[[taxa.level]])
  
  # separate the two arguments for search.taxa so you can be specific with informing user
  # about errors
  # search.taxa MUST be class = character vector
  if(class(search.taxa) != "character") { # do all the things in the curly brackets if not df
    warning("'search.taxa' is not a character vector")
    return(NA)
  } 
  
  # search taxa MUST be length = 1 
  if(length(search.taxa) != 1) { # do all the things in the curly brackets if not df
    warning("'search.taxa' is not one element")
  } else {
    # lower case of search.taxa to match taxa.level vector
    search.taxa <- tolower(search.taxa)
    print(search.taxa)
    # is search.taxa a thing in the column that taxa level is referring to (df.taxa.level)
    if(!search.taxa %in% df.taxa.level) {
      warning("'search.taxa' is not in 'df.taxa.level")
      return(NA)
    }
  }
  
  # extract data.frame for search.taxa in taxa.level column
  user.df <- df[df.taxa.level == search.taxa, ] # taken from above
  
  # count number of rows per family
  fam.spp <- table(user.df$family)
  
  # return mean of number of rows
  mean(fam.spp)
}

# check your function
meanSppPerFam3("spp", "1", "CeTacea")


# WHERE WE'RE HEADED, want the user to have to work less, only provide what's needed
meanSppPerFam(spp, search.taxa)
# problem: several columns, looking for something across them -> try an sapply
# Is "CETACEA" in any of the columns of x
in.taxa <- sapply(spp, function(x){
  "CETACEA" %in% x
})
in.taxa # a named logical vector
names(in.taxa)[in.taxa] # return the order that is TRUE for the logical argument using indexing

# now, let's put that into a function that we can supply with a df, and a search.taxa
taxaLevel <- function(df, search.taxa){
  #' df has to be a data.frame
  #' search.taxa has to be a character vector
  #' search.taxa has to be length of 1
  #' search.taxa has to be in correct case
  #' search.taxa has to exist in only one column (sometimes order and genus are same)
  #' 
  in.taxa <- sapply(df, function(x){ # in.taxa will always have the same # of columns as df
    tolower(search.taxa) %in% tolower(x) # allows you to input search.taxa in lowercase
  })
  # check that search.taxa is in df
  if(!any(in.taxa)) {# if there are not TRUE's in in.taxa
    warning("'search.taxa' not found in 'df', NULL returned.")
    return(NULL)
}
  # tell us if there is more than one taxa level that contains search.taxa
  if(sum(in.taxa) > 1) warning("more than one taxa level found")
  
  # return the taxa levels
  names(in.taxa)[in.taxa]
}

# check it
taxaLevel(spp, "cetacea")

# now, let's put taxaLevel into our previous function
meanSppPerFam4 <- function(df, search.taxa){
  # df MUST be class = data frame
  if(class(df) != "data.frame") { # do all the things in the curly brackets if not df
    stop("'df' is not a dataframe")
  } else { 
    # change case of column names IF df is a dataframe, then pass next check
    colnames(df) <- tolower(colnames(df))
  }
  
  # put in function from above, passing
  # is search taxa in more than one column (taxal.level)
  taxa.level <- taxaLevel(df, search.taxa)
  if(is.null(taxa.level)) return(NULL)
  if(length(taxa.level) > 1) {
    warning("returning NULL")
    return(NULL)
  }
  
  # if above checks pass, df.taxa.level will always work
  # lower case of contents of taxa.level vector
  df.taxa.level <- tolower(df[[taxa.level]])
  
  # separate the two arguments for search.taxa so you can be specific with informing user
  # about errors
  # search.taxa MUST be class = character vector
  if(class(search.taxa) != "character") { # do all the things in the curly brackets if not df
    warning("'search.taxa' is not a character vector")
    return(NA)
  } 
  
  # search taxa MUST be length = 1 
  if(length(search.taxa) != 1) { # do all the things in the curly brackets if not df
    warning("'search.taxa' is not one element")
  } else {
    # lower case of search.taxa to match taxa.level vector
    search.taxa <- tolower(search.taxa)
    print(search.taxa)
    # is search.taxa a thing in the column that taxa level is referring to (df.taxa.level)
    if(!search.taxa %in% df.taxa.level) {
      warning("'search.taxa' is not in 'df.taxa.level")
      return(NA)
    }
  }
  
  # extract data.frame for search.taxa in taxa.level column
  user.df <- df[df.taxa.level == search.taxa, ] # taken from above
  
  # count number of rows per family
  fam.spp <- table(user.df$family)
  
  # return mean of number of rows
  mean(fam.spp)
}

# check it
meanSppPerFam4(spp, "cetacea")

in.taxa # a named logical vector
names(in.taxa)[in.taxa] # return the order that is TRUE for the logical argument using indexing

# ___2: if/else----
# if requires one thing for thelogical test, cannot run two things (will warn and run first)
if("Mesoplodon" %in% spp$genus) print("Hello")
if(c("eric", "Mesoplodon" %in% spp$genus)) print("Hello")

# to run if statements on vector of logicals -> ifelse
# Example: abbreviating species names that are longer than 6 characters long
spp.len <- nchar(spp$species)
len.gt6 <- spp.len > 6 # create a logical vector

# abbreviate spp$species where spp.len > 6 to 3 characters long, AND
# return those that are less than 6 as full length
# this will not work, since spp.len has length greater than 1, only runs for 1st in spp.len
if(spp.len > 6) {
  substr(spp$species, 1, 3) # return characters 1 through 3
} else {
  spp$species
}

# have to use ifelse
# ifelse(spp.len > 6, "true statement" or action, "false statement" or action)
# abbreviate spp$species where spp.len > 6 to 3 characters long, AND
# return those that are less than 6 as full length
ifelse(spp.len > 6, substr(spp$species, 1, 3), spp$species)

# could also use curly brackets to make multiple statements
ifelse(
  spp.len > 6,
  {
    x <- spp$species
    y <- 1
    z <- 3
    substr(x, y, z)
  },
  spp$species # this is the "else" line
)

# ___2. for loop----
# i has a value, it's the product of the last iteration of the loop
rm(i)
for( i in 1:10) {
  print(i)
  if(i > 6) break
}

# let's make the argument from spp.len
rm(x)
for(x in spp) { # each column goes into x
  if("CETACEA" %in% x) break # is "cetacea" in the column
}
x # vector of the values in the order column

rm(x)
for(x in colnames(spp)) {
  if("CETACEA" %in% spp[[x]]) break
}
x

rm(x)
taxa.level <- NA
for(x in colnames(spp)) {
  if("erif" %in% spp[[x]]) {
    taxa.level <- x
    break
  }
}
x
taxa.level

# repeat and while are other options
# repeat: keep doing this code, and stop at the break if it's defined
# while: while the code in the while(here) is true it will continue running, once it's 
  # false, it will stop running

#     3. In the “ctd.csv” data set, what is the mean difference in temperature between 10 meters and the surface?



#     4. In the “ctd.csv” data set, which stations have the lowest and highest mean surface temperature?



#     5. Write a function that returns the following summary statistics for a numerical vector: mean, standard
#   deviation, number of values, number missing, minimum, maximum, and median.



#   6. Add to the function in 5 some code to make sure that the input is valid numerical vector.



#   7. Write a function that uses the function in 6 to summarize the measurements from a data.frame like
#   “ctd.csv”



#   8. Write a function that uses the function in 6 to summarize the measurements from a CTD data.frame
#   by station and depth.



#   9. Write a function that uses the function in 8 to identify outliers (> 3 standard deviations from mean).
#   The function should take as input a data.frame of CTD casts like “ctd.csv”, a depth value of interest,
#   and a measurement of interest. The output should be a three column data.frame that contains the
#   station, sample date, and value of that measurement for every outlier identified.


#   10. Write a function that uses the function in 9 to identify outliers for every depth and measurement.