# First, clear the workspace
rm(list = ls())

# Packages----
if(!require(ggplot2)) {
   install.packages("ggplot2")
  library(ggplot2)}

# tidyverse----
# all the packages within work nicely together, so just open this to load them all
library(tidyverse)

# ggplot----
# grammar graphics, philosophies on visualization, oriented for data frames

# __EX: iris data----
iris <- datasets::iris
# Create a graph of sepal length v sepal width by mapping variables to axes
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  # map data as points for a scatterplot
  # use aes() to map back to the dataframe
  geom_point(aes(color = Species)) +
  geom_line(color = "green")

# using fill is different from color, and depends on what shape you use
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(fill = Species), shape = 21) 

# with a lot of overlapping data, use shape 21 and white to differentiate between points
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(fill = Species), shape = 21, color = "white")

# __geoms link----
# all the ggplots geom's: https://ggplot2.tidyverse.org/reference/ 

# __Bar plots----
mean.sepal <- tapply(iris$Petal.Length, iris$Species, mean)
mean.sepal # currently not a dataframe, cannot plot as is

mean.sepal <- data.frame(Sepal.Length = mean.sepal, Species = names(mean.sepal))
mean.sepal

ggplot(mean.sepal, aes(x = Species, y = Sepal.Length)) +
  geom_bar()
# Error: stat_count() must not be used with a y aesthetic.
# Geom_bar is a visual representation of the nunmber of things in each species 
# (similar to table)

# Specifying how you want to plot
# stat count = frequencies
# value you want to summarize and plot = identity

ggplot(mean.sepal, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "identity") # needs a y value now (our mean sepal.length)

# abline, hline, and vline *adding lines)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(fill = Species), shape = 21, color = "white") +
  geom_vline(xintercept = 7, linetype = "dashed", color = "green", size = 3)

# geom_segment
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(fill = Species), shape = 21, color = "white") +
  geom_segment(x = 5.5, y = 2.3, xend = 6.2, yend = 4.1, color = "salmon")

# create a theme that you can apply across plots
my.theme <- theme(
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 15),
  axis.text = element_text(color = "blue", size = 20),
  panel.background = element_rect(fill = "white"),
  panel.grid = element_blank()
)
 print(p + my.theme)

 # __Faceting----
 ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
   geom_point() +
   # make a panel for each species (n=3), plot in two rows, scales set separately by plot
   facet_wrap(~ Species, nrow = 2, scales = "free_y")
 
iris.2 <- iris
iris.2$sex <- sample(c("Male", "Female"), nrow(iris.2), T)

ggplot(iris.2, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  facet_grid( sex ~ Species)


# change x and y lables for axes
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(fill = Species), shape = 21) +
  labs(x = "hello", y = "byebye")
  
# magrittr----

# using steps to run intermediate things to get your result
n <- 1000
x <- runif(n, 100, 500)
x.range <- range(x)

# could also do this by nesting
x.range <- range(runif(n, 100, 500))
x.range

# could also this by piping
library(magrittr) # gives you access to the pipe %>% (ctrl + shift + m)
# takes the thing on the left side of the pipe, 
# and passes as first argument on thing on right
x.range <- 1000 %>% 
  runif(., 100, 500) %>% 
  range()

# dplyr----
library(dplyr)
# a host of tools to manipulate dataframes
# in R: Help > cheatsheets > data transformation with dplyr
sw <- starwars[,1:10]

males <- sw[sw$gender == "male", ]
males <- filter(sw, gender == "male")
# __filter----
big.males <- filter(sw, gender == "male" & mass > 110)

# now to pipe
males <- sw %>% 
  filter(., gender == "male") %>% 
  filter(., mass > 110)

# add a column for BMI (old way)
sw$bmi <- sw$mass/((sw$height/100)^2)

# create a new column (semi old way)
new.sw <- mutate(sw, bmi = mass/(height/100) ^ 2)

# __mutate----
# create new column using mutate and pipes
new.sw <- sw %>%
  mutate(
    bmi = mass / ((height / 100) ^ 2)
  )

str(sw) # shows classes, which are tbl_df, tbl, and data.frame
# dplyr creates a tibble, which is faster for looking things up and manipulating dfs
class(sw)

male.bmi <- sw %>% 
  filter(gender == "male") %>% 
  mutate(
    bmi = mass / ((height / 100) ^ 2)) %>% 
  select(name, height, mass, birth_year, bmi)
male.bmi

# do above for male.bmi, but select only columns that contain "color"
male.bmi <- sw %>% 
  filter(gender == "male") %>% 
  mutate(
    bmi = mass / ((height / 100) ^ 2)) %>% 
  select(ends_with("_color"))
male.bmi

# __arrange----
# sort by ascenting homeworld , then descending bmi
male.bmi <- sw %>% 
  filter(gender == "male") %>% 
  mutate(
    bmi = mass / ((height / 100) ^ 2)) %>% 
  select(name, height, mass, birth_year,homeworld, bmi) %>% 
  arrange(homeworld, desc(bmi))
male.bmi

# __grouping----
# then run functions by group
# typically, only use group by when going to use summarize after
gender.ht <- sw %>% 
  group_by(gender) %>% 
  # summarize only returns the things within in tibble, i.e. mean.ht and mean.mass
  summarize(
    mean.ht = mean(height, na.rm = T),
    mean.mass = mean(mass, na.rm = T))

# __keep.rownames----
# murder per population rate
# USArrests has row names, but dplyr doesn't keep them by default
df <- USArrests %>% 
  mutate(
    murd.rate = Murder / UrbanPop)
df

# keep row names, run rownames_to_column at beginning and end of pipe
df <- USArrests %>% 
  rownames_to_column("state") %>% 
  mutate(
    murd.rate = Murder / UrbanPop) %>% 
  column_to_rownames("state")
df

# tidyr----
# spreading and gathering the data together, helpful for ggplot

# you're not special----
# when you have a problem, search it, likely someone else has had same/similar problem