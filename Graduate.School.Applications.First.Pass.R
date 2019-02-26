# Interesting small data set on grad school admissions

# First thing to note, the path needs quotes.
# Second thing to note, there are "double" backslashes.  
# That is something of a trap, it is because backslash is an escape character.
# The second backslash is there to tell R that the first backslash is not an escape and to
# keep going.
# You can use a single forward slash instead.

path = "C:\\Users\\bmcdole\\Desktop\\R Practice\\Grad School Data\\Admission_Predict_Ver1.1.holes.csv"
path2 = "C:/Users/bmcdole/Desktop/R Practice/Grad School Data/Admission_Predict_Ver1.1.holes.csv"

# Both of those methods work fine.
# On a Mac you do not need to do this.  The path is saved in a naturally readable way.
# There are alternative methods you can use (setting working directory for example).
# In the interest of time, this will work!
# Also of note, you do not need to have the path separate, I just do that as a 
# good practices habit.

dataset = read.csv(path)
dataset = read.csv(path2)

# Note that if you ever want some information about a function R is very user friendly.
# ?read.csv will bring up help on the function.  In fact ?function.name will work a large
# portion of the time.

# Clicking on the dataset will give us some idea of what we are working with.  
# Alternatively you can use the str() function.
# I know str typically means string, but not here!  str will give us a general idea of 
# the data structure

str(dataset)

# A few quick things of note:
# First, all numbers.  This is beautiful and will make our lives easier.
# Second, some descriptions:
# 1. GRE Scores ( out of 340 ).  GRE = Graduate Record Exam.  A standardized test.
# 2. TOEFL Scores ( out of 120 ).  TOEFL is 'Test of English as a Foreign Language'.
# This is a standardized exam that international students take.
# 3. University Rating ( out of 5 ).  Presumably this was consistently performed.
# 4. Statement of Purpose and Letter of Recommendation Strength ( out of 5 ) 
# 5. Undergraduate GPA ( out of 10 ).  Pretty explanatory! 
# 6. Research Experience ( either 0 or 1 ).  This is a binary.  Did they research?
# 7. Chance of Admit ( ranging from 0 to 1 ).  This is our target.  Our predictor if you will.
# 8. After getting the columns to have names we like we will add a column
# with a simple yes/no for being accepted.  This will help with many models.

colnames(dataset)

# We need to rename these columns for our own sanity!

colnames(dataset) = c("Identifier", "GRE.Score", "TOEFL.Score", "University.Rating",
                      "Statement.Score", "Letter.Score", "GPA", "Research.History", 
                      "Admission.Chance")
colnames(dataset)

# Adding the binary column.

dataset$Admitted.Y.N = ifelse(dataset$Admission.Chance < 0.5, 0, 1)

# ifelse is something I use constantly.  It is a quick function that lets you
# create an if-then type scenario.  ifelse(condition, if true do this, if not do this)
# Note that we also create a column by just using the $ notation and adding the 
# column with whatever name we would like.

# One of the next things I like to do is see how much data is missing.

complete.cases(dataset)

# That is a mess.  complete.cases() is beautiful, but returns this giant boolean.
# An easier way is to subset our dataset using complete.cases() as a filter.
# Since we are only really interested in knowing where we have holes we actually
# want the negation of complete.cases().  Just a reminder that negation in R is !.

filter = !complete.cases(dataset)
missing.data = dataset[filter, ]

# We seem to be missing some data.  I cut it out on purpose.
# A giant part of what I do is preprocessing or 'tidying'.  Garbage in-garbage out is a
# VERY common phrase and a mantra we will live by!

# We need to deal with the missing data.  Here are some lovely options:
# 1.  Remove the rows with the missing data.  While this is not always bad there are
# a few problems here.  There is not a large number of cases to begin with.
# Losing even a small number could be problematic.  Also, each row is only missing
# an element or two.  This is something we can overcome!
# 
# 2.  Add a flag noting that the data was missing.  This is not something we will
# do here, but the fact that the data is missing can matter.  I have used that option
# at work and had the results be at least slightly relevant.  We prefer to have a value
# here so we can do linear modeling potentially.  And again, there are not too many gaps
# to fix.
# 
# 3.  Add a value in.  This is typically called "imputing" a value.  We have 3 options
# commonly used, mean, median, mode.  I would strongly advise against mode.  Unless you know
# that your data is a beautiful bell curve (normally distributed) mode can be a problem.
# Mean can be skewed by outliers, which we have not checked for.  In general median tends
# to be the least prone to skew and what I would suggest.  There are more details we can
# worry about in a bit.
# We can compute and compare mean, median and mode for the columns missing.

mean(dataset$GRE.Score)

# NA!  That is not good.  And it is also because we have missing values and thus "NA" is 
# one of our data points.  We need to tell our function how to deal with NA.  So we use
# the parameter na.rm = TRUE, which will tell the mean to remove NA when calculating.

mean(dataset$GRE.Score, na.rm = TRUE) #316.4306 We could round of course.
median(dataset$GRE.Score, na.rm = TRUE) #317 Probably an irrelevant difference.
mode(dataset$GRE.Score)

# That's a trap.  How weird.  There's no function for mode native to R.  So
# I will build one really quickly.

getmode <- function(v) {
  uniqv <- unique(v) #Unique does more or less what it sounds like.  Strips out duplicates.
  uniqv[which.max(tabulate(match(v, uniqv)))] #Filters unique by highest match count.
} 

# Quick note, you do not have to 'return' a value in R.  
# This is different and a little odd if you are accustomed to object-oriented languages.

getmode(dataset$GRE.Score) #312 So, potentially different enough to be relevant.

# For now I am just going to impute missing values as being the median.  We can talk
# about more refined methods in a bit, but this works as a start.

# Notice we filter dataset by the rows that are "NA" for GRE.Score.  Then we specify
# the column "GRE.Score" or else we run into issues of where the value is going to be 
# placed!

dataset[is.na(dataset$GRE.Score), "GRE.Score"] = median(dataset$GRE.Score, na.rm = TRUE)

# Notice that from the missing.data dataframe we have row 216 as missing GRE.
# If we check it now we see that the value is replaced with 317, the median!

dataset[216, ] # Progress!

filter = !complete.cases(dataset)
missing.data = dataset[filter, ]

# Notice that if we re-run our filter we have less observations now because the data
# that we were missing before no longer shows up as being incomplete.
# We will run the same tests we did last time, just with the TOEFL now.

mean(dataset$TOEFL.Score, na.rm = TRUE) #107.2278 We could round of course.
median(dataset$TOEFL.Score, na.rm = TRUE) #107 Probably an irrelevant difference.
getmode(dataset$TOEFL.Score) #110 So, potentially different enough to be relevant.

# This time we will use the mean, if only because it lets me introduce a new
# function, which is round.  Note we just want to round to an integer.
# If we wanted to round to decimals we would include how many.  Example:
# round(3.45754, 2) = 3.46, but round(3.45754, 4) = 3.4575

dataset[is.na(dataset$TOEFL.Score), "TOEFL.Score"] = round(mean(dataset$TOEFL.Score, na.rm = TRUE))

# Note that row 170 had NA for TOEFL, so a quick check....
dataset[170, ] #Success!

# Picking up the missing data again.

filter = !complete.cases(dataset)
missing.data = dataset[filter, ]

# Statement score is a bit problematic.  Writing is intensely personal and there is
# quite a bit more variance in how someone crafts words versus how they do on
# a standardized test.  This requires a bit more research on our part.

summary(dataset)

# Summary is comparable to str() except Summary will also provide a quick
# statistical breakdown.  An interesting benefit of this is finding a quick
# percentage of how many datapoints are doing research 
# (the mean will give us that percent)
# A few quick things:
# First, you can click on the packages tab and 'Install' and just type in
# ggplot2 and go that route.  If you do not wish to do so then the install.packages()
# function will work just fine.  Be sure to put the package name in quotes.
# Once you have installed the package then you need to actually load it.
# The library() function works great for that, though you can also simply click
# on the box in the packages tab.  Install is commented out for me because
# I already have ggplot2 installed of course!

# install.packages("ggplot2")

library(ggplot2)

# There are a fantastic number of color options and color palettes available.
# Funny enough you can even use a palette inspired by Wes Anderson (the film maker)
# I chose the palettes below because they are specifically designed to be color-blind 
# friendly.  The top palette utilizes gray and the one we are going to use features black.
# cbgPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p = ggplot(data = dataset, aes(x = GRE.Score)) +
  geom_bar(fill = "#000000")

p

q = ggplot(data = dataset, aes(x = TOEFL.Score)) +
  geom_bar(fill = "#E69F00")

q

r = ggplot(data = dataset, aes(x = University.Rating)) +
  geom_bar(fill = "#56B4E9")

r

s = ggplot(data = dataset, aes(x = Statement.Score)) +
  geom_bar(fill = "#E69F00")

s

t = ggplot(data = dataset, aes(x = Letter.Score)) +
  geom_bar(fill = "#009E73")

t

u = ggplot(data = dataset, aes(x = GPA)) +
  geom_bar(fill = "#0072B2")

u

# ggplot is enormous and far too detailed to go over in particularly large
# detail.  BUT, the plots above show that we are largely good in our assumptions of 
# the data being uniform.  GPA is fairly messy and we may need to create some buckets.
# Fortunately, summary gives us quartiles!



