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

# In order to make GPA a little bit more manageable we are going to place GPA into some
# "buckets" based on the quartile.  This will also give us a way to look for patterns.
# From the summary we see the following for quartiles:
# Q1 = GPA < 8.127
# Q2 = 8.127 < GPA < 8.560
# Q3 = 8.560 < GPA < 9.040
# Q4 = GPA > 9.040.
# We will need to include those boundaries somewhere.  I would recommend including
# them in the lower of the quadrants.
# Small downside.  First we need to impute GPA values.  If we do not then we will
# get an error, since R treats NA like a weird third boolean.

mean(dataset$GPA, na.rm = TRUE) #8.575907 We could round of course.
median(dataset$GPA, na.rm = TRUE) #8.56 Probably an irrelevant difference.
getmode(dataset$GPA) #8.76 So, potentially different enough to be relevant.

# These are all so close that any choice is good.  I am going to go with Median to err
# on the side of lower (as we will do below with our quartiles).
dataset[is.na(dataset$GPA), "GPA"] = median(dataset$GPA, na.rm = TRUE)
# Note that row 97 had NA for TOEFL, so a quick check....
dataset[97, ] #Success!

# Look at our bar graph for GPA.  We can see that 8.76 does look like the 
# most common GPA.  Nice to have a second verification!

dataset$GPA.Quadrant = NA
for (i in 1:nrow(dataset)){
  if(dataset$GPA[i] <= 8.127){
    dataset$GPA.Quadrant[i] = 1
  } else if(dataset$GPA[i] > 8.127 & dataset$GPA[i] <= 8.560){
    dataset$GPA.Quadrant[i] = 2
  } else if(dataset$GPA[i] > 8.560 & dataset$GPA[i] <= 9.040){
    dataset$GPA.Quadrant[i] = 3
  } else {
    dataset$GPA.Quadrant[i] = 4
  }
}

# Quick thing.  Graph the GPA.Quadrant like we did above.  What should the image roughly
# look like?  Why?  The answer and work after the filler below:
# Filler
# Filler
# More Filler
# Filler
# More Filler
# Yet More Filler

v = ggplot(data = dataset, aes(x = GPA.Quadrant)) +
  geom_bar(fill = "#D55E00")

v

# This is good.  Each quartile "quarter" should be 25% of the data.  So the image 
# should be four bars of approximately equal height.  That is what happened.  Good!

# Let us look at the measures of tendency for Statement Score.
mean(dataset$Statement.Score, na.rm = TRUE) #3.37249 We could round of course.
median(dataset$Statement.Score, na.rm = TRUE) #3.5 Probably an irrelevant difference.
getmode(dataset$Statement.Score) #4 So, potentially different enough to be relevant.

# This is actually very interesting.  mean < median < mode is actually the definition
# for left-skew of data!  Interesting that this means it is more unlikely for someone
# to get a lower writing score.  To me this indicates that we may want to use mode here.
# Row 131 is something that should cause us pause.  This row indicates that the 
# applicant has a high toefl, and high gre score.  Should they have 'only' a 4 on writing?
# Realistically, there is not a ton of data here to make as informed a decision as
# we might like.  With more information I have created groups and only used the mean
# or median or mode of those groups, etc.  Here we will just go with mode, though
# we have options for in the future!

dataset[is.na(dataset$Statement.Score), "Statement.Score"] = getmode(dataset$Statement.Score)
dataset[131, ]

filter = !complete.cases(dataset)
missing.data = dataset[filter, ]

# Letter score is another weird situation.  This is something where we should
# probably just go with the most 'middle' option, as the letter is not written
# by the applicant themselves.  It is also tempting to go with 0, though that
# is a little risky, as the letter has more than likely actually been written.

mean(dataset$Letter.Score, na.rm = TRUE) #3.488956 We could round of course.
median(dataset$Letter.Score, na.rm = TRUE) #3.5 Probably an irrelevant difference.
getmode(dataset$Letter.Score) #3 So, potentially different enough to be relevant.

# I think that mode is clearly the one on the outside here.  Median is fine, though
# mean is not a bad choice either.

dataset[is.na(dataset$Letter.Score), "Letter.Score"] = median(dataset$Letter.Score, na.rm = T)
dataset[159, ]
#159 as our checking row.

complete.cases(dataset)

# What a beautiful sight.  A great and glorious row of TRUE.
# A quick check:

filter = !complete.cases(dataset)
missing.data = dataset[filter, ]

# Notice that the missing.data has 0 observations.  We have completed data tidying!

# EXCUSE ALL CAPS BUT NOW IS THE MOST FUN PART.  Building and fitting models
# are by far the coolest/most fun part of machine learning.

# We will start by using some algorithms to predict the likelihood of acceptance.
# Additionally, we will be holding back some of our data to use as a test case.
# There are different trains of thought on how much to hold back, but I
# typically use 80% of the data to train and 20% to test.

# install.packages("caTools")
library(caTools)
set.seed(42)

# A few things to unpack here.  We will use caTools to create our data split.
# I am also setting the seed.  Things in R are not truly random of course.  Setting a
# seed gives us all the same 'random' outcome.  Essentially it will make our work match
# and also makes my work reproducible.  Why 42?  I am a nerd and it is the answer
# to life, the universe, and everything.  This is a "Hitchhiker's guide to the Galaxy"
# reference and does not impact data science at all.  Please forgive my indulgences.

split = sample.split(dataset$Admission.Chance, SplitRatio = 0.80) # Creates a filter
training_set = subset(dataset, split == TRUE) # Filter is true, 80% of data
test_set = subset(dataset, split == FALSE) # Filter is false, so the remaining 20%

# So our data should be split into approximately 400/100 and we see that it is!
# As a side note, I tend to name my models either regressor or classifier
# depending on if they are predicting a value or a classification.  Just a habit!

# General Linear Model
# Honestly even if the linear model does not end up being all that effective we should start here 
# if possible.  I say if possible, because in order for our linear model to really work there
# needs to be some difference between the factors in our data.  What does that even mean!?
# That means that while I can technically encode data (R will do it for us actually), the codes must
# really mean something for linear to be effective. 
# Pretend with me that I have data about some athletes on a team.  They have jersey numbers.  
# Someone who has a jersey number of 34 is not better than someone who has a jersey number of 12.
# So jersey number would not be good for a linear model!

# Almost done talking and time to start coding I promise.  One last thing.  The way we find our line 
# of 'best fit' is by finding distances.  That is... problematic.  Before you read on, what problem
# could happen here?  YES, you are right.  Different measurement systems can cause drastically 
# different distances.  the GRE scores are enormous, and would dominate any distance computations.
# How do we get around that?  Our good friend the z-score.  This is partially why we did those
# graphs.  We needed to see if we could at least hopefully use normalization.  If not... well that is
# harder.  Let us leave that for the us that will show up for project 2!

# To keep things easier for us I am going to clone our data so we can make some weird changes to it.
# Note that this can cause some problems depending on our programming language.  R is fine!

glm.dataset = dataset

# First, we are trying to predict acceptance percentage.  So we do not need the binary we added
# just yet.  We remove it, as well as the GPA quadrant, since we have the GPA itself, and that is
# sufficient for linear.
glm.dataset$Admitted.Y.N = NULL #Notice we drop to 10 variables.
glm.dataset$GPA.Quadrant = NULL #I am removing these one at a time to be clearer.  You can mass remove.
glm.dataset$Identifier = NULL #Identifier is not helping anything.  We have column numbers.
glm_train_set = glm.dataset[split,]
glm_test_set = glm.dataset[!split,]
glm_test_set[1:6] = scale(glm_test_set[1:6]) #We omitted 7,8, they need no scaling.
glm_train_set[1:6] = scale(glm_train_set[1:6])

# Everything we want scaled is now scaled.  Also, each column is scaled separately.  A quick stats lesson:
# When I say scaled, I mean that each element is now transformed into a z-score.  Remember the z-score
# is z = (element - mean)/Standard deviation.  This lets us compare unlike terms to their average.
# If you are curious about z-scores for further discussion I LOVE statistics and will be happy
# to share some of my old lecture notes (assuming I did not burn them when I left academia!)
# OH, and one more big thing.  test and train are scaled separately.  The reason is because of the
# z-score definition.  The z-score requires the mean.  The mean requires all data points in the set.
# If you use the test set, the data that has been set aside and the model is not supposed to see then
# that can impact the mean!

regressor = glm(formula = Admission.Chance ~., family = gaussian, data = glm_train_set)

# So this requires some explanation.  Calling glm() is how we actually make the model.
# There are an absolute ton of options, and in fact typing ?glm will show you a lot of them.
# For our purposes the ones we will care about are formula, family (gaussian is the default,
# so we could have left it off), and data.  
# formula takes the form (Variable we want to predict) ~ Other variables.  We could have listed them
# individually:  Admission.Chance ~ GRE.Score + TOEFL.Score + ... for all the variables.
# However R gives us an option for a shorter notation.  You can use the . to represent all the others.
# Then the data is of course the glm_train_set, the set we designated for training.

# Now is the coolest part.  We try and predict what the values will be in the set of data
# that the model has never seen!  This is THE thing that made me want to do machine learning.

y_preds = predict(regressor, glm_test_set[-8])

# I tend to name my prediction y_preds just because y is our go-to for dependent variables in the US.
# predict is the function we need to call.  predict can take a lot of things, but in our case we
# need to pass predict a model (the one we created earlier, regressor), and some data.
# we need to pass the model the same type of data we trained it with.  Also, we do not want the model
# to see the 'real' answers, so we remove column 8, the actual values.  The result is our prediction.
# Congratulations!  You have now made a real, genuine prediction on a real, genuine data set!
# So, how did we do?  We will do some testing in the Fourth pass, but for now let us do a quick
# visual.  We will stick the predictions onto the dataset they came from and just check.

glm_test_set$y_preds = y_preds #Pasting the predictions next to the actual percentages.
head(glm_test_set, 10) #head(data, n) checks the top n elements of whatever data is.
head(glm_test_set[,8:9], 10) #all we care about right now is the last two columns (8 and 9)
tail(glm_test_set[,8:9], 10) #tail of course will look at the bottom n rows.

# Quite honestly a quick visual inspection tells me that our model looks pretty good!
# Now, I need something a bit more rigorous than 'looks good', and we will talk about that next!

# One of the measurements we can employ is checking "r squared".  
# r squared is approximately the goodness of fit of our predictions to the actual value.
# r squared ranges from 0 (bad fit) to 1 (perfect fit).
# An approximation for r squared is given by 1 - residual deviance/ null deviance.

r_squared = 1 - (regressor$deviance/regressor$null.deviance) #0.8161

# That's not a bad fit at all!  A natural question we may have is "Hey... what if we just want to
# know yes or no?"  I love that question.  We can come up with a quick answer here and then that
# question will lend itself to another approach for linear models.
# Remember that binary that we made above?  The one we removed because we did not need it for our 
# model?  Now we are going to put it back.  We are also going to create a comparable column
# but from the predictions.

glm_test_set$Admitted.Y.N = ifelse(glm_test_set$Admission.Chance < 0.5, 0, 1) #Yes/No for actual
glm_test_set$Predicted.Y.N = ifelse(glm_test_set$y_preds < 0.5, 0, 1) #Yes/No for predictions

# Now that we have two columns, both binary, both predicting the same thing, we can create a 
# confusion matrix.  The confusion matrix tells us all sorts of interesting information!

library(caret) # The package that houses our confusion matrix
cm = confusionMatrix(data = glm_test_set$Predicted.Y.N, 
                     reference = glm_test_set$Admitted.Y.N, positive = "1")
cm
# We see that the confusion matrix gives us all sorts of interesting information.
# Probably the most interesting thing is the accuracy listed and the chart that shows
# us predictions versus actual values.  Knowing kappa and p-value are good, but a bit 
# much for now.  Our model has a high rate of accuracy, probably uncomfortably high.
# There are a few things that could cause that that we will need to consider later.
# For now, this model is promising, but a bit concerning.

# Our first linear model was a regression model where we tried to predict a percentage.
# Now we take a linear model where we try and predict yes/no directly.  Linear models,
# by their nature do not tend to binary outcomes very well.  But, they will return functional
# results, as long as they are viewed with some skepticism!  We reset our data and being anew.

glm_binary_dataset = dataset
# Just like before we will strip the columns out that we do not need.
glm_binary_dataset$Identifier = NULL
glm_binary_dataset$Admission.Chance = NULL
glm_binary_dataset$GPA.Quadrant = NULL

# We still had split setup from before.  If we did not (for some reason), and we used
# set.seed(42) we would arrive at the same split as before.  Yay consistency!

glm_binary_train_set = glm_binary_dataset[split,]
glm_binary_test_set = glm_binary_dataset[!split,]

# And of course scaling.  We cannot forget scaling!

glm_binary_test_set[1:6] = scale(glm_binary_test_set[1:6]) #We omitted 7,8, they need no scaling.
glm_binary_train_set[1:6] = scale(glm_binary_train_set[1:6])

# The model we are creating is similar.  Two slightly different things though.
# First, the data is different of course!  Second, the family.  We are now predicting
# yes/no, so we use binomial.  I lied.  Three things.  Our formula is now Admitted.Y.N as well.

classifier = glm(formula = Admitted.Y.N ~., family = binomial, 
                data = glm_binary_train_set)

# Now that our model is done, we make our predictions!  This will be troublesome.
# Oh, uh spoiler alert, this will be troublesome.

y_preds_binary = predict(classifier, glm_binary_test_set[-8]) #Remember to remove col(8)

# So, a quick look here:

head(y_preds_binary)

# That is... maybe surprising.  It should not be.  While the line does the best it can, 
# lines are still... well linear.  The line does not know to stop at 0,1.  So we apply 
# the ifelse function we have been using all along.

glm_binary_test_set$Predicted.Y.N = ifelse(y_preds_binary < 0.5, 0, 1)

# So now we have a similar dataframe to what we had with glm_test set.  Time to check our 
# accuracy using a confusion matrix again.

cm2 = confusionMatrix(glm_binary_test_set$Predicted.Y.N, 
                      glm_binary_test_set$Admitted.Y.N, positive = "1")
cm2

# So, slightly worse.  We lose some accuracy points, but not many.  Why do we lose them?
# Well, we have something of an awkward situation here.  We are trying to predict a strict
# yes or no outcome using a line that is intended to take on a range of values.  With that
# in mind it should come as no surprise that the line was better at predicting a range of
# values (the probability) than just one of two outcomes.  There are other models that
# are better suited for such predictions!

# A reasonable question as well:  Can we tune this model?  What does tune even mean!?
# We certainly can, but first we want to check some other models.  Finding the best model
# for the job makes more sense than tuning and hoping.  At the very least we can extract
# some value out of what we have here.

summary(regressor)

# We see that for regressor the variables of significance are:
# VERY:  GPA, Research.History
# Somewhat:  GRE.Score, TOEFL.Score, Letter.Score

summary(classifier)

# Very neat!  Classifer on the other hand cares about:
# VERY:  GPA (Every students nightmare, only GPA matters!)
# Slight:  TOEFL.Score, Letter.Score
# Better than nothing:  Statement Score.
# NOTE we sometimes care about the non-three star factors.  We can touch on that later.
# Up next, the most beautiful model.  Genuinely one of my favorite.
# OK I lied.  Something really quick that may be of some help.

# Here the code is quick and not very complicated.  As such if we need to run the code to
# fix mistakes then there is not a long delay.  I can promise you that this will not
# always be the case.  In fact, recently training the model could take fourty-five minutes or
# longer, and using predict could in some cases take comparable time.  As such, once we have
# data we wish to keep we need to save it.  The way we can do that is writing it to your
# memory.  In this case the laptop/computer itself.  Since I am happy with how we transformed
# the data I wish to save the dataset for later use.

save.path = "C:/Users/bmcdole/Desktop/R Practice/Grad School Data/Admission_Predict_complete_cases.csv"
write.csv(dataset, save.path) 

# For write.csv() we pass the data we want to save, as we have named it, and then the 
# path we will use to find the data later.  Feel free to copy and paste a previous path,
# just remember to change the name.  So we are saving this data as:
# "Admission_Predict_complete_cases" in the Grad School Data subfolder of the R Practice folder.
