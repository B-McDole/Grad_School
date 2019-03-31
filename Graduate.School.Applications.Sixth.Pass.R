# Fifth Pass.  New models and beyond!

save.path = "C:/Users/bmcdole/Desktop/R Practice/Grad School Data/Admission_Predict_complete_cases.csv"

# Saving data has a bad habit of sometimes adding on a column named "X" that is the rownames.
# An easy enough fix is to just tell R that those are the row names.  Notice the difference here:

dataset1 = read.csv(save.path)
View(dataset1)

dataset = read.csv(save.path, row.names = "X")
View(dataset)

# The second is more what we had in mind.  Feel free to comment out the first dataset if
# you want, though it will not impact us further!  We can remove it from memory so that
# it does not cause us problems.
rm(dataset1) # Put the stuff to remove in the parenthesis.  Gone!

# We really don't need admission chance if we are going to do random forest.  Additionally,
# we really do not need identifier either.  So we cut those.

dataset = dataset[-c(1,9)]

# Before we cut and change anything else we should discuss the model.  The next model is 
# Random Forest.  Random Forest is creatively named and also particularly effective with 
# minimal tuning needs.  Is random forest perfect?  No, of course not!  We are using numbers
# and not some crystal ball!  But random forest is in general very effective.
# So, what the heck is random forest?  A decision tree is essentially what we would like to create
# for classification.  We want to follow a path down GPA, GRE Score, TOEFL Score, etc.  Once we 
# do that we will have an ending of accepted or not (1 or 0).  Well, if we repeat that many times
# then we have a group of trees... a forest!  That is essentially what random forest does.  Create
# a forest and return the mean, or mode outcome depending on regression or classification.
# Random Forest really excels when it comes to yes/no outputs, especially when compared to other
# models.  Let us dive in!

# We do not need GPA any longer.  We would prefer to have 4 splits over approximately 500 splits.

library(plyr)

# plyr gives us a lovely count function.  We will use it here quickly.

count(dataset$GPA)

# We see 183 different GPA scores.  I strongly suspect that we do not need that many.  Compare
# that to:

count(dataset$GPA.Quadrant)

# Well, four different quadrants (by name) should not be a surprise.  As an interesting fact
# plyr will also group for you!  
# Pause really quickly.  Before you run the code below, what do you think count will do?

count(dataset, c("GPA.Quadrant", "Admitted.Y.N"))

# Neat!  Count breaks down dataset using only the columns we passed in the vector.  
# A thing for us to note, if you are in quadrant 4 you essentially do not get declined.
# Are there other factors?  Surely, but Q4 is a strong indicator of success.

# Stripping out the GPA

dataset$GPA = NULL

# University rating, letter score, and statement score are all a small number of branches.
# No real need to transform them.  TOEFL and GRE, just like before are a little problematic.
# We will transform them in a comparable way to how we did GPA.

summary(dataset$GRE.Score) #Splits are 308.0, 317.0, 324.2 

# Let us do a different method.  In second.pass we used an if loop to do our classification.
# This type of loop can crush run time if data is very large.  Again we do not have that here
# but that does not mean you will always be OK.  Let me show you a slightly different way
# that is considerably faster.  For example, when doing this process on a dataset with 
# 2.6 million entries recently the method ran in approximately 1.5 minutes.  I'll take that!

dataset$GRE.Quadrant = NA #Reserve space in memory.
Q1.filter = which(dataset$GRE.Score < 308)

# Notice that this gives us 115 values.  What this tells me is that 115 of the datapoints fall
# into Q1 for GRE scores.  Great!  So now we want to set those spaces equal to "Q1".

dataset[Q1.filter, ] #Before you run this, what should the result be?  Yes!  All of the columns
# with the rows from Q1.filter.  So the first row you see should be row 9!

dataset[Q1.filter, "GRE.Quadrant"] = "Q1" #If you view the dataset you see that GRE.Quadrant
# is now beginning to populate.  Let's do the rest of the quadrants.

Q2.filter = which(dataset$GRE.Score >= 308 & dataset$GRE.Score < 317)
dataset[Q2.filter, "GRE.Quadrant"] = "Q2"
Q3.filter = which(dataset$GRE.Score >= 317 & dataset$GRE.Score < 324.2)
dataset[Q3.filter, "GRE.Quadrant"] = "Q3"
Q4.filter = which(dataset$GRE.Score >= 324.2)
dataset[Q4.filter, "GRE.Quadrant"] = "Q4"

# Quick check for sanity!
count(dataset, "GRE.Quadrant") #Approximately equal
count(dataset, c("GRE.Quadrant", "Admitted.Y.N")) #Just for fun.  Be curious about your data!

# We no longer need GRE Scores now!
dataset$GRE.Score = NULL

# We now quickly do the same with TOEFL score

summary(dataset$TOEFL.Score) #103.0, 107.0, 112.0

dataset$TOEFL.Quadrant = NA #Reserve space in memory.
Q1.filter = which(dataset$TOEFL.Score < 103)
dataset[Q1.filter, "TOEFL.Quadrant"] = "Q1"
Q2.filter = which(dataset$TOEFL.Score >= 103 & dataset$TOEFL.Score < 107)
dataset[Q2.filter, "TOEFL.Quadrant"] = "Q2"
Q3.filter = which(dataset$TOEFL.Score >= 107 & dataset$TOEFL.Score < 112)
dataset[Q3.filter, "TOEFL.Quadrant"] = "Q3"
Q4.filter = which(dataset$TOEFL.Score >= 112)
dataset[Q4.filter, "TOEFL.Quadrant"] = "Q4"

count(dataset, "TOEFL.Quadrant") 

# Perfectly balanced, as all things should be.  Yes that is an Avengers quote.
# I am excited for Endgame and have no shame.  

dataset$TOEFL.Score = NULL
dataset$GRE.Quadrant = as.factor(dataset$GRE.Quadrant)
dataset$TOEFL.Quadrant = as.factor(dataset$TOEFL.Quadrant)
# So now we make the model!  YES!!!

library(randomForest)
library(caret)
library(caTools)

set.seed(42)
split = sample.split(dataset$Admitted.Y.N, SplitRatio = 0.8)
training_set = dataset[split, ]
test_set = dataset[!split, ]

classifier = randomForest(formula = as.factor(Admitted.Y.N) ~., 
                          data = training_set, ntree = 500)

# Perfect, we have a model!  Now we predict, in the same way as before.

y_preds = predict(classifier, newdata = test_set[-5])

cm = confusionMatrix(y_preds, test_set[,5], positive = "1")

# We see, by our confusion matrix again, that randomForest performed very well right away.
# There is a thing we need to be concerned with.  With linear models you are making the
# best fit line.  There is no ambiguity.  We 'trained' the model, but in reality the model
# was not learning anything.  Calculations (distances) were being calculated, and then the
# lowest total distances formulated, which resulted in our line.  That is it.  Here the model
# truly is 'learning' what happens with each tree.  That can cause some problems.  Think of
# the model like a great student.  A better student than I was!  If the model gets exactly
# the test questions then it can memorize the answers.  The model can truly learn EVERYTHING.
# Then when the model sees something it does not know or has not seen before problems can arise.

# Here is an example.  Let us say that we are trying to predict housing prices.  If we included
# door color in our model then any weird pattern that happens could skew our model.  We want to
# avoid that problem (commonly called overfitting).  The way we do so is with cross
# validation.  Cross validation will split our data into pieces (just like we did before),
# and will train on all except one (just like we did before), and then test on the one that
# is withheld (just like we did before).  Then the process will repeat, but holding out a 
# new piece this time.  Then the results are averaged and our model is made!  So, we lower
# the likelihood of having some weird data being held out and messing with our model.  Let
# us now perform some cross validation!

# caret as a package very conveniently has cross validation built in.  We will take 
# advantage of this.

control = trainControl(method = "cv", number =5, repeats = 3) 
#we will use
# this control to better refine the model.  This will show up in the train() that we
# will call.  There are other methods to refine, we will use cv for cross-validation.
# Additionally, number is how many splits.  5 splits gives us our 80/20 ratio we 
# used before.  Repeats are how many times we repeat this entire process.  3 is fine.

classifier = train(as.factor(Admitted.Y.N) ~., data = training_set, 
                   trControl = control, method = "rf")

# method in the train function is where we specify our model type.  We keep it
# simple with random forest 'rf'

y_preds = predict(classifier, newdata = test_set[-5])
cm = confusionMatrix(y_preds, test_set[,5], positive = "1")

# Our percentage went up!  Great!  Except is it?  I feel like maybe not.  Our model
# does not seem to predict that people will be declined.  Such a high acceptance rate
# causes some problems for us.  Cross validation here seemed to learn that the best
# thing to do is "predict accepted".  Tricky machines!  Larger data sets pose problems
# but also make cross validation better.  We will see that for sure in the next 
# project.

# Now we start our next, another fairly popular choice, "Naive Bayes".
# Some explanation first:  Bayes theorem is the probability engine that drives
# this entire idea.  At the risk of GREATLY simplifying things, Bayes Theorem says that 
# if you want the probability of an event A given that some event B has already happened 
# you need to consider the likelihood of B impacting A, the likelihood of B happening at
# all, and then of course the likelihood of both.  There is more to it, and infinite 
# things that COULD happen.  For now we will worry about what makes our model Naive.
# Naive Bayes assumes that everything is largely independent.  As a side note, 
# Naive Bayes is very popular for text categorization, but that is not what we want now!

# Before we start I just want to clear out our previous classifier.  We can leave our 
# split in our data though!

rm(classifier, cm, control)

library(e1071)
classifier = naiveBayes(x = training_set[-5],
                        y = as.factor(training_set$Admitted.Y.N))

y_preds = predict(classifier, newdata = test_set[-5])

# See how straightforward!  Also, notice that realistically, after doing all of the 
# models before you probably predicted (predict!) what was going to happen right?
# Apart from needing the library, we did not do a whole lot new.  We created a classifier
# we input x and in this case y values and we called predict.  What happens next?
# YES, we make a confusion matrix!

cm = confusionMatrix(test_set[,5], y_preds, positive = "1")

#84% accuracy!  Is it as good as random forest?  In this case no.  But, 84% is a result
# that we would be THRILLED with.  High enough to be very accurate, but not so high
# that we should worry about overfitting.  What a great result!

# This brings us to what will be our last model.  Realistically we could go on forever
# and never really run out of models.  The last model is a fairly accurate model,
# but also one that requires a fair amount of explanation.  It is SVM otherwise known
# as Support Vector Machine.  We will dive into Support Vector Machines in our seventh
# and final pass with this set of data.



