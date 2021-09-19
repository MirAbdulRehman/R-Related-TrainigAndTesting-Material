
# Load Raw Data
train <- read.csv('train.csv', header = TRUE)
test <- read.csv('test.csv', header = TRUE)


# Add a 'Survived' Variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep('None',nrow(test)),test[,])


# Combine data Sets
data.combined <- rbind(train, test.survived)


# Checking data types in 'data.combine' table
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# Looking at Gross survival rates
table(data.combined$Survived)


# Distributing across classes
table(data.combined$Pclass)


# Loading ggplot2 package for visualization.
library(ggplot2)


# Hypothesis - Rich people survive at a higher rate
train$Pclass <- as.factor(train$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  stat_count(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Examine first few names in the training data set.
head(as.character(train$Name))


# How many unique name are there across train and test data sets.
length(unique(as.character(data.combined$Name)))

# Looking at duplicates in the data set and storing them in a variable
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])


# Looking at the record of the duplicate names in the data set.
data.combined[which(data.combined$Name %in% dup.names),]


# What is up with the "Miss." and "Mr." thing?
library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

# Hypothesis - Name titles correlating with age.
mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

# Checking out male to see if the pattern continues.
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


# Expand upon the relationship between "Survival" and "Pclass" adding the new "Title" variable
# to the data set and then explore a potential 3-Dimensional relationship

# Create a utility function to help with title extraction.
extractTitle <- function(name) {
  name <- as.factor(name)
  
  if (length(grep("Miss.",name)) > 0){
    return("Miss.")
  } else if (length(grep("Master.",name)) > 0){
    return('Master.')
  }else if (length(grep("Mrs.",name)) > 0){
    return("Mrs.")
  }else if (length(grep("Mr.",name)) > 0){
    return("Mr.")
  }else {
    return("Other")
  }
}

titles <- NULL

for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}

data.combined$Title <- as.factor(titles)


# Since we only have survived labels from our training set,
# only use the first 891 rows.

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived))+
  stat_count(width = 0.5)+ # use to count the cases at each x position.
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")


# what is the distribution of females and males across train and test?
table(data.combined$Sex)


# Visualizing the 3-way relationship between Sex, Pclass and Surivuved,
# compare to analyse
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived))+ 
  stat_count(width = 0.5)+  # stat_count counts the number of cases at each x position
  facet_wrap(~Pclass)+
  ggtitle('Survivability Basied on Gender and Class')+
  xlab('Gender')+ 
  ylab('Total Count')+ 
  labs(fill = 'Survived')


# Age and Sex seems pretty important as derived from Analysis of title, lets take
# a look at the distributions of age over the entire data set.

summary(data.combined$Age)
summary(data.combined[1:891,'Age']) # Summary of train data set for Age.

# Just to be thorough, take a look at survival rates broken out by sex, survived
# and age.

ggplot(data.combined[1:891,], aes(x = Age, fill = Survived))+ 
  facet_wrap(~Sex + Pclass)+ 
  geom_histogram(binwidth = 10)+ # Using geom_histogram to show total number of people
  xlab('Age')+ 
  ylab('Total Count')+ 
  labs(fill = 'Survived')
data.combined$Title <- as.character(data.combined$Title)

# Validates that the 'Master.' is a good proxy for Male Children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)


# We know that 'Miss.' is a lot more complicated, Lets examine further
misses <- data.combined[which(data.combined$Title == 'Miss.'),]
summary(misses$Age)


ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 5)+ # geom_histogram is use to show the distribution across levels of categorical variable.
  ggtitle("Age of 'Miss.' by 'Pclass'")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill = 'Survived')


# Appears that female children have different survival rate,
# could be a candidate for feature engineering later.
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


# Moving on to SibSp variable, summarize the variable.
summary(data.combined$SibSp)


# Can we treat SibSp as factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We believe title is predictive, Visualize survival rates by sibsp, pclass and
# title.

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived))+
  stat_count(width = 1)+ # Use stat_count to count number of cases at each x point.
  facet_wrap(~Pclass + Title)+
  ggtitle('Pclass, Title')+
  xlab('SibSp')+
  ylab('Total Count')+
  ylim(0,300)+
  labs(fill = "Survived")


# Treat the Parch variable as a factor and Visualize
data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived))+
  stat_count(width = 1)+
  facet_wrap(~Pclass + Title)+
  ggtitle('Pclass , Title')+
  xlab('Parch')+
  ylab('Total Count')+
  labs('Survived')


# Let's try feature engineering. what about creating a family size feature?
temp.Sibsp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)

data.combined$Family.size <- as.factor(temp.Sibsp + temp.Parch + 1)


# Visualize the data to see if it is predictive.

ggplot(data.combined[1:891,], aes(x = Family.size, fill = Survived))+
  stat_count(width = 1)+  # Count the total cases in each x position
  facet_wrap(~Pclass + Title)+
  ggtitle('Pclass, Title')+
  xlab('Family Size')+
  ylab('Total Count')+
  ylim(0,300)+  # Setting the limit of Y-axis to 300.
  labs('Survived')


# Take a look at Ticket variable
str(data.combined$Ticket)

data.combined$Sex <- as.factor(data.combined$Sex)

# Displaying the first 20.
data.combined$Ticket[1:20]


# There's no immediately apparent structure in the data, let's see if we can find some.
# well start taking a look at just the first character for each.
ticket.first.char <- ifelse(data.combined$Ticket == ""," ", substr(data.combined$Ticket,1,1))
unique(ticket.first.char)


# We can make a factor for analysis purposes and visualization.
data.combined$Ticket.First.char <- as.factor(ticket.first.char)


# First a high level plot of data
ggplot(data.combined[1:891,], aes(x = Ticket.First.char, fill = Survived))+
  geom_bar()+
  ggtitle("Survivability by Ticket First Character")+
  xlab('Ticket.First.Char')+
  ylab('Total Count')+
  ylim(0,350)+
  labs(fill = "Survived")

# Tickets seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = Ticket.First.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle('Pclass')+
  xlab('Ticket.First.Char')+
  ylab('Total Count')+
  ylim(0,150)+
  labs(fill = 'Survived')



# Lastly, see if there is a pattern when using combination of Pclass and Title
ggplot(data.combined[1:891,], aes(x = Ticket.First.char, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass and Title")+
  xlab('Ticket.First.Char')+
  ylab('Total Count')+
  ylim(0,200)+
  labs(fill = 'Survived')

# Next up - Fares Titanic Passengers Paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))


# Can't treat Fare as factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("Combined Fare Distribution")+
  xlab('Fare')+
  ylab('Total COunt')+
  ylim(0, 200)

# Lets see if Fare have predictive power.
ggplot(data.combined[1:891,], aes(x = Fare , fill = Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass + Title)+
  ggtitle('Pclass and Title')+
  xlab('Fare')+
  ylab('Total Count')+
  ylim(0, 50)+
  labs(fill = 'Survived')


# Analysis of Cabin Variable
str(data.combined$Cabin)

# Displaying first 100 characters of Cabin
data.combined$Cabin[1:100]


# Replacing Empty cabin with "U"
data.combined[which(data.combined$Cabin == ""),'Cabin'] <- 'U'
data.combined$Cabin[1:100]


# Take a look at just the first character as a Factor
cabin.firt.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.firt.char)
levels(cabin.firt.char)


# Adding to a combine data set and plot
data.combined$Cabin.First.Char <- cabin.firt.char


# High level plot
ggplot(data.combined[1:891,], aes(x = Cabin.First.Char, fill = Survived))+
  geom_bar()+
  ggtitle('Survivability by Cabin First Char')+
  xlab('Cabin.First.Char')+
  ylab('Total Count')+
  labs(fill = 'Survived')




#********************************Put on hold*******************************#


#*************************************************
#*************************************************
#        Video #4 - Exploratory Modeling
#*************************************************
#*************************************************

library(randomForest)


# First Training Data set
rf.train.1 <- data.combined[1:891, c('Pclass', 'Title')]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

str(data.combined)

# Training a randomForest using Pclass, Title and SibSp.
rf.train.2 <- data.combined[1:891, c('Pclass', 'Title', 'SibSp')]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)



# Training a randomForest using Pclass, Title, SibSp and ParCh
rf.train.3 <- data.combined[1:891, c('Pclass','Title','SibSp','Parch')]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)


# Training a randomForest using Pclass, Title and Family.size
rf.train.4 <- data.combined[1:891, c('Pclass', 'Title', 'Family.size')]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)



# Training a randomForest using Pclass, Title , SibSp, and Family.size
rf.train.5 <- data.combined[1:891, c('Pclass', 'Title', 'SibSp', 'Family.size')]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


# Training a randomForest using Pclass, Title, Parch and Family.size
rf.train.6 <- data.combined[1:891, c('Pclass', 'Title', 'Parch', 'Family.size')]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)


# Training a randomForest using Pclass, Title, SibSp, Parch and Sex.
rf.train.7 <- data.combined[1:891, c('Pclass', 'Title', 'SibSp', 'Parch', 'Sex')]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7 , y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)


# Training a randomForest using Pclass, Title, Family.size and Sex.
rf.train.8 <- data.combined[1:891, c('Pclass', 'Title', 'Family.size', 'Sex')]

set.seed(1234)
rf.8 <- randomForest(x = rf.train.8 , y = rf.label, importance = TRUE, ntree = 1000)
rf.8
varImpPlot(rf.8)


#****Best Predicting Features with Max Probability****#
#****and Minimum error rate rf.8****#


#*************************************************
#*************************************************
#        Video #5 - Cross Validation
#*************************************************
#*************************************************


# Before we jump into feature engineering we need to establish a methodology
# for estimating our error rate on the test set (i.e., unseen data). This is
# critical, for without this we are more likely to overfit. Let's start with a
# submitionb of rf.8 to kaggle to see if our OOB error estimate is accurate.


# Subset our test recoards and Features.
test.submit.df <- data.combined[892:1309,c('Pclass','Title','Family.size','Sex')]


# Make Predictions
rf.8.preds <- predict(rf.8, test.submit.df)
table(rf.8.preds)


# Write out a CSV file for SUbmission for Kaggle

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.8.preds)

write.csv(submit.df, file = 'TDA_SUB_20210730_1.csv', row.names = FALSE)


# My Submission Scored 0.77033, but the OOB Predicted that we should score 0.8339.
# Lets look in to Cross-Validation using caret package to see if we can get more
# accuracy estimates

library(caret)
library(doSNOW)

# Research has shown that 10-fold CV repeated 10 times is the best place to start,
# However there are no hard and fast rules - this is where the experience of the
# Data Scientist (i.e.., the 'art') comes in to play. We'll start with 10-fold CV,
# repeated 10 times and see how it goes.


# Leverage caret to create 100 total folds, but ensure that the ratio of those that
# Survived and perished in each fold matches the overall training set. This is known as
# Stratified Core validation and generally provide better results.

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)


# Check the Stratification
table(rf.label)  # 342/549 = 0.6229508 = 62.29%

table(rf.label[cv.10.folds[[33]]]) # 307/494 = 0.6214575 = 62.14%


# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, index = cv.10.folds)


# Set up doSNOW package for multi-core training. This is going to be helpful as
# your going to be train a lot of trees.

cl <- makeCluster(6, type = 'SOCK')
registerDoSNOW(cl)


# Set seed for reproducibility and train.
set.seed(34324)
rf.8.cv.1 <- train(x = rf.train.8 , y = rf.label, method = 'rf', tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)


# Shutdown Cluster
stopCluster(cl)

# Check out Results
rf.8.cv.1     # 0.8229 = 82.29%

# Check the previous randomForest Result
rf.8          # 100-16.61 = 83.39%


# The above is only slightly more pessimistic than the rf.8 OOB prediction but,
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.

set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = 'repeatedcv', number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(6, type = 'SOCK')
registerDoSNOW(cl)

set.seed(89472)
rf.8.cv.2 <- train(x = rf.train.8, y = rf.label, method = 'rf', tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

# Shutdown Cluster
stopCluster(cl)


# Check out Results
rf.8.cv.2     # 0.8245 = 82.45%

# Check the previous randomForest Result
rf.8          # 100-16.61 = 83.39%


# Let's try 3-fold CV repeated 10 times.

set.seed(5983)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = 'repeatedcv', number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(6, type = 'SOCK')
registerDoSNOW(cl)

set.seed(89472)
rf.8.cv.3 <- train(x = rf.train.8, y = rf.label, method = 'rf', tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)

# Shutdown Cluster
stopCluster(cl)


# Check out Results
rf.8.cv.3     # 0.8207 = 82.07%

# Check the previous randomForest Result
rf.8          # 100-16.61 = 83.39%



#****Best Accuracy that we got after cross-validation****#
#****rf.8.cv.2 with the accuracy rate of 82.45% ****#


#*************************************************
#*************************************************
#        Video #6 - Exploratory Modeling - 2
#*************************************************
#*************************************************


# Let's use a single decision tree to better understand what's going on with our
# features. Obviously Random Forests are far more powerful than single trees,
# but the single tree have the advantage of being easier to understand.
library(rpart)
library(rpart.plot)


# Creating a Utility Function

rpart.cv <- function(seed, training, labels, ctrl){
  
  cl <- makeCluster(6, type = 'SOCK')
  registerDoSNOW(cl)
  
  set.seed(seed)
  
  # Leverage formula interface for training
  
  rpart.cv <- train(x = training, y = labels, method = 'rpart', tuneLength = 30,
                     trControl = ctrl)
   
  # Shutdown Cluster
  stopCluster(cl)
  
  return(rpart.cv)
  
}


# Grab Features
features <- c('Pclass', 'Family.size', 'Title', 'Sex')

rpart.train.1 <- data.combined[1:891,features]

# Run CV and Check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1


# Plot

prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)



# The Plot brings out some interesting lines of investigation, Namely:
#   1 - Titles of "Mr." and "Other" are predicted to perish at an overall
#       accuracy rate of 83.2%
#   2 - Titles of "Master" ,"Miss." & "Mrs." in 1st & 2nd class are predicted to
#       survive at an overall accuracy rate of 94.9%
#   3 - Titles of "Master" ,"Miss." & "Mrs." in 3rd class with Family.size equal
#       to 5, 6, 8 & 11 are predicted to perish with 100% accuracy.
#   4 - Titles of "Master" ,"Miss." & "Mrs." in 3rd class with Family.size not
#       equal to 5, 6, 8 & 11 are predicted to survive with 59.6% accuracy.


# Both rpart and rf confirms that title is important , let's investigate further.
table(data.combined$Title)

library(stringr)

# Parse out the last name and title
data.combined[1:25, 'Name']

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]

last.names <- sapply(name.splits, "[", 1)
last.names[1:10]


# Add last name in to data name in case we find it useful later.
data.combined$last.names <- last.names


# Now for Titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with a title of 'the'
data.combined[which(titles == "the"),]


# Re-map titles to be more exact
titles[titles %in% c('Dona.', 'the')] <- "Lady."
titles[titles %in% c("Ms.","Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.","Don.")] <- "Sir."
titles[titles %in% c('Col.','Capt.','Major.')] <- 'Officer'
table(titles)


# Make titles as factor and store
data.combined$New.Titles <- as.factor(titles)


# Visualize new version of titles
ggplot(data.combined[1:891,], aes(x = New.Titles, fill = Survived)) +
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival Rate for New.Titles by Pclass.")


# Collapse Title based on Visual analysis
indexes <- which(data.combined$New.Titles == 'Lady.')
data.combined$New.Titles[indexes] <- 'Mrs.'

indexes <- which(data.combined$New.Titles == "Dr." |
                 data.combined$New.Titles == "Rev." |
                 data.combined$New.Titles == "Sir." |
                 data.combined$New.Titles == "Officer")
data.combined$New.Titles[indexes] <- "Mr."


# Visualize
ggplot(data.combined[1:891,], aes(x = New.Titles, fill = Survived)) +
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival Rate for New.Titles by Pclass.")


# Grab Features.
features <- c("Pclass","New.Titles","Family.size","Sex")
rpart.tarin.2 <- data.combined[1:891,features]

# Run CV and Check out Results
rpart.2.cv.1 <- rpart.cv(94622, rpart.tarin.2, rf.label, ctrl.3)
rpart.2.cv.1


#------------- Random Forest Test-----------#
# Training a randomForest using Pclass, Title, Family.size and Sex.
#rf.train.9 <- data.combined[1:891, c('Pclass', 'New.Titles', 'Family.size', 'Sex')]

#set.seed(1234)
#rf.9 <- randomForest(x = rf.train.9 , y = rf.label, importance = TRUE, ntree = 1000)
#rf.9
#varImpPlot(rf.9)
#-------------------------------------------#

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Dive in 1st Class "Mr."
index.first.mr <- which(data.combined$New.Titles == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[index.first.mr,]
summary(first.mr.df)


# One Female?
first.mr.df[first.mr.df$Sex == 'female',]


# Updating New.Titles
indexes <- which(data.combined$New.Titles == "Mr." &
                 data.combined$Sex == 'female')
data.combined$New.Titles[indexes] <- 'Mrs.'


# Any other gender slit-ups?
length(which(data.combined$Sex == 'female' &
             (data.combined$New.Titles == 'Master.' |
             data.combined$New.Titles == 'Mr.')))

# Refresh Data Frame
index.first.mr <- which(data.combined$New.Titles == 'Mr.' & data.combined$Pclass == '1')
first.mr.df <- data.combined[index.first.mr,]

# Let's look at surviving 1st Class 'Mr.'
summary(first.mr.df[first.mr.df$Survived == '1',])
View(first.mr.df[first.mr.df$Survived =='1',])


# Taking a look at some of the high Fare's
indexes <- which(data.combined$Ticket == 'PC 17755' |
                 data.combined$Ticket == 'PC 17611' |
                 data.combined$Ticket == '113760')
View(data.combined[indexes,])


# Visualize Survival rates for 1st class 'Mr.' by Fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  ggtitle("1st CLass 'Mr.' Survival Rates by Fare ") +
  geom_density(alpha = 0.5)


# Engineer Features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1],'Fare'] / length(party.indexes)
  
  for(k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}


data.combined$Ticket.party.size <- ticket.party.size
data.combined$Avg.fare <- avg.fare

# Refresh 1st Class 'Mr.' dataframe
first.mr.df <- data.combined[index.first.mr,]
summary(first.mr.df)

# Visualize New Features
ggplot(first.mr.df[first.mr.df$Survived != 'None',], aes(x = Ticket.party.size, fill = Survived)) +
  ggtitle("Survival Rate 1st Class 'Mr.' by Ticket.party.size") +
  geom_density(alpha = 0.5)

ggplot(first.mr.df[first.mr.df$Survived != 'None',], aes(x = Avg.fare, fill = Survived)) +
  ggtitle("Survival Rate 1st Class 'Mr.' by Avg.Fare") +
  geom_density(alpha = 0.5)


# Hypothesis - Ticket.party.size is highly correlated with Avg.Fare
summary(first.mr.df$Avg.fare)

# One missing Value take a look
data.combined[is.na(data.combined$Avg.fare),]

# Get records for similar passengers and summery Avg.
indexes <- with(data.combined, which(Pclass == '3' & Title == 'Mr.' &
                                     Family.size == 1 & Ticket != '3701'))
similar.na.passangers <- data.combined[indexes,]
summary(similar.na.passangers$Avg.fare)

# Using Median since close to Mean and a little higher then Mean
data.combined[is.na(avg.fare), 'Avg.fare'] <- 7.840

# Leverage caret's preProcess function to  normalize data
preproc.data.combined <- data.combined[,c('Ticket.party.size', 'Avg.fare')]
preProc <- preProcess(preproc.data.combined, method = c('center','scale'))

postProc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data
cor(postProc.data.combined$Ticket.party.size, postProc.data.combined$Avg.fare)


# How about for just 1st class all-up?
indexes <- which(data.combined$Pclass == '1')
cor(postProc.data.combined$Ticket.party.size[indexes],
    postProc.data.combined$Avg.fare[indexes])

# Let's see if our feature engineering has made any progress
features <- c('Pclass', 'New.Titles', 'Family.size', 'Ticket.party.size', 'Avg.fare')
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results 
rpart.3.cv.1 <- rpart.cv(94662, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


#*************************************************
#*************************************************
#        Video #7 - Submission & Final Thoughts
#*************************************************
#*************************************************

# 
# Rpart Scores 0.83602

# Subset our test records and features
test.submit.df <- data.combined[892:1309, features]

# Make Prediction
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# Write out a CSV file for submission to Kaggle
data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds) -> submit.df

write.csv(submit.df, file = "TDA_SUB_20210918_2.csv", row.names = FALSE)


#
# Random Forest Score 0.8339
#

c('Pclass', 'New.Titles', 'Ticket.party.size', 'Avg.fare') -> features
rf.train.temp <- data.combined[1:891,features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree= 1000)
rf.temp

test.submit.df <- data.combined[892:1309,features]

# Make Prediction
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)






