

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
  xlab('Title')+ 
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
data.combined$title <- as.character(data.combined$title)

# Validates that the 'Master.' is a good proxy for Male Children
boys <- data.combined[which(data.combined$title == "Master."),]
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


# Moving on to SibSp variable, summerize the variable.
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
  xlab('Family Size')+
  ylab('Total Count')+
  ylim(0,300)+  # Setting the limit of Y-axis to 300.
  labs('Survived')











