

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

data.combined$title <- as.factor(titles)


# Since we only have survived labels from our training set,
# only use the first 891 rows.

ggplot(data.combined[1:891,], aes(x = title, fill = Survived))+
  geom_bar(binwidth = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")





















