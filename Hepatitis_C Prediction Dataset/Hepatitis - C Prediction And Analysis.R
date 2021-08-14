# Read CSV Data

Hepatitis_C_Dataset <- read.csv('HepatitisCdata.csv', header = TRUE)

str(Hepatitis_C_Dataset)
#View(Hepatitis_C_Dataset)

Hepatitis_C_Dataset <- Hepatitis_C_Dataset[,2:14]
#View(Hepatitis_C_Dataset)

# Changing datatype of variables
Hepatitis_C_Dataset$Sex <- as.factor(Hepatitis_C_Dataset$Sex)
Hepatitis_C_Dataset$Category <- as.factor(Hepatitis_C_Dataset$Category)

str(Hepatitis_C_Dataset)
#View(Hepatitis_C_Dataset)

levels(Hepatitis_C_Dataset$Category)

#---------- Puberty Variable Construction
#indeses <- which(Hepatitis_C_Dataset$Age < 15)
#Hepatitis_C_Dataset$Puberty[indeses] <- 'Child'

#indeses <- which(Hepatitis_C_Dataset$Age > 14)
#Hepatitis_C_Dataset$Puberty[indeses] <- 'Adult'

#Hepatitis_C_Dataset$Puberty[which(Hepatitis_C_Dataset$Puberty == 'Adult')]
#---------- Puberty Variable Construction

# Disease Variable Construction
indeses <- which(Hepatitis_C_Dataset$Category == '1=Hepatitis')

Hepatitis_C_Dataset$Disease[indeses] <- '1'

indeses <- which(Hepatitis_C_Dataset$Category == '2=Fibrosis')

Hepatitis_C_Dataset$Disease[indeses] <- '2'

indeses <- which(Hepatitis_C_Dataset$Category == '3=Cirrhosis')

Hepatitis_C_Dataset$Disease[indeses] <- '3'

indeses <- which(Hepatitis_C_Dataset$Category == '0=Blood Donor')

Hepatitis_C_Dataset$Disease[indeses] <- '0'

indeses <- which(Hepatitis_C_Dataset$Category == '0s=suspect Blood Donor')

Hepatitis_C_Dataset$Disease[indeses] <- '0s'


# Separating Data Based on Category
Blood_Donor <- Hepatitis_C_Dataset[which(Hepatitis_C_Dataset$Category == '0=Blood Donor'),]

S_Blood_Donor <- Hepatitis_C_Dataset[which(Hepatitis_C_Dataset$Category == '0s=suspect Blood Donor'),]

Hepatitis_Dataset <- Hepatitis_C_Dataset[which(Hepatitis_C_Dataset$Category == '1=Hepatitis'),]

Fibrosis_Dataset <- Hepatitis_C_Dataset[which(Hepatitis_C_Dataset$Category == '2=Fibrosis'),]

Cirrhosis_Dataset <- Hepatitis_C_Dataset[which(Hepatitis_C_Dataset$Category == '3=Cirrhosis'),]

Some_Form_Hepatitis_Dataset <- Hepatitis_C_Dataset[which(Hepatitis_C_Dataset$Category == '1=Hepatitis' |
                                                         Hepatitis_C_Dataset$Category == '2=Fibrosis' |
                                                         Hepatitis_C_Dataset$Category == '3=Cirrhosis'),]

# Loading Libraries
library(ggplot2)
library(stringr)
library(randomForest)
library(caret)

# Hepatitis Count Based on Gender
ggplot(Hepatitis_Dataset, aes(x = Category, fill = Sex)) +
  ggtitle("Hepatitis Based on Gender") +
  facet_wrap(~Sex) +
  stat_count()

# Fibrosis Count Based on Gender
ggplot(Fibrosis_Dataset, aes(x = Category, fill = Sex)) +
  ggtitle("Fibrosis Based on Gender") +
  facet_wrap(~Sex) +
  stat_count()

# Cirrhosis Count Based on Gender
ggplot(Cirrhosis_Dataset, aes(x = Category, fill = Sex)) +
  ggtitle("Cirrhosis Based on Gender") +
  facet_wrap(~Sex) +
  stat_count()

# Some_Form_Hepatitis Count Based on Gender
ggplot(Some_Form_Hepatitis_Dataset, aes(x = Category, fill = Sex)) +
  ggtitle("Some_Form_Hepatitis Based on Gender") +
  facet_wrap(~Sex) +
  stat_count()


# Starting our Random Forest Modeling.
# First Testing Data Set with Some_Form_Hepatitis_Dataset
rf.S_F_Hepatitis.1 <- Some_Form_Hepatitis_Dataset[,c(2,4,5,6,7,8,9,10,11,12,13)]
rf.S_F_Hepatitis.label <- as.factor(Some_Form_Hepatitis_Dataset$Disease)

set.seed(1234)
rf.1 <- randomForest(x = rf.S_F_Hepatitis.1, y = rf.S_F_Hepatitis.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# --- Remove NA or Predict the Value.

