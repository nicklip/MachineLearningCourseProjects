# Load in the packages
library(randomForest)
library(rpart)

# Loading in training and test datasets
train = read.csv("C:/Users/Delle/Documents/Stat6620MachLearning/train.csv")
test = read.csv("C:/Users/Delle/Documents/Stat6620MachLearning/test.csv")
# Making a Survived column full of NAs in the test data set  
test$Survived = vector(mode="integer", length=418)
test$Survived[1:418] = NA
# Reording the test dataset for merging
test = test[c(1,12,2:11)]
# Merging the train and test dataset
all_data = rbind(train, test)
str(all_data)

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, I gave them the value S 
all_data$Embarked[c(62,830)] = "S"
# Making emarkment a factor
all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value so I replaced it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm=TRUE)

# Making Survived a factor
all_data$Survived = factor(all_data$Survived, levels = c('0', '1'), labels = c('no','yes'))

# Dropping useless features
all_data$Ticket = NULL
all_data$Name = NULL

# Predicting Age using the other variables and a regression tree model (Imputation)
predicted_age <- rpart(Age ~ Survived + Pclass + Sex + SibSp + Parch + Fare + Cabin + Embarked,
					 data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Splitting the data back into train and test sets
train <- all_data[1:891,]
test <- all_data[892:1309,]

# Checking the structure of the train and test sets
str(train)
str(test)

# Setting seed for reproducibility
set.seed(111)

# Applying the Random Forest Algorithm
# Default is 500 trees, can't use 'cabin' variable because too many levels
my_forestlarge <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                        data = train, importance=TRUE)
# Trying a much smaller amount of trees, this model shouldn't be overfit
my_forestsmall <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                        data = train, importance=TRUE, ntree=29)

# Making predictions using the test sets
my_predictionlarge <- predict(my_forestlarge, test)
my_predictionsmall <- predict(my_forestsmall, test)

# Looking at performance
my_forestlarge
my_forestsmall

# Looking at variable importance
varImpPlot(my_forestlarge)
varImpPlot(my_forestsmall)

# Model improvement, using 3 predictor features in each tree instead of 2
my_foresttuned <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                        data = train, importance=TRUE, ntree=29, mtry = 3)

# Making predictions
my_predictiontuned <- predict(my_foresttuned, test)

# Looking at performance
my_foresttuned

# Looking at variable importance
varImpPlot(my_foresttuned)

# Creating a data frame with two columns: PassengerId & Survived (the predictions)
my_solutionsmall <- as.data.frame(cbind(test$PassengerID, my_predictionsmall))
my_solutionlarge<- as.data.frame(cbind(test$PassengerID, my_predictionlarge))
my_solutiontuned <- as.data.frame(cbind(test$PassengerID, my_predictiontuned))

# Writing solutions away to csv files to submit to kaggle, kaggle accuracies are commented
write.csv(my_solutionsmall, "my_solutionsmall.csv") # accuracy 0.77990
write.csv(my_solutionlarge, "my_solutionlarge.csv") # accuracy 0.77033
write.csv(my_solutiontuned, "my_solutiontuned.csv") # accuracy 0.76555

