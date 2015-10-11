################
# (1) Read data
################
#setwd('C:/Users/Eric/Documents/CSU East Bay Coursework/Spring 2015/Machine Learning/Project 1')
data = read.csv('C:/Users/Delle/Documents/Stat6620MachLearning/1987.csv')
 #unzipped with Bzip2 for Windows
####################
# (2.1) Examine data 
####################
str(data) #The output from this function answers 'Part 2.1'

#Remove extraneous columns
data$TailNum           = NULL
data$AirTime           = NULL
data$TaxiIn            = NULL
data$TaxiOut           = NULL
data$CancellationCode  = NULL
data$CarrierDelay      = NULL
data$WeatherDelay      = NULL 
data$NASDelay          = NULL
data$SecurityDelay     = NULL
data$LateAircraftDelay = NULL

data$Year = NULL #All data is from 1987

#Convert 'Cancelled' & 'Diverted' to type factor
data$Cancelled = factor(data$Cancelled, levels = c('0','1'), labels = c('no','yes'))
data$Diverted = factor(data$Diverted, levels = c('0','1'), labels = c('no','yes')) 

str(data)
str(as.factor(data$Month)) #note that there is only Q4 data for 1987

####################################
#Remove Observations with NA values
####################################
data = na.omit(data)
str(data)

########################
#Verify removal of NA's
########################

is.valid.numeric <- function(x,na.valid = FALSE, nan.valid = FALSE, Inf.valid = FALSE)
{ #begin is.valid.numeric
  if ( !is.numeric(x) )                            { cat("mode is not numeric\n"); return(FALSE) }
  if ( length(x) == 0 )                            { cat("length equals 0\n"); return(FALSE) }
  if ( nan.valid == FALSE && any(is.nan(x)) )      { cat("NaN value(s) detected\n"); return(FALSE) }
  if ( na.valid  == FALSE && any(is.na(x)) )       { cat("NA value(s) detected\n"); return(FALSE) }
  if ( Inf.valid == FALSE && any(is.infinite(x)) ) { cat("Inf value(s) detected\n"); return(FALSE) }
  return(TRUE)
} #end is.valid.numeric

lapply(data[sapply(data,is.numeric)], is.valid.numeric) #only apply function to numeric columns

###########################################
#Remove cancelled or diverted observations
###########################################
table(data$Cancelled)
table(data$Diverted)
#note that after removing all obs with NA values there are no longer any cancelled or div observations
#therefore we remove the variables
data$Cancelled = NULL
data$Diverted  = NULL

#############################################################
# (2.2) Calculate mean & sd for numerical variables by month
#############################################################

var_name = character(0)
mean_oct = numeric(0)
sd_oct   = numeric(0)
mean_nov = numeric(0)
sd_nov   = numeric(0)
mean_dec = numeric(0)
sd_dec   = numeric(0)

for (i in 4:length(data[sapply(data,is.numeric)])) #start at i=4 to skip summarizing date & time variables
{
var_name[i] = names(data[sapply(data,is.numeric)])[i]
mean_oct[i] = mean( (data[sapply(data,is.numeric)])[which(data$Month == 10),i] )
sd_oct[i]   = sd( (data[sapply(data,is.numeric)])[which(data$Month == 10),i] )

mean_nov[i] = mean( (data[sapply(data,is.numeric)])[which(data$Month == 11),i] )
sd_nov[i]   = sd( (data[sapply(data,is.numeric)])[which(data$Month == 11),i] )

mean_dec[i] = mean( (data[sapply(data,is.numeric)])[which(data$Month == 12),i] )
sd_dec[i]   = sd( (data[sapply(data,is.numeric)])[which(data$Month == 12),i] )
}

#remove leading missing values
var_name = var_name[-1*1:3]
mean_oct = mean_oct[-1*1:3]
sd_oct   = sd_oct[-1*1:3]
mean_nov = mean_nov[-1*1:3]
sd_nov   = sd_nov[-1*1:3]
mean_dec = mean_dec[-1*1:3]
sd_dec   = sd_dec[-1*1:3]

#create dataframe
summary_stats = data.frame(var_name, mean_oct, sd_oct, mean_nov, sd_nov, 
                           mean_dec, sd_dec, stringsAsFactors=FALSE)
#view dataframe
summary_stats

#write dataframe to disk
write.csv(summary_stats, file = "summary_stats.csv") 

####################################################################################
# (2.3) Generate tables of counts & relative freq for categorical variables by month
####################################################################################

var_name  = character(0)
value     = character(0)
count_oct = numeric(0)
prop_oct  = numeric(0)
count_nov = numeric(0)
prop_nov  = numeric(0)
count_dec = numeric(0)
prop_dec  = numeric(0)

for (i in 1:length(data[sapply(data,is.factor)]))
{

var_name = names(data[sapply(data,is.factor)])[i]
value    = levels((data[sapply(data,is.factor)])[[i]])

count_oct = as.numeric(table((data[sapply(data,is.factor)])[which(data$Month == 10),i]))
prop_oct  = as.numeric(table((data[sapply(data,is.factor)])[which(data$Month == 10),i]))/length((data[sapply(data,is.factor)])[which(data$Month == 10),i])

count_nov = as.numeric(table((data[sapply(data,is.factor)])[which(data$Month == 11),i]))
prop_nov  = as.numeric(table((data[sapply(data,is.factor)])[which(data$Month == 11),i]))/length((data[sapply(data,is.factor)])[which(data$Month == 11),i])

count_dec = as.numeric(table((data[sapply(data,is.factor)])[which(data$Month == 12),i]))
prop_dec  = as.numeric(table((data[sapply(data,is.factor)])[which(data$Month == 12),i]))/length((data[sapply(data,is.factor)])[which(data$Month == 12),i])

#create dataframe
factor_tables = data.frame(value, count_oct, prop_oct, count_nov, prop_nov, count_dec, prop_dec, stringsAsFactors=FALSE)

#view dataframe
print(factor_tables)

#write dataframe to disk
write.csv(factor_tables, file = paste(var_name,'csv', sep='.'))

#zero out vectors for next iteration
var_name  = character(0)
value     = character(0)
count_oct = numeric(0)
prop_oct  = numeric(0)
count_nov = numeric(0)
prop_nov  = numeric(0)
count_dec = numeric(0)
prop_dec  = numeric(0)

}

###################################
# (3.1) Create 'ArrivedLate' label
###################################

ArrivedLate = ifelse(data$ArrDelay>15, 1, 0)
ArrivedLate = factor(ArrivedLate, levels = c(0,1), labels = c('on_time', 'delayed'))

table(ArrivedLate)
table(ArrivedLate)/length(ArrivedLate)

#######################################
# (3.2) Verify time delay calculations
#######################################
#Just checking  first 10,000 observations (checking too many kills my computer)
 
for (i in 1:10000)
{
  if(nchar(data$ArrTime[i],allowNA=TRUE)    == 3){data$ArrTime[i] = paste('0',data$ArrTime[i],sep='')}
  if(nchar(data$CRSArrTime[i],allowNA=TRUE) == 3){data$CRSArrTime[i] = paste('0',data$CRSArrTime[i],sep='')}
}

ActArr.split = t(sapply(data$ArrTime[1:10000], function(x) substring(x,first=c(1,3),last=c(2,4))))
CRSArr.split = t(sapply(data$CRSArrTime[1:10000], function(x) substring(x,first=c(1,3),last=c(2,4))))

#Not bothering to actually specify year/month/day since we are just comparing sheduled and actual arrival

ActArrTime.str = sprintf("1987-01-01 %s:%s:00",ActArr.split[,1],ActArr.split[,2]) 
ActArrTime = strptime(ActArrTime.str,"%Y-%m-%d %H:%M:%S")

CRSArrTime.str = sprintf("1987-01-01 %s:%s:00",CRSArr.split[,1],CRSArr.split[,2])
CRSArrTime = strptime(CRSArrTime.str,"%Y-%m-%d %H:%M:%S")

delay = difftime(ActArrTime,CRSArrTime,units='mins')

table(data$ArrDelay[1:10000] == delay, useNA = 'ifany')
table(data$ArrDelay[1:10000] == delay)/10000

#Looks pretty good. I'm finding ~99% to be calculated correctly. 
#I could probably improve this a bit by considering the date, but I'm stopping here. 

#######################################
# (4.1) kNN classification
#######################################

# randomize order of observations
set.seed(0) 
#This should make results reproducible
data_r = data[sample(nrow(data)),]

#remove problematic predictors
data_r$DepTime           = NULL
data_r$ArrTime           = NULL
data_r$ActualElapsedTime = NULL
data_r$DepDelay          = NULL
data_r$ArrDelay          = NULL
data_r$CRSDepTime        = NULL
data_r$CRSArrTime        = NULL
data_r$FlightNum         = NULL
data_r$Month             = NULL
data_r$DayofMonth        = NULL
data_r$DayOfWeek         = NULL

#remove categorical predictors
data_r$UniqueCarrier     = NULL
data_r$Origin            = NULL
data_r$Dest              = NULL

# create normalization function
normalize = function(x) {
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
}

# normalize the data
data_n = as.data.frame(lapply(data_r, normalize))
str(data_n)
#note there are really only too numeric predictors appropriate for knn analysis
#also note that CRSElapsedTime is stored as minutes in the air so the variable doesn't require any special handling

# create smaller training and test data (I get a "too many ties" error running on the full data set)
data_train = data_n[1:100000, ]
data_test  = data_n[100001:130000, ]

labels_train = ArrivedLate[1:100000] 
labels_test  = ArrivedLate[100001:130000]

# train a model on the data
library(class)

# classify test set using knn with k=9 
test_pred = knn(train = data_train, test = data_test, cl = labels_train, k=9) 

# evaluate model performance
library(gmodels)
CrossTable(x = labels_test, y = test_pred, prop.chisq=FALSE)

####################################################
# (5.1) Correlations between quantitative variables
####################################################

#randomize the 'original' data set
data_ran = data[sample(nrow(data)),]

#normalization isn't needed for this algorithm
#looking at structure of the data set
str(data_ran)

#removing problematic predictors
data_ran$DayofMonth = NULL
data_ran$DepTime = NULL
data_ran$ArrTime = NULL
data_ran$CRSArrTime = NULL
data_ran$FlightNum = NULL
data_ran$ElapsedTime = NULL
data_ran$Origin = NULL
data_ran$Dest = NULL
data_ran$DepDelay = NULL
data_ran$ActualElapsedTime = NULL
#multicollinearity with distance
data_ran$CRSElapsedTime = NULL


#Making a new variable for the busiest flight days
BusyDay = ifelse((data_ran$DayOfWeek!=2) & (data_ran$DayOfWeek!=3) & 
                    (data_ran$DayOfWeek!=4), 1, 0)
data_ran$BusyDay = factor(BusyDay, levels = c(0,1), labels = c('not busy', 'busy'))
#Turning Month into a factor
data_ran$Month = factor(data_ran$Month, levels = c(10,11,12), labels = c('Oct', 'Nov', 'Dec'))
#Removing DayOfWeek since it has been replaced by BusyDay
data_ran$DayOfWeek = NULL

#splitting the data set into training and test sets, 
#only using 130000 records so my computer doesn't break
data_train = data_ran[1:100000, ]
data_test  = data_ran[100001:130000, ]

#Creating a scatterplot/correlation matrix
library('psych')
pairs.panels(data_train[,c(4,2,5)])

###############################################
# (5.2) Regression Tree for numeric prediction
###############################################

par(mfrow=c(1,1))
# regression tree using rpart
library(rpart)
air.rpart <- rpart(data_train$ArrDelay ~ ., data=data_train)
air.rpart2 <- rpart(data_train$ArrDelay ~ ., data=data_train,
                  control = rpart.control(cp = 0.001))

# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(air.rpart, digits = 2)
rpart.plot(air.rpart2, digits = 2)

##Evaluating model performance

# generate predictions for the testing dataset
p.rpart <- predict(air.rpart, data_test)
p.rpart2 <- predict(air.rpart2, data_test)

#complexity parameter
printcp(air.rpart)
printcp(air.rpart2)

# compare the correlation
cor(p.rpart, data_test$ArrDelay)
cor(p.rpart2, data_test$ArrDelay)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, data_test$ArrDelay)
MAE(p.rpart2, data_test$ArrDelay)

# mean absolute error between actual values and mean value
MAE(mean(data_train$ArrDelay), data_test$ArrDelay)               # **** correction  ****

## Improving model performance 

# train a M5' Model Tree
library(RWeka)

m.m5p <- M5P(data_train$ArrDelay ~ ., data = data_train)

# get a summary of the model's performance
summary(m.m5p)

#m.m5p
#Number of Rules : 80

# generate predictions for the model
p.m5p <- predict(m.m5p, data_test)

# correlation between the predicted and true values
cor(p.m5p, data_test$ArrDelay)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(data_test$ArrDelay, p.m5p)


 




