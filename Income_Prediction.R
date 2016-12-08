ls = rm()

install.packages("data.table")
library(data.table)
train = fread("train.csv",na.strings = c(""," ","?","NA",NA))
test = fread("test.csv",na.strings = c(""," ","?","NA",NA))
str (train)
str (test)

summary(train)

dim(train)
dim(test)

train[1:5]
test[1:5]

## Target variable income_level
## unique function provides unique values in a column
unique(train$income_level)
unique(test$income_level)

# Recoding data to 0 and 1; Since it is a binary classifier
train[,income_level := ifelse(income_level == "-50000",0,1)]
test[, income_level := ifelse(income_level == "-50000",0,1)]

# Find the percentage of classes to check for imbalance data. prop.table() gives proportion/percentage of values
round(prop.table(table(train$income_level))*100)
round(prop.table(table(test$income_level))*100)

# Changing the column types to numeric and factors using data.table package

factcols = c(2:5,7,8:16,20:29,31:38,40,41)
factcols
## factcols has the column numbers of factor variables

## setdiff is from data.table package
numcols = setdiff(1:40,factcols)  ## setdiff() saves columns other than factcols
numcols ## numcols has the numbers of numeric columns

## Formula for converting data to factors and numeric columns..(.SD is from data.table package)
train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

str(train)

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

str(test)

# Now we can see that variables have been changed to numeric and factor variables

# SUbset the categorical variables
cat_train = train[,factcols, with=FALSE]
cat_test = test[,factcols,with = FALSE]

# Subset the numerical variables
num_test = test[,numcols, with = FALSE]
num_train = train[,numcols, with = FALSE]

str(num_test)
str(num_train)
## Removing variables help in memory utilization
rm(train,test)

# Data analysis of Numerical variables

install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

# Plot function for Histogram along with the density plot
tr = function(a){
  ggplot(data = num_train, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}

tr(num_train$age)

## We can bin the Age variables as people below 20 have lesser chances of earning $50K

## Histogram for variable capital_losses
tr(num_train$capital_losses)

## Add the target variable income_level to num_train data to check relation between input and output variables

num_train[,income_level := cat_train$income_level]

## Now create a function to plot scatter plot
ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

## Function for dodge bar chart
all_bar <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

all_bar(cat_train$class_of_worker)
all_bar(cat_train$education)

## Can also check using proportionate tables
prop.table(table(cat_train$marital_status,cat_train$income_level),1)
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)

################################## Data Exploration Completed -- Other variables can be checked in a similar fashion #######################################

################## Data Cleaning process #########################################

#check missing values in numerical data
table(is.na(num_train))
table(is.na(num_test))

## Check for correlation between the numeric variables
install.packages("caret")
library(caret)

#set threshold as 0.7 fincorrelation() function() is from caret package to find the correlation, also can specify the threshold
ax = findCorrelation(x = cor(num_train), cutoff = 0.7)
## Column 7 weeks_worked_in_year is having correlation more than 0.7 so remove it
num_train <- num_train[,-ax,with=FALSE] ## Removes the correlated variables 
str(num_train)

## Remove it from test data as well to keep it consistent with training data; although it is not necessary
num_test$weeks_worked_in_year = NULL
str(num_test)

## Check for missing values in the categorical data
table(is.na(cat_train))
table(is.na(cat_test))

#check missing values per columns
mvtr = sapply(cat_train, function(x){sum(is.na(x))/length(x)}*100)
mvte = sapply(cat_test, function(x){sum(is.na(x))/length(x)}*100)

mvtrn = sapply(cat_train, function(x){sum(is.na(x))})
mvtrn
mvtr
mvte

## Function to find the sum of missing values in each column is given below
function(x){sum(is.na(x))}
function(x){sum(is.na(x))/length(x)} ## Multiply by 100 to get percentage of missing values in each column
########### Apply this using sapply function on the whole data set#################

#select columns with missing value less than 5%
cat_train = subset(cat_train, select = mvtr < 5)
cat_test = subset(cat_test, select = mvte < 5)

table(is.na(cat_train))
#set NA as Unavailable - train data
#convert to characters

cat_train =  cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")

#convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

#set NA as Unavailable - test data
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]


################### Data Cleaning Completed ###########################################################################################

############# Data Manipulation Starts here #######################################################################

#combine factor levels with less than 5% values
#train

for(i in names(cat_train))
{
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test))
{
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}

#check columns with unequal levels in training and test data
install.packages("mlr")
library(mlr)

summarizeColumns(cat_train)[,"nlevs"] ## "nlevs"(summarizecolumns is from mlr package)
summarizeColumns(cat_test)[,"nlevs"]

## Gives count of records for each value of age
num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]

#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]


num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#Bin numeric variables with Zero and MoreThanZero on training data
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

## On test data 
num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]


########################################################## Exploratory Data Analysis Completed here , now we can build machine learning models #####################

d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)


table(is.na(num_train))
table(is.na(cat_train))
#remove unwanted files
rm(num_train,num_test,cat_train,cat_test) #save memory

library(mlr)

str(d_train)
str(d_test)
table(is.na(d_train))
d_train$income_level = NULL

#create task
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

str(train.task)

#get variable importance chart
install.packages("FSelector")
library(FSelector)
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

## Handle the imbalanced data
#undersampling

train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))

#oversampling
train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))

#SMOTE
train.smote <- smote(train.task,rate = 15,nn = 5)

system.time(
  train.smote <- smote(train.task,rate = 10,nn = 3) 
)

system.time(
  test.smote <- smote(test.task,rate = 10,nn = 3) 
)

str(test.task)
str(train.task)

is.na(test.task)

## List of models and the packages for them can found
listLearners("classif","twoclass")[c("class","package")]

## Naive Bayes Classifier
install.packages("e1071")
library(e1071)
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}

fun_cv (train.task)

fun_cv(train.under)

fun_cv(train.over)

fun_cv(train.smote)

#train and predict

nB_model <- train(naive_learner, train.smote)
nB_predict <- predict(nB_model,test.task)

#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
dCM

#calculate F measure
precision <- dCM$byClass['Pos Pred Value']
recall <- dCM$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure
