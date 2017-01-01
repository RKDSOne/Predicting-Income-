library(caret)
install.packages("RANN")
library(RANN)

install.packages("xlsx")
library(xlsx)


train = read.csv("train.csv", stringsAsFactors = TRUE)

str(train)

## Missing value imputation

sum(is.na(train))

colSums(is.na(train))

preProcValues <- preProcess(train, method = c("knnImpute","center","scale"))

train_processed <- predict(preProcValues, train)

sum(is.na(train_processed))

## Converting outcome variable to numeric

train_processed$Loan_Status = ifelse(train_processed$Loan_Status == 'N',0,1)

ID = train_processed$Loan_ID

train_processed$Loan_ID = NULL

str(train_processed)

########## Creating dummy variables using one-hot encoding ######################

dmy = dummyVars("~ .",data = train_processed,fullRank = T)

train_transformed = data.frame(predict(dmy,newdata = train_processed))

str(train_transformed)

#Converting the dependent variable back to categorical

train_transformed$Loan_Status<-as.factor(train_transformed$Loan_Status)

########## Splitting Dataset using caret ###################

#Spliting training set into two parts based on outcome: 75% and 25%

index = createDataPartition(train_transformed$Loan_Status,p = 0.75,list = FALSE)

trainSet = train_transformed[index,]
testSet = train_transformed[-index,]

str(trainSet)
str(testSet)

############# Checking for Imbalanced data #########################################

prop.table(train_transformed$Loan_Status)
prop.table(table(train_transformed$Loan_Status))

############# Feature selection using rfe in Caret package: Recursive Feature Elimination ###############

control = rfeControl(functions = rfFuncs,method = "repeatedcv",repeats = 3,verbose = FALSE)

outcomeName = 'Loan_Status'

predictors = names(trainSet)[!names(trainSet) %in% outcomeName]

Loan_Pred_Profile = rfe(trainSet[,predictors],trainSet[,outcomeName],rfeControl = control)

Loan_Pred_Profile

predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome", "CoapplicantIncome")

#####################################################################################

############ Training Models using Caret Package ####################################

names(getModelInfo())

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')  ### Gradient Boosting Model

## Important features 
varImp(object = model_gbm)
plot(varImp(object = model_gbm))

model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf') ### Random Forest Model
varImp(object = model_rf)
plot(varImp(object = model_rf))

model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet') ### Neural Networks
varImp(object = model_nnet)
plot(varImp(object = model_nnet))

model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm') ## Logistic Regression -- Generalized Linear Model
varImp(object = model_glm)
plot(varImp(object = model_glm))

#### Predictions using caret package ###########

library(Metrics)

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')  ### Gradient Boosting Model

## Important features 
varImp(object = model_gbm)
plot(varImp(object = model_gbm))

predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
predictions_prob<-predict.train(object=model_gbm,testSet[,predictors],type="prob")
table(predictions)

predictions_prob[,2]

confusionMatrix(predictions,testSet[,outcomeName])



