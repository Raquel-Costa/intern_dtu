rm(list = ls())
setwd("C:/Users/rakac/OneDrive - Universidade de Lisboa/Faculdade/6 ano/QMRA genomic data")
library(readxl)
library(caret)
library(knitr)
library(dplyr)
library(doParallel)
Ecoli <- read_excel("a.xlsx")
Ecoli <- Ecoli[,1:50]

#Parallel Processing
#Specifies amount of cores R can use 
#Find out how many cores are available (if you don't already know)
cores<-detectCores()
#Create cluster with desired number of cores, leave one free          
#core processes
cl <- makeCluster(cores[1]-1)
#Register cluster
registerDoParallel(cl)

####################
#Data Preprocessing#
####################

#Near zero variance removal
#First remove clinical variable response variable "Clinical" 
Ecoli2 <- Ecoli[1:105,-1]

#Identify near zero variance variables 
nzv <- nearZeroVar(Ecoli2)
filtered_vars <- Ecoli2[, -nzv]
Ecoli3 <- filtered_vars 
dim(Ecoli3)

#Add back the clinical variable response variable "Clinical"
Ecoli3$Clinical <- as.factor(Ecoli3$Clinical[1:105])


# randomly pick 70% of the number of observations 
index <- sample(1:nrow(Ecoli3),size = 0.7*nrow(Ecoli3))

# subset  to include only the elements in the index
training <- Ecoli3[index,] 

# subset  to include all but the elements in the index
testing <- Ecoli3[-index,] 

#Here are the dimensions of various subsets after split
dim(training)
dim(testing)

#Exploration of data
#Exploration of response variable "Clinical".

qplot(Clinical,data=Ecoli3, main="Distribution of clinical outcomes")  +theme(axis.text=element_text(size=14, color="black"))+ 
  theme(axis.title=element_text(face="bold",size="14")) 

# Considerable imbalance can be observed in number of isolates for each response class

##################################
#Model Building#
##################################
#Training control parametsrs. Ten fold cross-validation
fitCtrl <- trainControl(method = "cv",number = 10, verboseIter = F)

#Model Selection

#Model selection is carried out by building multiple models and later selecting best performing models 
#based on average accuracy. Gradient boosting (gbm), random forest (rf), support vector machine with 
#radial kernel (svmr), support vector machine with linear kernel (svml), neural network (nn) and 
#logit boost (lb) are primary candidates

#Evaluation of multiple models
#Models are build 10 times and their respective accuracies are saved 

# Dataframe for saving of values from the 10 fold 
predDf <- data.frame(run = 0, time = 0, gbm = 0, rf = 0, svmr = 0, svml = 0, lb = 0)

start.time.all = Sys.time() #log the starting time
# Run the model buiding 10 times & record accuracy over test set
for (i in 1:10){
  index <- sample(1:nrow(Ecoli3),size = 0.7*nrow(Ecoli3)) 
  training <- Ecoli3[index,] 
  testing <- Ecoli3[-index,] 
  dim(training)
  dim(testing)
  #Start building model
  start.time = Sys.time()
  mod.gbm <- train(Clinical ~ . , data= training , method = "gbm", trControl = fitCtrl, verbose = F)
  mod.rf <- train(Clinical ~ . , data= training , method = "rf", trControl = fitCtrl, verbose = F)
  mod.svmr <- train(Clinical ~ . , data= training , method = "svmRadial", trControl = fitCtrl, verbose = F)
  mod.svml <- train(Clinical ~ . , data= training , method = "svmLinear", trControl = fitCtrl, verbose = F)
  mod.lb <- train(Clinical ~ . , data= training , method = "LogitBoost", trControl = fitCtrl, verbose = F)
  stop.time = Sys.time()
  
  #Predictions
  pred_val <- c( i, (stop.time - start.time),
                 unname(confusionMatrix(predict(mod.gbm, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.rf, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.svmr, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.svml, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.lb, testing), testing$Clinical)$overall[1]))
  predDf <- rbind(predDf, pred_val)
}
stop.time.all = Sys.time()

#calculate total time for run execution
print(stop.time.all - start.time.all)

#correct the prediction frame
predDf <- predDf[-1,]

#Accuracy of multiple models
#Following shows the accuracy of all models for all runs. Please note that models are refereed by short names.

rownames(predDf) <- NULL
kable(predDf[,-c(2)], digits = 3)


#Average accuracy of all runs for all models are as per following

modAccuracy <- data.frame(colMeans(predDf[,-c(1,2)]))
colnames(modAccuracy) <- "Avg. Accuracy"
kable(t(modAccuracy), digits = 3)


#From average accuracy point of view, svml is best performing model while
#rf and lb are second and third best models respectively. 
##However ANOVA showed these differences to be non-significant 


#Select the final set of Models & out of sample accuracy
#Best models are used to predict values on validation data set (only once) for calculation of "out of sample" accuracy.
validAccuracy <- data.frame(Accuracy = c(
  confusionMatrix(predict(mod.rf, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.gbm, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.lb, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.svml, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.svmr, testing), testing$Clinical)$overall[1]))
rownames(validAccuracy) <- c("rf", "gbm", "lb", "svml", "svmr")
kable(t(validAccuracy), digits = 3)

##Check model statistics: I use random forest 

confusionMatrix(predict(mod.rf, testing), testing$Clinical)

#Model with class imbalances (Supplemental Figure 1) performed poorly at an accuracy of 0.28 (CI: 0.14, 0.47) 
#and dismal Kappa value of -0.05.  Mitigation for this class imbalance was performed by up-sampling cases 
#from the minority classes with replacement.

##############################
#Resampling for class balance#  
##############################
set.seed(123)
up_train <- upSample(x =Ecoli3, y = Ecoli3$Clinical)

table(up_train$Class)

#Rename Class to Clinical
Ecoli4 <- up_train
Ecoli4 <- rename(Ecoli4, Clinical = Class)



################################
#Repeat ML model building steps#
################################
set.seed(123)

# randomly pick 70% of the number of observations 
index <- sample(1:nrow(Ecoli4),size = 0.7*nrow(Ecoli4)) 

# subset  to include only the elements in the index
training <- Ecoli4[index,] 

# subset  to include all but the elements in the index
testing <- Ecoli4[-index,] 


#Here are the dimensions of various subsets after split
dim(training)
dim(testing)

#Exploration of data
#Exploration of predicted variable "Clinical".

qplot(Clinical,data=Ecoli4, main="Distribution of clinical outcomes")  +theme(axis.text=element_text(size=14, color="black"))+ 
  theme(axis.title=element_text(face="bold",size="14"))

#The cross validation. number = 10, could be higher
fitCtrl <- trainControl(method = "cv",number = 10, verboseIter = F, allowParallel = T)


# generate dataframe over multiple prediction
predDf <- data.frame(run = 0, time = 0, gbm = 0, rf = 0, svmr = 0, svml = 0, lb = 0)
#```
##Running the ML algorithms
#```{r}
start.time.all = Sys.time() #log the starting time
# Run the model buiding 10 times & record accuracy over test set
for (i in 1:10){
  
  # randomly pick 70% of the number of observations 
  index <- sample(1:nrow(Ecoli4),size = 0.7*nrow(Ecoli4)) 
  
  # subset  to include only the elements in the index
  training <- Ecoli4[index,] 
  
  # subset  to include all but the elements in the index
  testing <- Ecoli4[-index,] 
  
  
  #Here are the dimensions of various subsets after split
  dim(training)
  dim(testing)
  
  #Start building model
  start.time = Sys.time()
  mod.gbm <- train(Clinical ~ . , data= training , method = "gbm", trControl = fitCtrl, verbose = F)
  mod.rf <- train(Clinical ~ . , data= training , method = "rf", trControl = fitCtrl, verbose = F)
  mod.svmr <- train(Clinical ~ . , data= training , method = "svmRadial", trControl = fitCtrl, verbose = F)
  mod.svml <- train(Clinical ~ . , data= training , method = "svmLinear", trControl = fitCtrl, verbose = F)
  mod.lb <- train(Clinical ~ . , data= training , method = "LogitBoost", trControl = fitCtrl, verbose = F)
  stop.time = Sys.time()
  
  #Predictions
  pred_val <- c( i, (stop.time - start.time),
                 unname(confusionMatrix(predict(mod.gbm, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.rf, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.svmr, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.svml, testing), testing$Clinical)$overall[1]),
                 unname(confusionMatrix(predict(mod.lb, testing), testing$Clinical)$overall[1]))
  predDf <- rbind(predDf, pred_val)
}
stop.time.all = Sys.time()
#calculate total time for execution
print(stop.time.all - start.time.all)

#correct the prediction frame
predDf <- predDf[-1,]
predDf

##Accuracy of the models

#Accuracy of multiple models
#Following shows the accuracy of all models for all runs. Please note that models are refereed by short names.
library(knitr)
rownames(predDf) <- NULL
kable(predDf[,-c(2)], digits = 3)


#Average accuracy of all runs for all models are as per following

modAccuracy <- data.frame(colMeans(predDf[,-c(1,2)]))
colnames(modAccuracy) <- "Avg. Accuracy"
kable(t(modAccuracy), digits = 3)



#Select the final set of Models & out of sample accuracy
#Best models are used to predict values on validation data set (only once) for calculation of "out of sample" accuracy.
validAccuracy <- data.frame(Accuracy = c(
  confusionMatrix(predict(mod.rf, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.gbm, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.lb, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.svml, testing), testing$Clinical)$overall[1],
  confusionMatrix(predict(mod.svmr, testing), testing$Clinical)$overall[1]))
rownames(validAccuracy) <- c("rf", "gbm", "lb", "svml", "svmr")
kable(t(validAccuracy), digits = 3)


#FINAL MODEL


set.seed(123)

fitCtrlfin <- trainControl(method = "cv",number = 10, verboseIter = F, allowParallel = T)

#svml 

finMod.rf <- train(Clinical ~ . , data= training , method = "svmLinear", trControl = fitCtrlfin, verbose = F)

#Logit boost

finMod.lb <- train(Clinical ~ . , data= training , method = "LogitBoost", trControl = fitCtrlfin, verbose = F, importance = TRUE)

#Model agreement accuracy
#On original test set, to improve prediction confidence level various model agreement accuracy is used. 
#Here additionally, neuralnet, gradient boosting, logit boost models are built on original training dataset.


#gbm for agreement accuracy
finMod.gbm <- train(Clinical ~ . , data= training , method = "gbm", trControl = fitCtrl, verbose = F)
# svml for agreement accuracy
finMod.svml <- train(Clinical ~ . , data= training , method = "svmLinear", trControl = fitCtrl, verbose = F)

# svml for agreement accuracy
finMod.svmr <- train(Clinical ~ . , data= training , method = "svmRadial", trControl = fitCtrl, verbose = F)

#Prediction values are generated for all models and used for checking model agreement accuracy 

#predict from 3 different best model
predFin.rf <- predict(finMod.rf,testing)
predFin.svmr <- predict(finMod.svmr,testing)
predFin.gbm <- predict(finMod.gbm,testing)
predFin.lb <- predict(finMod.lb, testing)
predFin.svml <- predict(finMod.svml, testing)
modAgreementAccuracy <- data.frame(Agreement.Accuracy = c(
  confusionMatrix(predFin.svmr,predFin.lb)$overall[1],
  confusionMatrix(predFin.lb,predFin.svml)$overall[1],
  confusionMatrix(predFin.svml,predFin.svmr)$overall[1],
  confusionMatrix(predFin.lb,predFin.rf)$overall[1]))
rownames(modAgreementAccuracy) <- c("svmr vs. lb", "lb vs. svml","svml vs. svmr","lb vs. rf")
modAgreementAccuracy


#If all models are in full agreement of predicted values, confidence in chosen model is increased to very high level.

##Final predictions

#Final prediction
#Here is the final predicted values for test cases provided.

# Final models
#Logit boost final model
finMod.lb

#Support vector machile linear final model
finMod.svml


#Making predictions from svml and lb

finPred.svml <- predict(finMod.svml,testing)
finPred.lb <- predict(finMod.lb, testing)

#Confusion matrices 

conf.svml <- confusionMatrix(predict(finMod.svml, testing), testing$Clinical)
conf.svml

conf.Lb <- confusionMatrix(predict(finMod.lb, testing), testing$Clinical)
conf.Lb

#Final Model for Important Variable Selection. 

set.seed(123)

fitCtrlfin <- trainControl(method = "cv",number = 10, verboseIter = F, allowParallel = T)

#Logit boost

finMod.lb <- train(Clinical ~ . , data= Ecoli4, method = "LogitBoost", trControl = fitCtrlfin, verbose = F, importance = TRUE)


#Important variables

lbImp <- varImp(finMod.lb, scale = FALSE)
lbImp

