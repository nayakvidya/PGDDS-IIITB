#Load the required libraries 

library("caret")
library("kernlab")
library("dplyr")
library("readr")
library("ggplot2")
library("gridExtra")
library("caTools")



#Read the train and test data file
data_train = read.csv("mnist_train.csv" ,stringsAsFactors = F,header=F)
data_test =  read.csv("mnist_test.csv"  ,stringsAsFactors = F,header=F)

###################################################################################
#                Data Prepration and EDA
###################################################################################
###################################################################################


#check the dimension of the training and test data set

dim(data_train) #Dimension of training - 59999 rows and 785 columns
dim(data_test)  #Dimension of test     -  9999 rows and 785 columns


#Get the structure of the training and test data set
str(data_train)  #All the columns are integer values
str(data_test )  #All the columns are integer values


#Check first few rows of data set
head(data_train)   #We see there is no header row with column names. So assign a column name for target class
head(data_test)

#Assign a column name for the target class for ease of use
colnames(data_train)[1] <-  "digit"
colnames(data_test)[1]  <-  "digit"

#Exploring the data
summary(data_train)
summary(data_test)


#check for NA's in training and test data set
sum(sapply(data_train,function(x)sum(is.na(x))) )  #we dont find any NA rows
sum(sapply(data_test, function(x)sum(is.na(x))) )  #we dont find any NA rows

#check for duplicated rows
sum(duplicated(data_train))  #we dont find any duplicated rows
sum(duplicated(data_test))   #we dont find any duplicated rows


#scaling only those columns which have values other than 0
colnum <- which(colSums(data_train) > 0)
data_train[,colnum[-1]]= sapply(data_train[,colnum[-1]],function(x)scale(x))

colnum <- which(colSums(data_test) > 0)
data_test[,colnum[-1]]= sapply(data_test[,colnum[-1]],function(x)scale(x))


#Making our target class to factor
data_train$digit <- factor(data_train$digit)
data_test$digit  <- factor(data_test$digit)

###############################################################################
#                     Model Building and Evaluation 
###############################################################################

#Use 1% of the data as the training data

#set the seed
set.seed(1)

#split the data to randomly sample 10% of the rows

train_indices <- sample.split(1:nrow(data_train),0.1*nrow(data_train))
data_train    <- data_train[train_indices,]


#Constructing Model
#-------------------------------------------------------------------------------------
#             ######### Model 1:    Linear Kernel  ###############
#-------------------------------------------------------------------------------------

Model_linear <- ksvm(digit~ ., data = data_train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, data_test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,data_test$digit)


#Output :
#Accuracy - 91.66
#Sensitivity around 90 except for class 5 and 8 
#Specificity  very high more than 95%


#Statistics by Class:
  
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9673   0.9903   0.9099   0.9000   0.9369   0.8789   0.9447   0.8969   0.8439   0.8850
#Specificity            0.9929   0.9932   0.9897   0.9871   0.9884   0.9862   0.9956   0.9936   0.9911   0.9895




#-------------------------------------------------------------------------------------
# Hyperparameter tuning and Cross Validation  - Linear - SVM 
#-------------------------------------------------------------------------------------

# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

set.seed(100)


# making a grid of C values. 
grid_linear<- expand.grid(C=c(0.01,0.1,1,2))

# Performing 5-fold cross validation
fit.svm_linear <- train(digit~., data=data_train, method="svmLinear", metric=metric, 
                 tuneGrid=grid_linear, trControl=trainControl)

# Printing cross validation result
print(fit.svm_linear)


#
#Output :
#
#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 4801, 4799, 4800, 4801, 4799 
#Resampling results across tuning parameters:
  
#  C     Accuracy   Kappa    
#0.01  0.9170055  0.9077193
#0.10  0.9016726  0.8906790
#1.00  0.9008387  0.8897513
#2.00  0.9008387  0.8897513

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was C = 0.01.

# Plotting "fit.svm" results
plot(fit.svm_linear)



#-------------------------------------------------------------------------------------
#              Valdiating the model after cross validation on test data
#-------------------------------------------------------------------------------------

evaluate_linear_test<- predict(fit.svm, data_test)
confusionMatrix(evaluate_linear_test, data_test$digit)

#Overall Statistics

#Accuracy   : 0.9286          
#Sensitivity: Above 90 except for class 8
#Specificity: Abovf 95 for all the classes

#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9806   0.9894   0.9147   0.9129   0.9460   0.9058   0.9405   0.9086   0.8747   0.9039
#Specificity            0.9935   0.9940   0.9912   0.9893   0.9885   0.9886   0.9971   0.9947   0.9927   0.9912





#-------------------------------------------------------------------------------------
#                    ####### Model 2: Polynomial Kernel  ##########
#-------------------------------------------------------------------------------------

Model_poly <- ksvm(digit~ ., data = data_train, scale = FALSE, kernel = "polydot")
Eval_poly<- predict(Model_poly, data_test)

#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_poly,data_test$digit)

#Accuracy   : 0.9166         
#Sensitivity: Above 90 except for class 5,8 and 9
#Specificity: Above 95 for all

#Statistics by Class:

#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9673   0.9903   0.9099   0.9000   0.9369   0.8789   0.9447   0.8969   0.8439   0.8850
#Specificity            0.9929   0.9932   0.9897   0.9871   0.9884   0.9862   0.9956   0.9936   0.9911   0.9895

#-------------------------------------------------------------------------------------
# Hyperparameter tuning and Cross Validation  - Polynomial - SVM 
#-------------------------------------------------------------------------------------

# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=2)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

set.seed(7)
grid_poly <- expand.grid(C=c(0.01,0.1,1,2),degree = c(1, 2, 3, 4, 5), 
                    scale = c(-100, -10, -1, 1, 10, 100))

# Performing 2-fold cross validation
fit.svm_poly <- train(digit~., data=data_train, method="svmPoly", metric=metric, 
                 tuneGrid=grid_poly, trControl=trainControl)

# Printing cross validation result
print(fit.svm_poly)


# Plotting "fit.svm" results
plot(fit.svm_poly)

#-------------------------------------------------------------------------------------
#                 Valdiating the model after cross validation on test data
#-------------------------------------------------------------------------------------

evaluate_poly_test<- predict(fit.svm_poly, data_test)
confusionMatrix(evaluate_poly_test, data_test$digit)

#Overall Statistics

#Accuracy : 0.9497 
#Sensitivity: Above 90 for all
#Specificity: Above 95 for all


#Statistics by Class:

#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9786   0.9912   0.9283   0.9386   0.9440   0.9529   0.9551   0.9358   0.9343   0.9346
#Specificity            0.9953   0.9946   0.9945   0.9933   0.9935   0.9943   0.9971   0.9957   0.9922   0.9935



#-------------------------------------------------------------------------------------
#                    ####### Model 3: RBF Kernel  ##########
#-------------------------------------------------------------------------------------

#Using RBF Kernel
Model_RBF <- ksvm(digit~ ., data = data_train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, data_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,data_test$digit)
#Accuracy : 0.9312          
#Sensitivity: Above 90 for all classes
#Specificity: Above 95 for all classes

#Statistics by Class:

#                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9704   0.9859   0.9380   0.9228   0.9420   0.9092   0.9311   0.9008   0.9014   0.9019
#Specificity            0.9950   0.9959   0.9794   0.9922   0.9926   0.9931   0.9946   0.9944   0.9929   0.9934

#-------------------------------------------------------------------------------------
# Hyperparameter tuning and Cross Validation  - RBF - SVM 
#-------------------------------------------------------------------------------------

# We will use the train function from caret package to perform crossvalidation

trainControl <- trainControl(method="cv", number=2)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

set.seed(7)
grid_RBF <- expand.grid(.sigma=c(1.0e-02,0.05), .C=c(0.1,1,10,100) )

# Performing 2-fold cross validation
fit.svm_rbf <- train(digit~., data=data_train, method="svmRadial", metric=metric, 
                 tuneGrid=grid_RBF, trControl=trainControl)

# Printing cross validation

#sigma  C    Accuracy   Kappa     
#0.01     0.1  0.2276681  0.1362790
#0.01     1.0  0.7131642  0.6811925
#0.01    10.0  0.7316639  0.7017590
#0.01   100.0  0.7316639  0.7017590
#0.05     0.1  0.1163334  0.0000000
#0.05     1.0  0.2001660  0.1056801
#0.05    10.0  0.2136645  0.1207952
#0.05   100.0  0.2136645  0.1207952

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.01 and C = 10.


# Plotting "fit.svm" results
plot(fit.svm_rbf)

#-------------------------------------------------------------------------------------
#                 Valdiating the model after cross validation on test data
#-------------------------------------------------------------------------------------

evaluate_rbf_test<- predict(fit.svm_rbf, data_test)
confusionMatrix(evaluate_rbf_test, data_test$digit)

#Overall Statistics

#Accuracy : 0.7688     
#Sensitivity: Above 90 for class 1,2.
#Specificity: Above 95 except for class 2
#
#Statistics by Class:

#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.7949   0.9586   0.9758   0.6832   0.7312   0.6783   0.6493   0.6926   0.7156   0.7631
#Specificity            0.9978   0.9988   0.7725   0.9949   0.9955   0.9979   0.9986   0.9959   0.9955   0.9951


