############################   Linear Regression Assignment   ####################################

#Apart from the two packages mentioned "MASS" and "car" , we require one more package "DataCombine" as we use functions
#from those packages for data preparation.
#We assume the packages are already install and comment out the code
#Install the required packages 
#install.packages("MASS")
#install.packages("car")
#install.packages("DataCombine")

library("DataCombine")
library("MASS")
library("outliers")
library("car")

#Load the car price csv file 
cars_df<- read.csv("CarPrice_Assignment.csv")


#view the cars dataframe and check if the data is loaded properly
View(cars_df)

#datadictionary
#Car_ID			                    Unique id of each observation (Interger)		
#Symboling 			                Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 		
#carCompany			                Name of car company (Categorical)		
#fueltype			                  Car fuel type i.e gas or diesel (Categorical)		
#aspiration			                Aspiration used in a car (Categorical)		
#doornumber			                Number of doors in a car (Categorical)		
#carbody			                  body of car (Categorical)		
#drivewheel			                type of drive wheel (Categorical)		
#enginelocation		              Location of car engine (Categorical)		
#wheelbase		                	Weelbase of car (Numeric)		
#carlength		 	                Length of car (Numeric)		
#carwidth			                  Width of car (Numeric)		
#carheight			                height of car (Numeric)		
#curbweight			                The weight of a car without occupants or baggage. (Numeric)		
#enginetype			                Type of engine. (Categorical)		
#cylindernumber		              cylinder placed in the car (Categorical)		
#enginesize			                Size of car (Numeric)		
#fuelsystem			                Fuel system of car (Categorical)		
#boreratio			                Boreratio of car (Numeric)		
#stroke			                    Stroke or volume inside the engine (Numeric)		
#compressionratio	              compression ratio of car (Numeric)		
#horsepower			                Horsepower (Numeric)		
#peakrpm			                  car peak rpm (Numeric)		
#citympg			                  Mileage in city (Numeric)		
#highwaympg			                Mileage on highway (Numeric)		
#price(Dependent variable)	    Price of car (Numeric)		


#check the structure of data frame
str(cars_df$fueltype)


#################### Data Cleaning and Preparation ###################################


# remove the unnecessary columns ,here car_ID is not required ,hence remove it.
cars_df <- cars_df[,-which(colnames(cars_df)=="car_ID")]

#split the carName and use only the car company name
cars_df$CarName <- sapply(strsplit(as.character(cars_df$CarName)," ",fixed=TRUE),"[", 1)

## Duplicated and NA Rows ##

#check for duplicated rows/columns , no duplicated rows found
cars_df[which(duplicated(cars_df)),]

#check for na values , no na values found
#cars_df[which(is.na(cars_df)),]

names(cars_df)[sapply(cars_df, anyNA)]  

## Mispelled Car Names ##

#correctness of the categorical information is as per the referenced link provided in the data file Data Dictionary - carprices.xlsx
#https://archive.ics.uci.edu/ml/datasets/Automobile

#we find mispelled words in car names ,we correct those


# Create replacements data frame for mispelled carNames
Replaces <- data.frame(from = c("maxda", "porcshce","toyouta","vokswagen","vw","nissan"), to = c("mazda", "porsche","toyota","volkswagen","volkswagen","Nissan"))

# Replace wrong carNames with the correct one
cars_df <- FindReplace(data = cars_df, Var = "CarName", replaceData = Replaces,
                      from = "from", to = "to", exact = FALSE)


##  Outlier Detetction and Removal for numerical columns ##
#   We choose capping and remove values lower than 5th percentile and higher than the 95th percentile


test_outlier <- function(val){
  outlier_vals <- boxplot.stats(val)$out 
  if(length(outlier_vals) > 0) {
     qnt  <- quantile( val, probs=c(.25, .75), na.rm = T)   ## Value at 25th and 75th percentile
     caps <- quantile( val, probs=c(.05, .95), na.rm = T)   ## Value at  5th and 95th percentile
     H <- 1.5 * IQR(val, na.rm = T)                         ## 1.5 times IQR  , say buffer
     val[val < (qnt[1] - H)] <- caps[1]                     ## Replace values less than Lower limit ( ie. Val at 25%ile - buffer ) with  5th%ile value
     val[val > (qnt[2] + H)] <- caps[2]                     ## Replace values more than Upper limit ( ie. Val at 75%ile + buffer ) with 95th%ile value
  } 
}


# Check for outlier and remove each by using custom method test_outlier which uses capping 
cars_df$wheelbase        <- test_outlier(cars_df$wheelbase)
cars_df$carlength        <- test_outlier(cars_df$carlength)
cars_df$carwidth         <- test_outlier(cars_df$carwidth)
cars_df$carheight        <- test_outlier(cars_df$carheight)
cars_df$curbweight       <- test_outlier(cars_df$curbweight)
cars_df$boreratio        <- test_outlier(cars_df$boreratio)
cars_df$enginesize       <- test_outlier(cars_df$enginesize)
cars_df$stroke           <- test_outlier(cars_df$stroke)
cars_df$compressionratio <- test_outlier(cars_df$compressionratio)
cars_df$horsepower       <- test_outlier(cars_df$horsepower)
cars_df$peakrpm          <- test_outlier(cars_df$peakrpm)
cars_df$citympg          <- test_outlier(cars_df$citympg)
cars_df$highwaympg       <- test_outlier(cars_df$highwaympg) 


##########  Convert Categorical variables with 2 values to numerical using Level ###############



# convert fueltype variable to numeric with levels and store the numeric values in the same variable

summary(factor(cars_df$fueltype))
levels(cars_df$fueltype)<-c(0,1)
cars_df$fueltype<- as.numeric(levels(cars_df$fueltype))[cars_df$fueltype]

# convert aspiration variable to numeric with levels and store the numeric values in the same variable
levels(cars_df$aspiration)<-c(0,1)
cars_df$aspiration<- as.numeric(levels(cars_df$aspiration))[cars_df$aspiration]

# convert doornumber variable to numeric with levels and store the numeric values in the same variable
levels(cars_df$doornumber)<-c(0,1)
cars_df$doornumber<- as.numeric(levels(cars_df$doornumber))[cars_df$doornumber]

# convert enginelocation variable to numeric with levels and store the numeric values in the same variable
levels(cars_df$enginelocation)<-c(0,1)
cars_df$enginelocation<- as.numeric(levels(cars_df$enginelocation))[cars_df$enginelocation]



                     ##  Done with 2 Categorical variables ##

##########  Convert Categorical variables with more than 2 values to numerical using Level ###############

#Converting "CarName" into dummies
summary(factor(cars_df$CarName))
dummy_carname <- data.frame(model.matrix( ~CarName, data = cars_df))
dummy_carname <- dummy_carname[,-1]
cars_df <- cbind(cars_df[,-which(colnames(cars_df) == 'CarName')], dummy_carname)


#Converting "carbody" into dummies
summary(factor(cars_df$carbody))
dummy_carbody <- data.frame(model.matrix( ~carbody, data = cars_df))
dummy_carbody <- dummy_carbody[,-1]
cars_df <- cbind(cars_df[,-which(colnames(cars_df) == 'carbody')], dummy_carbody)




#Converting "cylindernumber" into dummies
summary(factor(cars_df$cylindernumber))
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = cars_df))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
cars_df <- cbind(cars_df[,-which(colnames(cars_df) == 'cylindernumber')], dummy_cylindernumber)


#Converting "fuelsystem" into dummies
summary(factor(cars_df$fuelsystem))
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = cars_df))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
cars_df <- cbind(cars_df[,-which(colnames(cars_df) == 'fuelsystem')], dummy_fuelsystem)

#Converting "enginetype" into dummies
summary(factor(cars_df$enginetype))
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = cars_df))
dummy_enginetype <- dummy_enginetype[,-1]
cars_df <- cbind(cars_df[,-which(colnames(cars_df) == 'enginetype')], dummy_enginetype)


#Converting "drivewheels" into dummies
summary(factor(cars_df$drivewheel))
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = cars_df))
dummy_drivewheel <- dummy_drivewheel[,-1]
cars_df <- cbind(cars_df[,-which(colnames(cars_df) == 'drivewheel')], dummy_drivewheel)


#write.csv(cars_df,"cars_prepared_dummy.csv")

########################### Model Building ###############################

#set the seed to 100
set.seed(100) 


# randomly generate row indices for train dataset
trainindices= sample(1:nrow(cars_df), 0.7*nrow(cars_df))

# generate the train data set
train = cars_df[trainindices,]

#store the rest into test data set
test = cars_df[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)



# Check the summary of model. 
summary(model_1)

##vif(model_1)

#  Create Step AIC model
step_1 <- stepAIC(model_1, direction = "both")


# Create model_2 with the variables removed as denoted by StepAIC output
model_2 <- lm(price ~ CarNamesaab+CarNamevolvo+CarNamerenault+drivewheelrwd+fuelsystemspdi +doornumber +CarNameisuzu +CarNamemazda +CarNamemitsubishi +fuelsystem2bbl+fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour, data=train)


summary(model_2)

vif(model_2)

#CarNamesaab       CarNamevolvo     CarNamerenault      drivewheelrwd     fuelsystemspdi         doornumber       CarNameisuzu 
#1.790390           1.728174           1.319723           4.340061           3.466158           1.459954           1.205702 
#CarNamemazda  CarNamemitsubishi     fuelsystem2bbl      fuelsystemmfi           fueltype      CarNameNissan  CarNamevolkswagen 
#2.972608           2.641385           8.627910           1.622095           4.856221           2.587814           1.987772 
#CarNamedodge    CarNameplymouth      CarNamesubaru  cylindernumbertwo         aspiration         CarNamebmw      CarNametoyota 
#1.908737           2.079628           1.761503           5.262657           2.366118           1.403017           2.959148 
#fuelsystemmpfi       CarNamebuick  cylindernumbersix      CarNamejaguar   CarNamechevrolet     enginelocation cylindernumberfive 
#9.595185           3.251204          10.613587           1.401749           1.635946           1.590835           4.042513 
#cylindernumberfour 
#16.256987 


# Looking at p value removing CarNameSaab with high pvalue
model_3 <- lm(price ~ CarNamevolvo+CarNamerenault+drivewheelrwd+fuelsystemspdi +doornumber +CarNameisuzu +CarNamemazda +CarNamemitsubishi +fuelsystem2bbl+fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)

summary(model_3)
vif(model_3)


## We observe that now the drivewheel has significantly high vif -  3.453238     and also p-val = 0.233395 > 0.05, hence removing it and re-building model

model_4 <- lm(price ~ CarNamevolvo+CarNamerenault+fuelsystemspdi +doornumber +CarNameisuzu +CarNamemazda +CarNamemitsubishi +fuelsystem2bbl+fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)

summary(model_4)
vif(model_4)

## now since door number has higher p-value of 0.09 we reomve that variable

model_5 <- lm(price ~ CarNamevolvo+CarNamerenault+fuelsystemspdi+CarNameisuzu +CarNamemazda +CarNamemitsubishi +fuelsystem2bbl+fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)


summary(model_5)
vif(model_5)

## fuelsystemspdi variables have high VIF but both are statistically significant. since it has a little higher VIF ,we remove that variable

model_6 <- lm(price ~ CarNamevolvo+CarNamerenault+CarNameisuzu +CarNamemazda +CarNamemitsubishi +fuelsystem2bbl+fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)


summary(model_6)
vif(model_6)

##now we see that CarNameisuzu is insignificant with p value 0.09> 0.05 .Removing that

model_7 <- lm(price ~ CarNamevolvo+CarNamerenault+CarNamemazda +CarNamemitsubishi +fuelsystem2bbl+fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_7)
vif(model_7)

##carNamemazda has relatively higher VIF > 2 and p value  0.014236, remove that
model_8 <- lm(price ~ CarNamevolvo+CarNamerenault +CarNamemitsubishi +fuelsystem2bbl+fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_8)
vif(model_8)

##fuelsystem2bbl has relatively higher VIF > 2 and p value  0.068842, remove that
model_9 <- lm(price ~ CarNamevolvo+CarNamerenault +CarNamemitsubishi +fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_9)
vif(model_9)

##CarNamemitsubishi has relatively high p value  0.090222, remove that
model_10 <- lm(price ~ CarNamevolvo+CarNamerenault + +fuelsystemmfi+fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_10)
vif(model_10)


##fuelsystemmfi  has relatively high p value  0.093766, remove that
model_11 <- lm(price ~ CarNamevolvo+CarNamerenault +fueltype+CarNameNissan +CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_11)
vif(model_11)

##carNameNissan  has relatively high p value  0.18618, remove that
model_12 <- lm(price ~ CarNamevolvo+CarNamerenault +fueltype+CarNamevolkswagen+CarNamedodge+CarNameplymouth+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_12)
vif(model_12)

##CarNameplymouth  has relatively high p value  0.204, remove that
model_13 <- lm(price ~ CarNamevolvo+CarNamerenault +fueltype+CarNamevolkswagen+CarNamedodge+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_13)
vif(model_13)
#alias(model_13)

##CarNamedodge  has relatively high p value  0.106802, remove that
model_14 <- lm(price ~ CarNamevolvo+CarNamerenault +fueltype+CarNamevolkswagen+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_14)
vif(model_14)

##CarNamerenault  has relatively high p value  0.110879, remove that
model_15 <- lm(price ~ CarNamevolvo+fueltype+CarNamevolkswagen+CarNamesubaru+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_15)
vif(model_15)

##CarNamesubaru  has relatively high p value  0.074404, remove that
model_16 <- lm(price ~ CarNamevolvo+fueltype+CarNamevolkswagen+cylindernumbertwo +aspiration+CarNamebmw+CarNametoyota+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_16)
vif(model_16)
##----------------------------------------------------------------------
##carNametoyota  has relatively high p value  0.00417, remove that
model_17 <- lm(price ~ CarNamevolvo+fueltype+CarNamevolkswagen+cylindernumbertwo +aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_17)
vif(model_17)

##CarNamevolkswagen has relatively high p value  0.00417, remove that 0.0218
model_18 <- lm(price ~ CarNamevolvo+fueltype+cylindernumbertwo +aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_18)
vif(model_18)

##fueltype has high p valued 0.0289 ,remove
model_18_1 <- lm(price ~ CarNamevolvo+cylindernumbertwo +aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive +cylindernumberfour,data=train)
summary(model_18_1)
vif(model_18_1)

##Since all variable ase significant , compare the vif score and remove cylindernumberfour with score of 13.7
model_19 <- lm(price ~ CarNamevolvo+cylindernumbertwo +aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation +cylindernumberfive ,data=train)
summary(model_19)
vif(model_19)

##cylindernumbefive becomes insignifincant with p value 0.841435 > 0.05, remove
model_20 <- lm(price ~ CarNamevolvo+cylindernumbertwo +aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar+CarNamechevrolet +enginelocation ,data=train)
summary(model_20)
vif(model_20)

##CarNamechevrolet has high p value 0.456119  > 0.05 , remove
model_21 <- lm(price ~ CarNamevolvo+cylindernumbertwo +aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar +enginelocation ,data=train)
summary(model_21)
vif(model_21)

##cylindernumbertwo can or cannot be removed , though significant , since p value 0.02  is not "***"  further refine
model_22 <- lm(price ~ CarNamevolvo+aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar +enginelocation ,data=train)
summary(model_22)
vif(model_22)

##carnamevolvo try removal 0.015041 , since it is not "***"
model_23 <- lm(price ~ aspiration+CarNamebmw+fuelsystemmpfi +CarNamebuick +cylindernumbersix+CarNamejaguar +enginelocation ,data=train)
summary(model_23)
vif(model_23)

##Now since all the variables are significant and VIF is less than 2 we stop here . model_23 is the final model


# Predict the prices in the testing dataset using model_23
Predict_1 <- predict(model_23,test)
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2


##################### Driving factors as per the final model #########################

#1.CarNamebuick
#2.aspiration
#3.CarNamebmw 
#4.CarNamejaguar 
#5.fuelsystemmpfi
#6.enginelocation
#7.cylindernumbersix 

##################### Driving factors as per the final model #########################

4.