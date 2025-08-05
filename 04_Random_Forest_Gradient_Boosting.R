#Step 1: Read in the Data
library(rpart) #to use decision tree
library(rpart.plot) #display the decision tree
library(ROCR) #print and see how acurate it is

library( randomForest )
library( gbm )

SEED = 1
set.seed( SEED )

#Read the data into R

TARGET = "TARGET_BAD_FLAG"

PATH = "/Users/raypeng/Documents/IS 5213 Data science and big data/HMEQ_Scrubbed"
FILE_NAME = "HMEQ_Scrubbed.csv"

INFILE = paste(PATH, FILE_NAME, sep = "/")

setwd(PATH)
df = read.csv(FILE_NAME)

#List the structure of the data (str)
str(df)

#Execute a summary of the data
summary(df)

#Print the first six records
head(df)

#Step 2: Classification Models
#Using the code discussed in the lecture, split the data into training and testing data sets.

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL #Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE,
               prob = c(0.7,0.3) )
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]

dim(df_flag)
dim(df_train)
dim(df_test)

#Create a Decision Tree model using the rpart library to predict the variable TARGET_BAD_FLAG
#Plot the Decision Tree and list the important variables for the tree.

#Decision Tree Model
tr_set = rpart.control( maxdepth = 10 ) #All model parameters such as tree depth are up to you.
tr_model = rpart( data = df_train, TARGET_BAD_FLAG ~ ., 
                  control = tr_set, method = "class", parms = list(split = 'information'))
rpart.plot( tr_model )
tr_model$variable.importance

pt = predict( tr_model, df_test, type = "prob" )
head( pt )
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )

#Create a Random Forest model using the randomForest library to predict the variable TARGET_BAD_FLAG
#List the important variables for the Random Forest and include the variable importance plot.

#Random Forest Model
rf_model = randomForest( data = df_train, TARGET_BAD_FLAG ~ ., 
                         ntree = 100, importance = TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head( pr )
pr2 = prediction ( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )

#Create a Gradient Boosting model using the gbm library to predict the variable TARGET_BAD_FLAG
#List the important variables for the Gradient Boosting model and include the variable importance plot.

#Gradient Boosting Model
gb_model = gbm( data = df_train, TARGET_BAD_FLAG ~ ., n.trees = 100,
                distribution = "bernoulli" )
summary.gbm( gb_model, cBars = 10 )

pg = predict( gb_model, df_test, type = "response" )
head(pg)
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )

#Using the testing data set, create a ROC curves for all models. They must all be on the same plot.
plot( pt3, col = "green" )
abline( 0, 1, lty = 2 )
legend( "bottomright", c("TREE"), col = c("green"), bty = "y", lty =1 )

plot( pt3, col = "green" )
plot( pr3, col = "red", add = TRUE )
plot( pg3, col = "blue", add = TRUE )

#Display the Area Under the ROC curve (AUC) for all models.
aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste( "Decision Tree AUC = ", aucT ))
print( paste( "Random Forest AUC = ", aucR ))
print( paste( "Gradient Boosting AUC = ", aucG ))

#Rerun with different training and testing data at least three times.
#Determine which model performed best and why you believe this.
#Write a brief summary of which model you would recommend using.

#Random Forest Model is the best for this case and it has largest AUC.
#I recommend Random Forest as shown with the red line on the ROC curve chart. 
#"Decision Tree AUC is around 0.8266"
#"Random Forest AUC is around  0.9537"
#"Gradient Boosting AUC is around  0.9034"


#Step 3: Regression Decision Tree
#Using the code discussed in the lecture, split the data into training and testing data sets.
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL #Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt),
               replace = TRUE, prob = c(0.7,0.3) )
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

#Create a Decision Tree model using the rpart library to predict the variable TARGET_LOSS_AMT
#Plot the Decision Tree and list the important variables for the tree.

#Decision Tree Model
tr_set = rpart.control( maxdepth = 10 ) #All model parameters such as tree depth are up to you.
tr_model = rpart( data = df_train, TARGET_LOSS_AMT ~ ., 
                  control = tr_set, method = "poisson" )
rpart.plot( tr_model, digits = 3, extra = 100 )
tr_model$variable.importance

pt = predict( tr_model, df_test )
head(pt)
RMSEt = sqrt( mean( (df_test$TARGET_LOSS_AMT - pt )^2 ) )

#Create a Random Forest model using the randomForest library to predict the variable TARGET_LOSS_AMT
#List the important variables for the Random Forest and include the variable importance plot.

#Random Forest Model
rf_model = randomForest( data = df_train, TARGET_LOSS_AMT ~ .,
                         ntree = 200, importance = TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head(pr)
RMSEr = sqrt( mean( (df_test$TARGET_LOSS_AMT - pr )^2 ) )

#Create a Gradient Boosting model using the gbm library to predict the variable TARGET_LOSS_AMT
#List the important variables for the Gradient Boosting model and include the variable importance plot.

#Gradient Boosting Model

gb_model = gbm( data = df_train, TARGET_LOSS_AMT ~ .,
                         n.trees = 200, distribution = "poisson" )
summary.gbm( gb_model, cBars = 10 )

pg = predict( gb_model, df_test, type = "response" )
head(pg)
RMSEg = sqrt( mean( (df_test$TARGET_LOSS_AMT - pg )^2 ) )

#Using the testing data set, calculate the Root Mean Square Error (RMSE) for all models.

print( paste( "Decision Tree RMSE =", RMSEt ))
print( paste( "Random Forest RMSE =", RMSEr ))
print( paste( "Gradient Boosting RMSE =", RMSEg ))

#Rerun with different training and testing data at least three times.
#Determine which model performed best and why you believe this.
#Write a brief summary of which model you would recommend using. Note that this is your opinion. There is no right answer. You might, for example, select a less accurate model because it is faster or easier to interpret.

#The best model is Random Forest one. This has smallest RMSE compared to two others.
#"Decision Tree RMSE is around 5288/5288/5288/5288"
#"Random Forest RMSE is around 4232/4244/4259/4210"
#"Gradient Boosting RMSE is around 5890/5256/5579/6122"

#Step 4: Probability / Severity Model Decision Tree (Push Yourself!)
#Using the code discussed in the lecture, split the data into training and testing data sets.
#Use any model from Step 2 in order to predict the variable TARGET_BAD_FLAG

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.7, 0.3))
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]

tr_set = rpart.control( maxdepth = 10 )

#Random Forest Model 2
rf_model = randomForest( data = df_train, TARGET_BAD_FLAG ~ ., 
                         ntree = 100, importance = TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr_flag = predict( rf_model, df_test )
head( pr_flag )
pr2_flag = prediction ( pr_flag, df_test$TARGET_BAD_FLAG )
pr3_flag = performance( pr2, "tpr", "fpr" )

#Develop three models to predict the variable TARGET_LOSS_AMT
#using only records where TARGET_BAD_FLAG is 1.

df_amt = subset( df, TARGET_BAD_FLAG == 1)
df_amt$TARGET_BAD_FLAG = NULL
head(df_amt)

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt),
               replace = TRUE, prob = c(0.7,0.3) )
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

#Decision Tree Model
tr_set = rpart.control( maxdepth = 10 ) #All model parameters such as tree depth are up to you.
tr_model = rpart( data = df_train, TARGET_LOSS_AMT ~ ., 
                  control = tr_set, method = "poisson" )
rpart.plot( tr_model, digits = 3, extra = 100 )
tr_model$variable.importance

pt = predict( tr_model, df_test )
head(pt)
RMSEt = sqrt( mean( (df_test$TARGET_LOSS_AMT - pt )^2 ) )

#Random Forest Model
rf_model = randomForest( data = df_train, TARGET_LOSS_AMT ~ .,
                         ntree = 200, importance = TRUE )
importance( rf_model )
varImpPlot( rf_model )

pr = predict( rf_model, df_test )
head(pr)
RMSEr = sqrt( mean( (df_test$TARGET_LOSS_AMT - pr )^2 ) )

#Gradient Boosting Model
gb_model = gbm( data = df_train, TARGET_LOSS_AMT ~ .,
                n.trees = 200, distribution = "poisson" )
summary.gbm( gb_model, cBars = 10 )

pg = predict( gb_model, df_test, type = "response" )
head(pg)
RMSEg = sqrt( mean( (df_test$TARGET_LOSS_AMT - pg )^2 ) )


print( paste( "Decision Tree RMSE =", RMSEt ))
print( paste( "Random Forest RMSE =", RMSEr ))
print( paste( "Gradient Boosting RMSE =", RMSEg ))

#Select one of the models to predict damage.
#I would choose Gradient Boosting since it has the smallest RMSE.
#"Decision Tree RMSE = 5326/5752/6632/6389"
#"Random Forest RMSE = 3547/3442/4289/5170"
#"Gradient Boosting RMSE = 3500/3430/3562/4260"

#List the important variables for both models.
importance( rf_model )
varImpPlot( rf_model )

summary.gbm( gb_model, cBars = 10 )

#Using your models, predict the probability of default and the loss given default.
#Multiply the two values together for each record.
p2 = pr_flag * pg
head(p2)

#Calculate the RMSE value for the Probability / Severity model.
RMSE2 = sqrt( mean( (df$TARGET_LOSS_AMT - p2 )^2 ))
print(RMSE2)

#Rerun at least three times to be assured that the model is optimal and not over fit or under fit.
#Comment on how this model compares to using the model from Step 3. Which one would your recommend using?

#This Gradient Boosting Model is probably underfitting 
#and the RMSE2 is super large (8521/8582/8393/9008).
#I may still recommend the Random Forest Model of Step 3 which has lowest RMSE (around 4200).
