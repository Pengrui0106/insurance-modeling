#Step 1: Read in the Data
#Read the data into R
library(rpart) #to use decision tree
library(rpart.plot) #display the decision tree
library(ROCR) #print and see how acurate it is

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

#Step 2: Classification Decision Tree

#Using the code discussed in the lecture, split the data into training and testing data sets.
#Use the rpart library to predict the variable TARGET_BAD_FLAG
df_flag = df

#Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.
df_flag$TARGET_LOSS_AMT = NULL
head(df_flag)

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.8, 0.2))
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]

#Develop two decision trees, one using Gini and the other using Entropy using the training and testing data
#All other parameters such as tree depth are up to you.
tr_set = rpart.control( maxdepth =10 )
t1G = rpart( data = df_train, TARGET_BAD_FLAG ~ ., 
             control = tr_set, method = "class", parms = list(split = 'gini'))
t1E = rpart( data = df_train, TARGET_BAD_FLAG ~ ., 
             control = tr_set, method = "class", parms = list(split = 'information'))

#Plot both decision trees
#List the important variables for both trees
rpart.plot( t1G )
t1G$variable.importance

rpart.plot( t1E )
t1E$variable.importance

#Using the training data set, create a ROC curve for both trees
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col = "red" )
plot( pE3, col = "green", add = TRUE )
abline( 0,1, lty =2 )
legend( "bottomright", c("TRAIN GINI", "TRAIN ENTROPY"), 
        col = c("red", "green"), bty = "y", lty =1)

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI = ", aucG) )
print( paste("TRAIN AUC ENTROPY = ", aucE) )

fG = predict( t1G, df_train, type = "class" )
fE = predict( t1E, df_train, type = "class" )

table( fG, df_train$TARGET_BAD_FLAG )
table( fE, df_train$TARGET_BAD_FLAG )

#Using the testing data set, create a ROC curve for both trees
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col = "red" )
plot( pE3, col = "green", add = TRUE )
abline( 0,1, lty =2 )
legend( "bottomright", c("TEST GINI", "TEST ENTROPY"), 
        col = c("red", "green"), bty = "y", lty =1)

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TEST AUC GINI = ", aucG) )
print( paste("TEST AUC ENTROPY = ", aucE) )

fG = predict( t1G, df_test, type = "class" )
fE = predict( t1E, df_test, type = "class" )

table( fG, df_test$TARGET_BAD_FLAG )
table( fE, df_test$TARGET_BAD_FLAG )

#Write a brief summary of the decision trees discussing whether or not the trees are are optimal, overfit, or underfit.
#The trees are optimal. From the training and test data, we can see that both ROC are above the dash line.
#Both Gini and Entropy perform well.

#Rerun with different training and testing data at least three times.
#Determine which of the two models performed better and why you believe this
#I believe the Gini model is better after running the three times of training and test datasets.
#The Gini line is always above the line of entropy 
#the auc of gini is around 0.85/0.84/0.86 while the auc of entropy is maller: 0.84/0.83/0.85

#Step 3: Regression Decision Tree
#Using the code discussed in the lecture, split the data into training and testing data sets.
#Use the rpart library to predict the variable TARGET_LOSS_AMT
df_amt = df
#Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c(TRUE, FALSE), nrow(df_amt), replace = TRUE, prob = c(0.7, 0.3))
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]

tr_set = rpart.control( maxdepth = 10 )

#Develop two decision trees, one using anova and the other using poisson
#All other parameters such as tree depth are up to you.
#Plot both decision trees
#List the important variables for both trees
t1a = rpart( data = df_train, TARGET_LOSS_AMT ~ ., control = tr_set, method = "anova" )
rpart.plot( t1a )
t1a$variable.importance

t1p = rpart( data = df_train, TARGET_LOSS_AMT ~ ., control = tr_set, method = "poisson" )
rpart.plot( t1p )
t1p$variable.importance

#Using the training data set, calculate the Root Mean Square Error (RMSE) for both trees
p1a = predict( t1a, df_train )
RMSE1a = sqrt ( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ))

p1p = predict( t1p, df_train )
RMSE1p = sqrt ( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ))

print( paste( "TRAIN RMSE ANOVA =", RMSE1a) )
print( paste( "TRAIN RMSE POISSON =", RMSE1p) )

#Using the testing data set, calculate the Root Mean Square Error (RMSE) for both trees
p1a = predict( t1a, df_test )
RMSE1a = sqrt ( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ))

p1p = predict( t1p, df_test )
RMSE1p = sqrt ( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ))

print( paste( "TEST RMSE ANOVA =", RMSE1a) )
print( paste( "TEST RMSE POISSON =", RMSE1p) )

#Write a brief summary of the decision trees discussing whether or not the trees are are optimal, overfit, or underfit.
#Maybe the trees are bit underfitting because the RMSEs are larger than the mean.

#Rerun with different training and testing data at least three times.
#Determine which of the two models performed better and why you believe this
#Anova tree is better model
#since the RMSE of test data is always smaller than the poisson one.  

#Step 4: Probability / Severity Model Decision Tree (Push Yourself!)
#Using the code discussed in the lecture, split the data into training and testing data sets.
#Use the rpart library to predict the variable TARGET_BAD_FLAG
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.7, 0.3))
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]

tr_set = rpart.control( maxdepth = 10 )

#train data
t2_f_train = rpart( data = df_train, TARGET_BAD_FLAG ~ ., control = tr_set )
rpart.plot( t2_f_train )
p2_f_train = predict( t2_f_train, df )


#test data
t2_f_test = rpart( data = df_test, TARGET_BAD_FLAG ~ ., control = tr_set )
rpart.plot( t2_f_test )
p2_f_test = predict( t2_f_test, df )

#all data
t2_f_all = rpart( data = df_flag, TARGET_BAD_FLAG ~ ., control = tr_set )
rpart.plot( t2_f_all )
p2_f_all = predict( t2_f_all, df )

head(p2_f_train)
head(p2_f_test)
head(p2_f_all)

#Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
df_amt_2 = subset( df, TARGET_BAD_FLAG == 1)
df_amt_2$TARGET_BAD_FLAG = NULL
head(df_amt_2)

t2_a = rpart( data = df_amt_2, TARGET_LOSS_AMT ~ ., 
              control = tr_set, method = "anova" ) 
rpart.plot( t2_a )
p2_a = predict ( t2_a, df )

#List the important variables for both trees
t2_f_all$variable.importance
t2_a$variable.importance

#Using your models, predict the probability of default and the loss given default.
#Multiply the two values together for each record.
p2 = p2_f_all * p2_a
head( p2 )

#Calculate the RMSE value for the Probability / Severity model.
RMSE2 = sqrt( mean( (df$TARGET_LOSS_AMT - p2 )^2 ))

print(RMSE1a)
print(RMSE1p)
print(RMSE2)

#Rerun at least three times to be assured that the model is optimal and not over fit or under fit.
#Comment on how this model compares to using the model from Step 3. Which one would your recommend using?
#Step 4 is recommended with lower RMSE: 4867. On step 3, RMSE1a and RMSE1p were larger.





