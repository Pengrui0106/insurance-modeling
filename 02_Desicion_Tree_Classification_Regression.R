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
#Use the rpart library to predict the variable TARGET_BAD_FLAG
df_flag = df

#Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.
df_flag$TARGET_LOSS_AMT = NULL

#All other parameters such as tree depth are up to you.
tr_set = rpart.control( maxdepth = 5 )

tree_flag = rpart( data = df_flag, TARGET_BAD_FLAG ~ ., control = tr_set )
rpart.plot( tree_flag )

tree_flag$variable.importance

#Develop two decision trees, one using Gini and the other using Entropy
tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data = df_flag, TARGET_BAD_FLAG ~., 
             control = tr_set, method = "class", parms = list(split = 'gini'))

t1E = rpart( data = df_flag, TARGET_BAD_FLAG ~., 
             control = tr_set, method = "class", parms = list(split = 'information'))

#Plot both decision trees
rpart.plot( t1G )
rpart.plot( t1E )

#List the important variables for both trees
t1G$variable.importance
t1E$variable.importance

#Create a ROC curve for both trees
pG = predict ( t1G, df )
pG2 = prediction( pG[,2], df$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr")

pE = predict ( t1E, df )
pE2 = prediction( pE[,2], df$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr")

plot( pG3, col = "red" )
plot( pE3, col = "green", add = TRUE )
abline( 0,1,lty=2 )
legend("bottomright", c("GINI","ENTROPY"), 
       col = c("red", "green"), bty = "y", lty = 1)

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print(aucG)
print(aucE)
#Write a brief summary of the decision trees discussing whether or not they make sense. 
#Summary: both of the gini and entropy trees make sense.
#Because the they are both above the random guess line (black dash line).

#Which tree would you recommend using? What type of person will default on a loan?
#I recommend using the red Gini one because it has a larger area under the curve.
#Gini one has the area of 0.8433084 which is larger than 0.8293732 of the entropy one. 
#So according to the Gini decision tree, those persons tend to default on a loan:
#Debt income ratio more or equal to 45 (0.96 possibility). 
#Who have been late on bills. Who have a credit line age shorter than 178 months (0.63 possibility)

#Step 3: Regression Decision Tree
#Use the rpart library to predict the variable TARGET_LOSS_AMT
df_amt = df

#Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.
df_amt$TARGET_BAD_FLAG = NULL
mean( df_amt$TARGET_LOSS_AMT )

#All other parameters such as tree depth are up to you.
tr_set = rpart.control( maxdepth = 10 )

#Develop two decision trees, one using anova and the other using poisson
t1a = rpart(data = df_amt, TARGET_LOSS_AMT ~ ., 
            control = tr_set, method = "anova")

#Plot both decision trees
rpart.plot( t1a )

#List the important variables for both trees
t1a$variable.importance

#Calculate the Root Mean Square Error (RMSE) for both trees
p1a = predict( t1a, df )
RMSE1a = sqrt( mean( ( df$TARGET_LOSS_AMT - p1a )^2 ) )


t1p = rpart( data = df_amt, TARGET_LOSS_AMT ~ ., 
             control = tr_set, method = "poisson" )
rpart.plot( t1p )
t1p$variable.importance

p1p = predict ( t1p, df )
RMSE1p = sqrt( mean( ( df$TARGET_LOSS_AMT - p1p )^2 ) )

print( RMSE1a )
print( RMSE1p )

#Write a brief summary: whether or not they make sense. Which tree would you recommend using?
#The models make sense and I would recommend Anova tree 
#because it has less prediction error (4848.417) compared to Poisson tree (5558.973).

#What factors dictate a large loss of money?
#According to the anova chart, there are two main causing big loss of money:
#Number one reason: Big amount of loan.
#Number two reason: Credit lines. The more credit lines the persons have, the larger amount of money it may cause to the loan company. 

#Step 4: Probability / Severity Model Decision Tree (Push Yourself!)
#Use the rpart library to predict the variable TARGET_BAD_FLAG
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

t2_f = rpart( data = df_flag, TARGET_BAD_FLAG ~ ., control = tr_set )

#Plot both decision trees
rpart.plot( t2_f )
p2_f = predict ( t2_f, df )

#Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
df_amt_2 = subset( df, TARGET_BAD_FLAG == 1)
df_amt_2$TARGET_BAD_FLAG = NULL
head(df_amt_2)

t2_a = rpart( data = df_amt_2, TARGET_LOSS_AMT ~ ., 
              control = tr_set, method = "poisson" ) 
rpart.plot(t2_a)
p2_a = predict ( t2_a, df )
head( p2_f )
head( p2_a )

#List the important variables for both trees
t2_f$variable.importance
t2_a$variable.importance

#Using your models, predict the probability of default and the loss given default.
#Multiply the two values together for each record.
p2 = p2_f * p2_a
head( p2 )

#Calculate the RMSE value for the Probability / Severity model.
RMSE2 = sqrt( mean( (df$TARGET_LOSS_AMT - p2 )^2 ))
print(RMSE2)

#Comment on how this model compares to using the model from Step 3. Which one would your recommend using?
#This one is better than the model from Step 3 because this one has a smaller RMSE of 4830.517
#While in step 3, the RMSE was 4848 and 5559.




