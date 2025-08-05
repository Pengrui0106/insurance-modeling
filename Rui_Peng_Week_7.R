#Step 1: Use the Decision Tree / Random Forest / Decision Tree / Regression code from Week 6 as a Starting Point

library( rpart )
library( rpart.plot )
library( ROCR )
library( MASS )
library( randomForest )

library( Rtsne )

SEED = 1
set.seed( SEED )

TARGET = "TARGET_BAD_FLAG"

PATH = "/Users/raypeng/Documents/IS 5213 Data science and big data/HMEQ_Scrubbed"
FILE_NAME = "HMEQ_Scrubbed.csv"

INFILE = paste(PATH, FILE_NAME, sep = "/")

setwd(PATH)
df = read.csv(FILE_NAME)

#Step 2: PCA Analysis

#Use only the input variables. Do not use either of the target variables.

df_pca = df
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL

#Use only the continuous variables. Do not use any of the flag variables.
#Do a Principal Component Analysis (PCA) on the continuous variables.
pca2 = prcomp(df_pca[,c(1,2,4,6,8,10,12,14,16,18)] ,center=TRUE, scale=TRUE)
summary(pca2)

#Display the Scree Plot of the PCA analysis.
plot(pca2, type = "l")

#Using the Scree Plot, determine how many Principal Components you wish to use.
#I decide to use 4 PCs with PC4 has a standard deviation above 1.
#This means the first 4 PCs contain most of the information of the imputed dataset.

#Print the weights of the Principal Components. Use the weights to tell a story on what the Principal Components represent.
print(pca2)
#PC1 is more about MORTDUE, VALUE and CLNO. I call this "Financial Capacity".
#PC2 is more about DEROG, NINQ and DELINQ. I call this "Credit Risk".
#PC3 is more about YOJ, CLAGE and DELINQ. I call this "Financial Responsibility".
#PC4 is more about LOAN, YOJ and NINQ. I call this "Borrowing Intensity".

#Perform a scatter plot using the first two Principal Components. Color the scatter plot dots using the Target Flag. 
df_new = predict( pca2, df_pca )

df_flags = df
df_flags$PC1 = df_new[,"PC1"]
df_flags$PC2 = df_new[,"PC2"]  

#If you believe the graph is too cluttered, you are free to do a random sample of the data to make it more readable.
df_flags$RAND1 = sample(100, size = nrow(df_flags), replace = TRUE)
df_flags$RAND2 = sample(100, size = nrow(df_flags), replace = TRUE)

df_flags0 = df_flags[ which(df_flags$TARGET_BAD_FLAG == 0), ]
df_flags1 = df_flags[ which(df_flags$TARGET_BAD_FLAG == 1), ]

df_flags0 = df_flags0[ df_flags0$RAND1 < 30, ]
df_flags1 = df_flags1[ df_flags1$RAND1 < 90, ]

df_flagsx = rbind( df_flags0, df_flags1 )
df_flagsx = df_flagsx[ df_flagsx$RAND2 < 50, ]

#One color will represent "defaults" and the other color will represent "non defaults". 
colors <- c("steelblue", "red")
colors <- colors[df_flagsx$TARGET_BAD_FLAG + 1]
plot( df_flagsx$PC1, df_flagsx$PC2, col = colors, pch = 16 )

#Comment on whether you consider the first two Principal Components to be predictive. 
#I think they are predictive as we can see from the plot:
#Defaulted red flags are generally above the blue safe ones.
#Higher PC2 means higher credit risk and easier to default.

#However, I feel PC1 is not doing so good on distinguish the two sides.
#One possible reason is that people with different financial capacities
#may default on loans. Rich people may also overborrow and then default. 



#Step 3: tSNE Analysis

#Use only the input variables. Do not use either of the target variables.
dfu = df
dfu$TARGET_LOSS_AMT = NULL
dfu = unique(dfu)
head( dfu )

#Use only the continuous variables. Do not use any of the flag variables.
#Do a tSNE analysis on the data. Set the dimensions to 2. 
#Run two tSNE analysis for Perplexity=30.
theTSNE = Rtsne( dfu[,c(2,3,5,7,9,11,13,15,17,19)], dims = 2, 
                 perplexity = 30, verbose = TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

#Color the scatter plot dots using the Target Flag. 
#One color will represent "defaults" and the other color will represent "non defaults".
colors <- c("steelblue", "red")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col = colors, pch = 16 )

#Comment on whether you consider the tSNE values to be predictive.
#This TSNE ananlysis with perplexity of 30 is not very efficient.
#We can see some segments but there still exist many overlaps. 
#So maybe larger perplexity would be better and more cluttered.

#Repeat the previous step with a Perplexity greater than 30 (try to get a value much higher than 30).
theTSNE = Rtsne( dfu[,c(2,3,5,7,9,11,13,15,17,19)], dims = 2, 
                 perplexity = 50, verbose = TRUE, max_iter = 500)

dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]
colors <- c("steelblue", "red")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col = colors, pch = 16 )

#Repeat the previous step with a Perplexity less than 30 (try to get a value much lower than 30).
theTSNE = Rtsne( dfu[,c(2,3,5,7,9,11,13,15,17,19)], dims = 2, 
                 perplexity = 5, verbose = TRUE, max_iter = 500)
dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]
colors <- c("steelblue", "red")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col = colors, pch = 16 )

#Decide on which value of Perplexity best predicts the Target Flag.
# 50 works best. I also tried 100 but it seemed too smooth and crowded.
# 5 is a small perplexity but it is too spread out and hard to find a pattern. 

#Train two Random Forest Models to predict each of the tSNE values.
P = paste(colnames(dfu)[c(2,3,5,7,9,11,13,15,17,19)], collapse = "+")
F1 = as.formula( paste("TS1 ~", P ) )
F2 = as.formula( paste("TS2 ~", P ) )

print( F1 )
print( F2 )

ts1_model = lm( F1, data = dfu )
ts2_model = lm( F2, data = dfu )

ts1_model_rf = randomForest( data = dfu, F1, ntree = 200, importance = TRUE )
ts2_model_rf = randomForest( data = dfu, F2, ntree = 200, importance = TRUE )

df_tsne = df

df_tsne$TS1M = predict( ts1_model, df_tsne )
df_tsne$TS2M = predict( ts2_model, df_tsne )

df_tsne$TS1M_RF = predict( ts1_model_rf, df_tsne )
df_tsne$TS2M_RF = predict( ts2_model_rf, df_tsne )



df_tsne$RAND1 = sample( 100, size = nrow(df_tsne), replace = TRUE )
df_tsne$RAND2 = sample( 100, size = nrow(df_tsne), replace = TRUE )

df_tsne0 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 0), ]
df_tsne1 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 1), ]

df_tsne$RAND1 = sample( 100, size = nrow(df_tsne), replace = TRUE )
df_tsne$RAND2 = sample( 100, size = nrow(df_tsne), replace = TRUE )

df_tsne0 = df_tsne0[df_tsne0$RAND1 < 25, ]

df_tsnex = rbind( df_tsne0, df_tsne1 )
colors <- c("steelblue", "red")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M_RF, df_tsnex$TS2M_RF, col = colors, pch = 16 )

df_tsnex = df_tsnex[ df_tsnex$RAND2 < 20, ]
colors <- c("steelblue", "red")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M, df_tsnex$TS2M, col = colors, pch = 16 )

#Step 4: Tree and Regression Analysis on the Original Data
#Create a Decision Tree to predict Loan Default (Target Flag=1). 
df_model = df
df_model$TARGET_LOSS_AMT = NULL

head( df_model )

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data = df_model, TARGET_BAD_FLAG ~ ., 
             control = tr_set, method = "class", parms = list(split='gini') )
t1E = rpart( data = df_model, TARGET_BAD_FLAG ~ ., 
             control = tr_set, method = "class", parms = list(split='information') )

rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance
#Comment on the variables that were included in the model.
#For both the gini and information method, 
#they think debt income ratio is the most significant variable.
#And then both of them think DELINQ and CLAGE are important.
#Actually I remember on step 2 the PC2 is about those variables and we can conclude them as
#Credit Risk. Those variables are all about the risk of defaulting.

#Create a Logistic Regression model to predict Loan Default (Target Flag=1). 
#Use either Forward, Backward, or Stepwise variable selection. 
theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data = df_model )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data = df_model )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC( theLower_LR, direction="forward", 
                    scope  =list(lower=theLower_LR, upper=theUpper_LR))
summary( lr_model )

#Comment on the variables that were included in the model.
#For this logistic regression model, also IMP_DEBTINC has highest weight.
#Then it is IMP_DELINQ, LOAN, IMP_CLAGE. These are the same as the previous models.

#Create a ROC curve showing the accuracy of the model.
pG = predict( t1G, df_model )
pG2 = prediction( pG[,2], df_model$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_model )
pE2 = prediction( pE[,2], df_model$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plr = predict( lr_model, df_model, type="response" )
plr2 = prediction( plr, df_model$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

#Calculate and display the Area Under the ROC Curve (AUC).
plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
plot( plr3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY","REGRESSION"),
       col = c("red","green","blue"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values
aucR = performance( plr2, "auc" )@y.values

print( aucG )
print( aucE )
print( aucR )


#Step 5: Tree and Regression Analysis on the PCA/tSNE Data

#Append the Principal Component values from Step 2 to your data set.
#Using the Random Forest models from Step 3, append the two tSNE values to the data set.
df_model = df
df_model$TARGET_LOSS_AMT = NULL


df_model$PC1 = df_new[,"PC1"]
df_model$PC2 = df_new[,"PC2"]
df_model$PC3 = df_new[,"PC3"]
df_model$PC4 = df_new[,"PC4"]

df_model$TS1M_RF = predict( ts1_model_rf, df_model )
df_model$TS2M_RF = predict( ts2_model_rf, df_model )

#Remove all of the continuous variables from the data set (set them to NULL). 
#Keep the flag variables in the data set. 
df_model$LOAN = NULL
df_model$IMP_MORTDUE = NULL
df_model$IMP_VALUE = NULL
df_model$IMP_YOJ = NULL
df_model$IMP_DEROG = NULL
df_model$IMP_DELINQ = NULL
df_model$IMP_CLAGE = NULL
df_model$IMP_NINQ = NULL
df_model$IMP_CLNO = NULL
df_model$IMP_DEBTINC = NULL

head( df_model )

#Create a Decision Tree to predict Loan Default (Target Flag=1). 
tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_model, TARGET_BAD_FLAG ~ ., 
             control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_model, TARGET_BAD_FLAG ~ ., 
             control=tr_set, method="class", parms=list(split='information') )

rpart.plot( t1G )
rpart.plot( t1E )

t1G$variable.importance
t1E$variable.importance

#Comment on the variables that were included in the model. 
#Did any of the Principal Components or tSNE values make it into the model? Discuss why or why not.

#The trees are amazing with those man-made PC and TS variables included!
#For both trees, debt-income ratio flag is the most important one.
#However, our PC2 plays a very big role. And then PC1 and PC3.
#This means our PC analysis provides extra information for our trees.


#Create a Logistic Regression model to predict Loan Default (Target Flag=1). 
#Use either Forward, Backward, or Stepwise variable selection. 
theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data = df_model )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data = df_model )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC(theLower_LR, direction="forward", 
                   scope=list(lower=theLower_LR, upper=theUpper_LR))
summary( lr_model )

#Comment on the variables that were included in the model. 
#Did any of the Principal Components or tSNE values make it into the model? Discuss why or why not.

#Fortunately we have our PC2 and TS1M_RF included in the model.
#As we have expected, PC2 has a small Std. Error and a high z value,
#which means PC2 is very precise and accurate when it comes to predicting.
#TS1M_RF is not so good in this model. 

#Create a ROC curve showing the accuracy of the model.
#Calculate and display the Area Under the ROC Curve (AUC).
pG = predict( t1G, df_model )
pG2 = prediction( pG[,2], df_model$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_model )
pE2 = prediction( pE[,2], df_model$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plr = predict( lr_model, df_model, type="response" )
plr2 = prediction( plr, df_model$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
plot( plr3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","ENTROPY","REGRESSION"),
        col=c("red","green","blue"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values
aucR = performance( plr2, "auc" )@y.values

print( aucG )
print( aucE )
print( aucR )


#Step 6: Comment
#Discuss how the PCA / tSNE values performed when compared to the original data set.

#I think PCA/tSNE is a really good way to make our prediction precise.
#In the original data set, there are so many variables and when we create models,
#Some important information may be ignored.
#However, with the engineered lower-dimension values added to the original data set,
#we can create a model more robust and include more details.
#It is also a good way to find some potential risks that are not so easy to be 
#identified in the original data set. 
#So if we have a data set with many variables and rows,
#it is wise to use PCA or tSNE to get some neutral and extra information.
#This is more persuasive when we deliver our analysis to the manager or clients.
