#Step 1: Use the Decision Tree / Random Forest / Decision Tree code from Week 5 as a Starting Point

library( rpart )
library( rpart.plot )
library( ROCR )
library( MASS )
library( randomForest )
library( gbm )

SEED = 1
set.seed( SEED )

TARGET = "TARGET_BAD_FLAG"

PATH = "/Users/raypeng/Documents/IS 5213 Data science and big data/HMEQ_Scrubbed"
FILE_NAME = "HMEQ_Scrubbed.csv"

INFILE = paste(PATH, FILE_NAME, sep = "/")

setwd(PATH)
df = read.csv(FILE_NAME)
str(df)
summary(df)
head(df)

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE,
               prob = c(0.7,0.3) )
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]

dim(df_flag)
dim(df_train)
dim(df_test)

#Decision Tree Model
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data = df_train, TARGET_BAD_FLAG ~ ., 
                  control = tr_set, method = "class", parms = list(split = 'information'))
rpart.plot( tr_model )
tr_model$variable.importance

pt = predict( tr_model, df_test, type = "prob" )
head( pt )
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )

#Random Forest Model
rf_model = randomForest( data = df_train, TARGET_BAD_FLAG ~ ., 
                         ntree = 100, importance = TRUE )

pr = predict( rf_model, df_test )
head( pr )
pr2 = prediction ( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )

#Gradient Boosting Model
gb_model = gbm( data = df_train, TARGET_BAD_FLAG ~ ., n.trees = 100,
                distribution = "bernoulli" )

pg = predict( gb_model, df_test, type = "response" )
head(pg)
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )

#Step 2: Classification Models
#Logistic Reg All and Backward
theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data = df_train )
lr_model = stepAIC( theUpper_LR, direction = "backward")

pla = predict( theUpper_LR, df_test, type = "response" )
pla2 = prediction( pla, df_test$TARGET_BAD_FLAG )
pla3 = performance( pla2, "tpr", "fpr" )

plr = predict( lr_model, df_test, type = "response" )
plr2 = prediction( plr, df_test$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )


#LR forward step tree
treeVars = tr_model$variable.importance
treeVars = names(treeVars)
treeVarsPlus = paste( treeVars, collapse = "+")
F = as.formula( paste( "TARGET_BAD_FLAG ~", treeVarsPlus ))

tree_LR = glm( F, family = "binomial", data = df_train )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data = df_train )
#summary( tree_LR )
#summary( theLower_LR )

lrt_model = stepAIC( theLower_LR, direction = "forward",
                     scope = list(lower = theLower_LR, upper = tree_LR ))

plrt = predict( lrt_model, df_test, type = "response" )
plrt2 = prediction( plrt, df_test$TARGET_BAD_FLAG )
plrt3 = performance( plrt2, "tpr", "fpr" )

#Compare and list the important variables of RF/GB/LOGIT
importance( rf_model )
varImpPlot( rf_model )
summary.gbm( gb_model, cBars = 10 )
summary( theUpper_LR )
summary( lr_model )
summary( lrt_model )

#Plot the ROC curve
plot(pt3, col = "green")
plot( pr3, col = "red", add = TRUE )
plot( pg3, col = "blue", add = TRUE )
plot( pla3, col = "black", add = TRUE )
plot( plr3, col = "gold", add = TRUE )
plot( plrt3, col = "gray", add = TRUE )
abline(0,1,lty = 2)
legend( "bottomright", c("TREE", "RANDOM FOREST", "GRADIENT BOOSTING", "LOGIT ALL",
                         "LOGIT REG BKW", "LOGIT REG FWD TREE"), 
        col = c("green", "red", "blue","black", "gold", "gray"),
        bty = "y", lty = 1)

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values
aucLRA = performance( pla2, "auc" )@y.values
aucLRB = performance( plr2, "auc" )@y.values
aucLRT = performance( plrt2, "auc" )@y.values

print( paste("TREE AUC = ", aucT ))
print( paste("RF AUC = ", aucR ))
print( paste("GB AUC = ", aucG ))
print( paste("LR ALL AUC = ", aucLRA ))
print( paste("LR BKW AUC = ", aucLRB ))
print( paste("LRT FWD AUC = ", aucLRT ))

#Random Forest model performs best with AUC > 0.95.


#Step 3: Linear Regression
#Linear Regression Decision Tree of last week
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt),
               replace = TRUE, prob = c(0.7,0.3) )
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

#Decision Tree Model
tr_set = rpart.control( maxdepth = 10 )
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

#LINEAR ALL & BACKWARD TREE
theUpper_LR = lm( TARGET_LOSS_AMT ~ ., data = df_train )
theLower_LR = lm( TARGET_LOSS_AMT ~ 1, data = df_train )

summary( theUpper_LR )
summary( theLower_LR )

lr_model = stepAIC( theUpper_LR, direction = "backward",
                    scope = list(lower = theLower_LR, upper = theUpper_LR))
summary( lr_model )

plr = predict( lr_model, df_test )
head(plr)
RMSElr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr )^2 ) )


#LINEAR REG STEPWISE FORWARD TREE
treeVars = tr_model$variable.importance
treeVars = names(treeVars)
treeVarsPlus = paste( treeVars, collapse = "+")
F = as.formula( paste( "TARGET_LOSS_AMT ~", treeVarsPlus ))

tree_LR = lm( F, data = df_train )
theLower_LR = lm( TARGET_LOSS_AMT ~ 1, data = df_train )
summary( tree_LR )
summary( theLower_LR )

lrt_model = stepAIC( theLower_LR, direction = "forward",
                     scope = list(lower = theLower_LR, upper = tree_LR ))
summary( lrt_model )

plr_tree = predict( tree_LR, df_test )
head( plr_tree )
RMSElr_tree = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr_tree )^2 ) )

plr_tree_step = predict( lrt_model, df_test )
head( plr_tree_step )
RMSElr_tree_step = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr_tree_step )^2 ) )

print( paste( "Decision Tree RMSE =", RMSEt ))
print( paste( "Random Forest RMSE =", RMSEr ))
print( paste( "Gradient Boosting RMSE =", RMSEg ))

print( paste("LR BACK RMSE = ", RMSElr ))
print( paste("LR TREE RMSE = ", RMSElr_tree ))
print( paste("LR TREE STEP RMSE = ", RMSElr_tree_step ))

#Random Forest performs best with lowest RMSE around 4201.


#Step 4: Probability / Severity Model Model
#I choose LOGIT Backward to predict TARGET_BAD_FLAG

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.7, 0.3))
df_flag_train = df_flag[FLAG, ]
df_flag_test = df_flag[!FLAG, ]

tr_set = rpart.control( maxdepth = 10 )

#LOGIT Backward Model 2
theUpper_LR2 = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data = df_flag_train )
lr_model2 = stepAIC( theUpper_LR2, direction = "backward")
summary( theUpper_LR2 )
summary( lr_model2 )

pla2 = predict( theUpper_LR2, df_flag_test, type = "response" )
pla22 = prediction( pla2, df_flag_test$TARGET_BAD_FLAG )
pla23 = performance( pla22, "tpr", "fpr" )

plr2 = predict( lr_model2, df_flag_test, type = "response" )
plr22 = prediction( plr2, df_flag_test$TARGET_BAD_FLAG )
plr23 = performance( plr22, "tpr", "fpr" )

#I choose Linear Reg backward model to predict TARGET_LOSS_AMT

df_amt_2 = subset( df, TARGET_BAD_FLAG == 1)
df_amt_2$TARGET_BAD_FLAG = NULL
head(df_amt_2)

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt_2),
               replace = TRUE, prob = c(0.7,0.3) )
df_amt_train = df_amt_2[FLAG, ]
df_amt_test = df_amt_2[!FLAG, ]

mean( df_amt_2$TARGET_LOSS_AMT )
mean( df_amt_train$TARGET_LOSS_AMT )
mean( df_amt_test$TARGET_LOSS_AMT )

#Linear Reg Backward Model 
theUpper_LR3 = lm( TARGET_LOSS_AMT ~ ., data = df_amt_train )
theLower_LR3 = lm( TARGET_LOSS_AMT ~ 1, data = df_amt_train )

summary( theUpper_LR3 )
summary( theLower_LR3 )

lr_model3 = stepAIC( theUpper_LR3, direction = "backward",
                    scope = list(lower = theLower_LR3, upper = theUpper_LR3))
summary( lr_model3 )

plr3 = predict( lr_model3, df_amt_test )
head(plr3)
RMSElr3 = sqrt( mean( ( df_amt_test$TARGET_LOSS_AMT - plr3 )^2 ) )

summary( lr_model2 )
summary( lr_model3 )


p_loss_amt = plr2 * plr3
head(p_loss_amt)

RMSE2 = sqrt( mean( (df$TARGET_LOSS_AMT - p_loss_amt )^2 ))
print(RMSE2)

#I suggest the model of step 3. Step 4 has a very large RMSE over 8000. 
