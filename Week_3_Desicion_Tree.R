library(rpart) #to use decision tree
library(rpart.plot) #display the decision tree
library(ROCR) #print and see how acurate it is


PATH = "/Users/raypeng/Documents/IS 5213 Data science and big data/Insurance"
FILE_NAME = "Insurance_Scrubbed.csv"

INFILE = paste(PATH, FILE_NAME, sep = "/")

setwd(PATH)
df = read.csv(FILE_NAME)

str(df)
summary(df)
head(df)

df_flag = df
df_flag$TARGET_CLM_AMT = NULL

tr_set = rpart.control( maxdepth = 3 )

tree_flag = rpart( data = df_flag, TARGET_CLM_FLAG ~ ., control = tr_set )
tree_flag
rpart.plot( tree_flag )

tree_flag$variable.importance
  
p = predict( tree_flag, df_flag )
dfp = as.data.frame( p )
head(dfp)

p2 = prediction( p, df_flag$TARGET_CLM_FLAG)
p3 = performance ( p2, "tpr", "fpr")

plot( p3, col = rainbow(10))
abline( 0, 1, lty = 2 )

auc = performance( p2, "auc" )@y.values





  