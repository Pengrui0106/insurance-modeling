#Step 1: Use the code from Week 7 as a Starting Point
library( ggplot2 )
library( flexclust )

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
#Select at least 4 of the continuous variables. 
#It would be preferable if there were a theme to the variables selected.
df_pca = df_pca[c(1,2,4,6,8,10,12,14,16,18)]

#Do a Principal Component Analysis (PCA) on the continuous variables.
pca = prcomp(df_pca,center=TRUE, scale=TRUE)
summary(pca)

#Display the Scree Plot of the PCA analysis.
plot(pca, type = "l")
df_new = data.frame( predict( pca, df_pca ) )

#Using the Scree Plot, determine how many Principal Components you wish to use. 
#Note, you must use at least two. You may decide to use more. Justify your decision.
#I decide to use 4 PCs with PC4 has a standard deviation above 1.
#This means the first 4 PCs contain most of the information of the imputed dataset.

#Print the weights of the Principal Components. 
print(pca$rotation)

#Use the weights to tell a story on what the Principal Components represent.
#PC1 is more about MORTDUE, VALUE and CLNO. I call this "Financial Capacity".
#PC2 is more about DEROG, NINQ and DELINQ. I call this "Credit Risk".
#PC3 is more about YOJ, CLAGE and DELINQ. I call this "Financial Responsibility".
#PC4 is more about LOAN, YOJ and NINQ. I call this "Borrowing Intensity".

#Perform a scatter plot using the first two Principal Components. 
#Do not color the dots. Leave them black.
df_kmeans = df_new[1:2]
print( head( df_kmeans ) )
plot( df_kmeans$PC1, df_kmeans$PC2 )


#Step 3: Cluster Analysis - Find the Number of Clusters
#Use the principal components from Step 2 for this step.
#Using the methods presented in the lectures, complete a KMeans cluster analysis for N=1 to at least N=10. 
#Feel free to take the number higher.
#Print a scree plot of the clusters and determine how many clusters would be optimum. Justify your decision.

# Maximum Clusters To Search
MAX_N = 10

# Set up an array to hold the Sum of Square Errors
WSS = numeric( MAX_N )

for ( N in 1:MAX_N ) 
{
  km = kmeans( df_kmeans, centers=N, nstart=20  )
  WSS[N] = km$tot.withinss
}

df_wss = as.data.frame( WSS )
df_wss$clusters = 1:MAX_N

scree_plot = ggplot( df_wss, aes( x=clusters, y=WSS, group=1 )) +
  geom_point( size=4 ) +
  geom_line() +
  scale_x_continuous( breaks=c(2,4,6,8,10)) +
  xlab("Number of Clusters")

scree_plot

#Step 4: Cluster Analysis
#Using the number of clusters from step 3, perform a cluster analysis using the principle components from Step 2.
#Print the number of records in each cluster.
#Print the cluster center points for each cluster

# 4 would be optimum since from here the line starts to get flat. 
BEST_N = 4
km = kmeans( df_kmeans, centers=BEST_N, nstart=20  )

print( km$size )
print( km$centers )

#Convert the KMeans clusters into "flexclust" clusters
kf = as.kcca( object=km, data=df_kmeans, save.data=TRUE )
kfi = kcca2df( kf )
agg = aggregate( kfi$value, list( kfi$variable, kfi$group ), FUN=mean )

#Print the bar plot of the cluster. Describe the clusters from the bar plot.
barplot(kf)

#Cluster 1 has very low PC1(low house value) and slightly low PC2(low risk),
#they are majority responsible people and I think they probably default on loans but not so much.

#Cluster 2 has very high PC1(high financial capability) and near-zero PC2(average risk),
#they have much house loans and they may default.

#Cluster 3 has a high PC1(relatively high house value) and slightly low PC2(low risk),
#they are safe to have loans in my opinion and they tend to be responsible.

#Cluster 4 has a relatively low PC1(average house value) and very high PC2(high risk in defaulting),
#they are the most dangerous group!!!!!!

#Perform a scatter plot using the first two Principal Components. Color the plot by the cluster membership. 
clus = predict( kf, df_kmeans )
plot( df_kmeans$PC1, df_kmeans$PC2, col=clus )

#Add a legend to the plot.
legend( x="topleft", legend=c(1:BEST_N), fill=c(1:BEST_N) )

#Score the training data using the flexclust clusters. In other words, determine which cluster they are in.
df$CLUSTER = clus
agg = aggregate( df$TARGET_BAD_FLAG, list( df$CLUSTER ), FUN=mean )

#Determine if the clusters predict loan default.
#I like this cluster analysis and I think it predicts loan default very well.
#As we can see from the chart:
#Black(C1) means "poor" but responsible;
#Green(C3) means "average income" and responsible;
#Red(C2) means "rich" but more possible to default because they have more loan amount.
#Blue(C4) means "average income" but highly possible to default on loans.


#Step 5: Describe the Clusters Using Decision Trees
#Using the original data from Step 2, predict cluster membership using a Decision Tree
#Display the Decision Tree
library( rpart )
library( rpart.plot )

df_tree = df_pca
df_tree$CLUSTER = as.factor(clus)

dt = rpart( CLUSTER ~ . , data=df_tree )
dt = rpart( CLUSTER ~ . , data=df_tree, maxdepth=3 )

rpart.plot( dt )

#Using the Decision Tree plot, describe or tell a story of each cluster. Comment on whether the clusters make sense.
#I feel those clusters make sense. 
#Let's look at cluster 1 first and they are on the leftmost side,
#it means smaller house loan value, less credit card and less borrowing money inquiries.
#Cluster 1 is the biggest proportion and they are ordinary borrowers.

#Then let's look at the grey Cluster 3, high house loan value, less mortgage due,
#Which means they are responsible rich persons. 

#The orange cluster 2 are close to cluster 3, but the difference is 
#Cluster 2 people have more mortgage due and more house loan value.

#The there are interesting cluster 4:
#They don't have a high house value but they have many borrowing money inquiries,
#and they have defaulted on bills of the past years. 

#Generally, I would rank their defaulting risk
#(combining default possibility and amount) in a descending order:
#Cluster 4(not responsible) > Cluster 2(large mortgage due) > 
#Cluster 3(large loan) > Cluster 1(smaller loan)

#Step 6: Comment
#Discuss how you might use these clusters in a corporate setting. 

#After the analysis of this loan default data set, I feel clusters are useful to distinguish groups.
#For example, in marketing or customer service sector the company can get data of all the customers 
#and all the customers have different purchasing hobbies.
#Usually we can use a lot of metrics such as gender, age or regions to set data apart.
#However, if there isn't enough information that can be observed by those obvious metrics,
#we can try to use the cluster analysis.

#And just like this loan default analysis, in risk management sector,
#cluster analysis can be very important to identify which groups are high-risk 
#and which ones are not. 
