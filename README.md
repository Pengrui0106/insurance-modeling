# HMEQ (Home Equity) Loan-Default Prediction Model Project:
Explanation: People get loans from our bank, they may default our loans. If they defaulted the mortgage loans, then it is a bad loan, we assign this person a binary BAD_FLAG variables with 1 to be bad and 0 to be good. They defaulted a certain amount of money and we assign this variables a name as LOSS_AMT because our bank is losing money from it.


## 🧠 Objective
Our goal is to use demographic and behavioral information to create a reliable model to predict what BAD_FLAG probability(0-1) our future clients will get and what LOSS_AMT they will default.


## ➡️ Step 1: Explore Data and Clean Data
Take a sample to see data structure and data type
Make basic box-whisker plots and histograms to see the holistic data distribution.
Impute data with IMP as the new column, fill all the NULL with estimated values.
Perform one hot encoding, create flags for each categorical variables.

## ➡️ Step 2: Classification Tree for Probability Model and Regression Tree for Severity Model
Make basic classification decision tree model to estimate the defaulting probability (BAD_FLAG).
Make basic regression decision tree model to estimate the defaulting amount (LOSS_AMT).
Decide what are the main reasons that people default loans: big amount of loan and credit lines. 
Multiple predicted BAD_FLAG and LOSS_AMT to create expected loss.

## ➡️ Step 3: Model Validation
Split data into training and testing datasets randomly. 
Compare different RMSEs of different decision tree models to decide which tree is better in consistency.

## ➡️ Step 4: Model Comparison
Compare decision tree, random forest, gradient boosting, logistic regression, linear regression models to see which has a better performance in ROC/RMSE.
![ROC Curve](./images/roc_curve.png)

## ➡️ Step 5: Reduce Dimensionalities with PCA and tSNE
Add new PCA variables into Step 4 random forest decision tree to see whether it improves the accuracy.

## ➡️ Step 6: 📊 Insights and Results Sharing with Cluster Analysis
![cluster](./images/cluster.png)
Black(C1) means "poor" but responsible;
Green(C3) means "average income" and responsible;
Red(C2) means "rich" but more possible to default because they have more loan amount.
Blue(C4) means "average income" but highly possible to default on loans.

Summary: Defaulting Risk
Cluster 4 blue (not responsible) > Cluster 2 red (large mortgage due) > Cluster 3 green (large loan) > Cluster 1 black (smaller loan)
