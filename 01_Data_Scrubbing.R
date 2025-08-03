#Step 1: Read in the Data
#Read the data into R
PATH = "/Users/raypeng/Documents/IS 5213 Data science and big data/Insurance"

FILE_NAME <- "HMEQ_Loss.csv"
OUT_NAME <- "HMEQ_Loss_Scrubbed.csv"

INFILE <- file.path(PATH, FILE_NAME)
OUTFILE <- file.path(PATH, OUT_NAME)

df = read.csv (INFILE)
head(df)

setwd(PATH)
df <- read.csv(FILE_NAME)

#List the structure of the data (str)
str(df)

#Execute a summary of the data
summary(df)

#Print the first six records
head (df)

#Step 2: Box-Whisker Plots
#Plot a box plot of all the numeric variables split by the grouping variable.
group_var <- df$TARGET_BAD_FLAG

numeric_vars <- c("LOAN", "MORTDUE", "VALUE", "YOJ", "DEROG",
                  "DELINQ", "CLAGE", "NINQ", "CLNO", "DEBTINC")

for (var in numeric_vars) {
  boxplot(
    df[[var]] ~ group_var,
    #The MAIN TITLE of the box plot should be set to your name
    main = "Rui Peng",
    xlab = "Bad Loan",
    ylab = var,
    
    #Add color to the boxes
    col = c("lightblue", "lightpink")
  )
}

#Comment on whether or not there are any observable differences in the box plots between the two groups.
#So according to my observation, those people tend to have bad loan: 
#less loan amount, less mortgage due, less house value, less year of job,less credit line age
#who borrowed money more often, who have a higher debt to income ratio

#Step 3: Histograms
#Plot a histogram of at least one of the numeric variables
hist(
  df$LOAN,
  #Manually set the number of breaks to a value that makes sense
  breaks = 20,
  col = "lightblue", 
  border = "white",
  xlab = "Loan (dollars)",
  ylab = "Density",
  main = "Histogram of Loan",
  freq = FALSE
)

#Superimpose a density line to the graph
lines(
  density(df$LOAN, bw = 2000),
  col = "darkred",
  lwd = 3
)

legend(
  "topright",
  legend = c("Density Curve"), 
  col = "darkred", 
  lwd = 3,
  bty = "n"
)

#Step 4: Impute "Fix" all the numeric variables that have missing values
#For the missing Target variables, simply set the missing values to zero

#Var 1: TARGET_LOSS_AMT
df$TARGET_LOSS_AMT[is.na(df$TARGET_LOSS_AMT)] <- 0

#Var 2: MORTDUE
summary(df$MORTDUE)

#The median or mean value will be useful in most cases.
median(df$MORTDUE, na.rm = TRUE)

#Create two new variables: #One variable beginning with IMP_ and the second value beginning with M_.
df$IMP_MORTDUE <- df$MORTDUE
df$IMP_MORTDUE[ is.na(df$MORTDUE) ] = 65019
df$M_MORTDUE = is.na(df$MORTDUE) + 0

#Compute a sum for all the M_ variables
sum(df$M_MORTDUE)
summary(df)

#Delete the original variable after it has been imputed.
df$MORTDUE = NULL
summary(df)

#Try one complex imputation like the one described in the lectures.
#Var 3: VALUE
summary(df$VALUE)
a = aggregate(x=df$VALUE, by=list(df$JOB), na.rm = TRUE, FUN = median)
a = a[ order(a$x, decreasing = TRUE), ]

df$IMP_VALUE = df$VALUE
df$IMP_VALUE[ is.na(df$VALUE) & (df$JOB == "Self")] = 130631
df$IMP_VALUE[ is.na(df$VALUE) & (df$JOB == "ProfExe")] = 110007

df$IMP_VALUE[ is.na(df$VALUE) & (df$JOB == "Mgr")] = 101258
df$IMP_VALUE[ is.na(df$VALUE) & (df$JOB == "Office")] = 89094.5

df$IMP_VALUE[ is.na(df$VALUE) & (df$JOB == "Sales")] = 84473.5
df$IMP_VALUE[ is.na(df$VALUE) & (is.na(df$JOB))] = 78227
df$IMP_VALUE[ is.na(df$VALUE) & (df$JOB == "Other")] = 76599.5

df$IMP_VALUE[ is.na(df$IMP_VALUE)] = 89236 
df$M_VALUE = is.na(df$VALUE) + 0
sum(df$M_VALUE)

df$VALUE = NULL
summary(df)

#Var 4: YOJ
summary(df$YOJ)
df$IMP_YOJ <- df$YOJ
df$IMP_YOJ[is.na(df$YOJ)] <- 7
df$M_YOJ <- is.na(df$YOJ) + 0

sum(df$M_YOJ)
df$YOJ = NULL
summary(df)

#Var 5: DEROG
summary(df$DEROG)
df$IMP_DEROG <- df$DEROG
df$IMP_DEROG[is.na(df$DEROG)] <- 0
df$M_DEROG <- is.na(df$DEROG) + 0

sum(df$M_DEROG)
df$DEROG = NULL
summary(df)

#Var 6: DELINQ
summary(df$DELINQ)
df$IMP_DELINQ <- df$DELINQ
df$IMP_DELINQ[is.na(df$DELINQ)] <- 0
df$M_DELINQ <- is.na(df$DELINQ) + 0

sum(df$M_DELINQ)
df$DELINQ = NULL
summary(df)

#Var 7: CLAGE
summary(df$CLAGE)
df$IMP_CLAGE <- df$CLAGE
df$IMP_CLAGE[is.na(df$CLAGE)] <- 173.5
df$M_CLAGE <- is.na(df$CLAGE) + 0

sum(df$M_CLAGE)
df$CLAGE = NULL
summary(df)

#Var 8: NINQ
summary(df$NINQ)
df$IMP_NINQ <- df$NINQ
df$IMP_NINQ[is.na(df$NINQ)] <- 1
df$M_NINQ <- is.na(df$NINQ) + 0

sum(df$M_NINQ)
df$NINQ = NULL
summary(df)

#Var 9: CLNO
summary(df$CLNO)
df$IMP_CLNO <- df$CLNO
df$IMP_CLNO[is.na(df$CLNO)] <- 20
df$M_CLNO <- is.na(df$CLNO) + 0

sum(df$M_CLNO)
df$CLNO = NULL
summary(df)

#Var 10: DEBTINC
summary(df$DEBTINC)
df$IMP_DEBTINC <- df$DEBTINC
df$IMP_DEBTINC[is.na(df$DEBTINC)] <- 34.8183
df$M_DEBTINC <- is.na(df$DEBTINC) + 0

sum(df$M_DEBTINC)
df$DEBTINC = NULL

#Run a summary to prove that all the variables have been imputed
summary(df)

#Step 5: One Hot Encoding
#For char/category variables, perform one hot encoding. For this create a Flag for each categories.

#Char 1: REASON
table(df$REASON)
df$FLAG.Reason.DebtCon = (df$REASON == "DebtCon") + 0
df$FLAG.Reason.HomeImp = (df$REASON == "HomeImp") + 0

#Delete the original class variable
df$REASON = NULL

#Run a summary to show that the category variables have been replaced by Flag variables.
summary(df)

#Char 2: JOB
table(df$JOB)
df$FLAG.Job.Mgr = (df$JOB == "Mgr") + 0
df$FLAG.Job.Office = (df$JOB == "Office") + 0

df$FLAG.Job.Other = (df$JOB == "Other") + 0
df$FLAG.Job.ProfExe = (df$JOB == "ProfExe") + 0

df$FLAG.Job.Sales = (df$JOB == "Sales") + 0
df$FLAG.Job.Self = (df$JOB == "Self") + 0

df$FLAG.Job.Salary = (df$JOB %in% c("Self", "ProfExe", "Mgr")) + 0
df$JOB = NULL

summary(df)

write.csv(df, OUTFILE, row.names = FALSE)
