#Step 1: Read in the Data
#Read the data into R
PATH = "/Users/raypeng/Documents/IS 5213 Data science and big data/Insurance"

FILE_NAME <- "Insurance.csv"
OUT_NAME <- "Insurance_Scrubbed.csv"

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
group_var <- df$TARGET_CLM_FLAG
numeric_vars <- c("KIDSDRIV", "AGE", "HOMEKIDS", "YOJ", "INCOME", "HOME_VAL",
                  "TRAVTIME", "BLUEBOOK", "TIF", "NPOLICY", "OLDCLAIM", "CLM_FREQ",
                  "MVR_PTS", "CAR_AGE")
#par(mfrow = c(4, 4), oma = c(0, 0, 2, 0))

for (var in numeric_vars) {
  boxplot(
    df[[var]] ~ group_var,
#The MAIN TITLE of the box plot should be set to your name
    main = "Rui Peng",
    xlab = "Claim Status",
    ylab = var,
    
#Add color to the boxes
    col = c("lightblue", "lightpink")
  )
}


#par(mfrow = c(1, 1))


#Comment on whether or not there are any observable differences in the box plots between the two groups.
#So according to my observation, those people tend to claim the car insurance: 
#younger age, more home kids, less income and less home value
#who claimed before and have claimed many times, who had more traffic tickets before

#Step 3: Histograms
#Plot a histogram of at least one of the numeric variables
par(mfrow = c(1, 2))

hist(
  df$TRAVTIME,
#Manually set the number of breaks to a value that makes sense
  breaks = 30,
  col = "lightblue", 
  border = "white",
  xlab = "Travel time (minutes)",
  ylab = "Density",
  main = "Histogram of Travel Time",
  freq = FALSE
)

#Superimpose a density line to the graph
lines(
  density(df$TRAVTIME, bw = 5),
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

hist(
  df$BLUEBOOK,
  breaks = 25,
  col = "lightblue", 
  border = "white",
  xlab = "Bluebook value of the car (dollars)",
  ylab = "Density",
  main = "Histogram of Bluebook",
  freq = FALSE
)

lines(
  density(df$BLUEBOOK, bw = 1000),
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

par(mfrow = c(1, 1))

#Step 4: Impute "Fix" all the numeric variables that have missing values
#For the missing Target variables, simply set the missing values to zero

#Var 1: TARGET_CLM_AMT
df$TARGET_CLM_AMT[is.na(df$TARGET_CLM_AMT)] = 0

#Var 2: AGE
summary(df$AGE)

#The median or mean value will be useful in most cases.
median(df$AGE, na.rm = TRUE)

#Create two new variables: #One variable beginning with IMP_ and the second value beginning with M_.
df$IMP_AGE <- df$AGE
df$IMP_AGE[ is.na(df$AGE) ] = 45
df$M_AGE = is.na(df$AGE) + 0

#Compute a sum for all the M_ variables
sum(df$M_AGE)
summary(df)

#Delete the original variable after it has been imputed.
df$AGE = NULL
summary(df)

#Var 3: YOJ
df$IMP_YOJ <- df$YOJ
df$IMP_YOJ[ is.na(df$YOJ) ] = 11
df$M_YOJ = is.na(df$YOJ) + 0

df$YOJ = NULL
sum (df$M_YOJ)

#Var 4: INCOME
#INCOME solution 1 - zero dollars
df$IMP_INCOME <- df$INCOME
df$IMP_INCOME[ is.na(df$INCOME) ] = 0
df$M_INCOME = is.na(df$INCOME) + 0
head(df$INCOME, 20)

#INCOME solution 2 - simple
df$IMP_INCOME <- df$INCOME
df$IMP_INCOME[ is.na(df$INCOME) ] = 53529
df$M_INCOME = is.na(df$INCOME) + 0

#Try one complex imputation like the one described in the lectures.
#INCOME solution 3 - complex
a = aggregate(x=df$INCOME, by=list(df$JOB), na.rm = TRUE, FUN = median)
a = a[ order(a$x, decreasing = TRUE), ]

df$IMP_INCOME = df$INCOME
df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Doctor")] = 121398
df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Lawyer")] = 83230

df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Manager")] = 78589
df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Professional")] = 71230

df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Blue Collar")] = 53694
df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Clerical")] = 30800

df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Home Maker")] = 776
df$IMP_INCOME[ is.na(df$INCOME) & (df$JOB == "Student")] = 360

df$IMP_INCOME[ is.na(df$IMP_INCOME)] = 109953
df$M_INCOME = is.na(df$INCOME) + 0
sum(df$M_INCOME)

df$INCOME = NULL
summary(df)

#Var 5: HOME_VAL
a = aggregate(x=df$HOME_VAL, by=list(df$JOB), na.rm = TRUE, FUN = median)
a = a[ order(a$x, decreasing = TRUE), ]

df$IMP_HOME_VAL = df$HOME_VAL
df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Doctor")] = 282708
df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Lawyer")] = 229325

df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Manager")] = 221902
df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Professional")] = 211874

df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Blue Collar")] = 176831
df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Clerical")] = 13363

df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Home Maker")] = 92207
df$IMP_HOME_VAL[ is.na(df$HOME_VAL) & (df$JOB == "Student")] = 0

df$IMP_HOME_VAL[ is.na(df$IMP_HOME_VAL)] = 240261
df$M_HOME_VAL = is.na(df$HOME_VAL) + 0
sum(df$M_HOME_VAL)

df$HOME_VAL = NULL
summary(df)

#Var 6: CAR_AGE
median(df$CAR_AGE, na.rm = TRUE)
df$IMP_CAR_AGE <- df$CAR_AGE
df$IMP_CAR_AGE[ is.na(df$CAR_AGE) ] = 8
df$M_CAR_AGE = is.na( df$CAR_AGE ) + 0
sum (df$M_CAR_AGE)

df$CAR_AGE = NULL
#Run a summary to prove that all the variables have been imputed
summary(df)


#Step 5: One Hot Encoding
#For char/category variables, perform one hot encoding. For this create a Flag for each categories.

#Char 1: Parent1
df$FLAG.SingleParent = (df$PARENT1 == "Yes") + 0
sum(df$FLAG.SingleParent)

#Delete the original class variable
df$PARENT1 = NULL

#Run a summary to show that the category variables have been replaced by Flag variables.
summary(df)

#Char 2: MSTATUS
table(df$MSTATUS)
df$FLAG.Married = (df$MSTATUS == "Yes") + 0
sum(df$FLAG.Married)
df$MSTATUS = NULL
summary(df)

#Char 3: SEX
table(df$SEX)
df$FLAG.Male = (df$SEX == "M") + 0
sum(df$FLAG.Male)
df$SEX = NULL
summary(df)

#Char 4: EDUCATION
table(df$EDUCATION)
df$FLAG.EDU.d_HS = (df$EDUCATION %in% c("a_PhD", "b_Masters", "c_Bachelors", "d_High School")) + 0
df$FLAG.EDU.c_BS = (df$EDUCATION %in% c("a_PhD", "b_Masters", "c_Bachelors")) + 0
df$FLAG.EDU.b_MS = (df$EDUCATION %in% c("a_PhD", "b_Masters")) + 0
df$FLAG.EDU.a_PhD = (df$EDUCATION %in% c("a_PhD")) + 0

sum(df$FLAG.EDU.d_HS)
sum(df$FLAG.EDU.c_BS)
sum(df$FLAG.EDU.b_MS)
sum(df$FLAG.EDU.a_PhD)

df$EDUCATION = NULL
head(df)

#Char 5: JOB
table(df$JOB)
df$FLAG.Job.BlueCollar = (df$JOB == "Blue Collar") + 0
df$FLAG.Job.Clerical = (df$JOB == "Clerical") + 0

df$FLAG.Job.Doctor = (df$JOB == "Doctor") + 0
df$FLAG.Job.HomeMaker = (df$JOB == "Home Maker") + 0

df$FLAG.Job.Lawyer = (df$JOB == "Lawyer") + 0
df$FLAG.Job.Manager = (df$JOB == "Manager") + 0

df$FLAG.Job.Professional = (df$JOB == "Professional") + 0
df$FLAG.Job.Student = (df$JOB == "Student") + 0

df$FLAG.Job.Salary = (df$JOB %in% c("Doctor", "Lawyer", "Manager", "Professional")) + 0

sum(df$FLAG.Job.BlueCollar)
sum(df$FLAG.Job.Clerical)

sum(df$FLAG.Job.Doctor)
sum(df$FLAG.Job.HomeMaker)

sum(df$FLAG.Job.Lawyer)
sum(df$FLAG.Job.Manager)

sum(df$FLAG.Job.Professional)
sum(df$FLAG.Job.Student)
sum(df$FLAG.Job.Salary)

df$JOB = NULL

#Char 6: CAR_USE
table(df$CAR_USE)
df$FLAG.CommercialUse = (df$CAR_USE == "Commercial") + 0
sum(df$FLAG.CommercialUse)
df$CAR_USE = NULL
summary(df)

#Char 7: CAR_TYPE
table(df$CAR_TYPE)

df$FLAG.CAR.Minivan = (df$CAR_TYPE == "Minivan") + 0
df$FLAG.CAR.PanelTruck = (df$CAR_TYPE == "Panel Truck") + 0

df$FLAG.CAR.Pickup = (df$CAR_TYPE == "Pickup") + 0
df$FLAG.CAR.SportsCar = (df$CAR_TYPE == "Sports Car") + 0

df$FLAG.CAR.SUV = (df$CAR_TYPE == "SUV") + 0
df$FLAG.CAR.Van = (df$CAR_TYPE == "Van") + 0

sum(df$FLAG.CAR.Minivan)
sum(df$FLAG.CAR.PanelTruck)
sum(df$FLAG.CAR.Pickup)
sum(df$FLAG.CAR.SportsCar)
sum(df$FLAG.CAR.SUV)
sum(df$FLAG.CAR.Van)

df$CAR_TYPE = NULL
summary(df)

#Char 8: RED_CAR
table(df$RED_CAR)
df$FLAG.RedCar = (df$RED_CAR == "yes") + 0
sum(df$FLAG.RedCar)
df$RED_CAR = NULL
summary(df)

#Char 9: REVOKED
df$FLAG.Revoked = (df$REVOKED == "Yes") + 0
sum(df$FLAG.Revoked)
df$REVOKED = NULL
summary(df)

#Char 10: URBANICITY
table(df$URBANICITY)
df$FLAG.Urban = (df$URBANICITY == "Highly Urban/ Urban") + 0
sum(df$FLAG.Urban)
df$URBANICITY = NULL
summary(df)

write.csv(df, OUTFILE, row.names = FALSE)
