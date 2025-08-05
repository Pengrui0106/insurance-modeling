library(tidyverse)

PATH = "/Users/raypeng/Documents/IS 5213 Data science and big data/HMEQ_Scrubbed"
FILE_NAME = "HMEQ_Scrubbed.csv"

INFILE = paste(PATH, FILE_NAME, sep = "/")

setwd(PATH)
df = read.csv(FILE_NAME)

str(df)
summary(df)
head(df)

#Chart 1
#Defaulted loans are about 1/5 among all the data.
df %>%
  ggplot( aes(x = factor(TARGET_BAD_FLAG)) )+
  geom_bar( fill = c("lightblue", "lightpink") ) +
  labs(title = "Chart-1: Default Loans Count", 
       x = "Defaulted or not (1 = Yes)", 
       y = "Count")


#Chart 2
#Loss amounts median and mean are around 10,000 dollars.
df %>%
  filter(TARGET_BAD_FLAG == 1) %>% 
  ggplot( aes(x = TARGET_LOSS_AMT) )+
  geom_histogram( fill = "darkblue", col = "lightblue", bins = 30 ) +
  labs(title = "Chart 2: Loss Amounts on Defaulted Loans",
       x = "Loss Amount",
       y = "Count")

#Chart 3
#People with less job experience tend to have bad loans.
#Higher debt-inocme ration may cause larger amounts of defaulted loan.
df %>%
  filter( TARGET_BAD_FLAG == 1 & M_DEBTINC == 0 ) %>%
  ggplot( aes(IMP_DEBTINC, IMP_YOJ))+
  geom_point(alpha = 0.5, 
             aes(size = TARGET_LOSS_AMT))+
  theme_bw()+
  labs(title = "Chart 3: Debt income ratio, Year of job and Loss amount",
       x = "Debt income ratio",
       y = "Year of job")

#Chart 4
#People with less loan amount tend to default.
df %>%
  ggplot( aes(x = IMP_VALUE,
              y = LOAN,
              color = factor(TARGET_BAD_FLAG)))+
  geom_point(alpha = 0.5)+
  scale_color_manual(values = c("0" = "darkgreen", "1" = "red")) +
  labs(title = "Chart 4: Loan vs House Value",
       color = "Defaulted",
       x = "House Value (dollars)",
       y = "Loan amount")

#Chart 5
#People with higher Debt-income ratio tend to default. 
df %>% 
  filter(M_DEBTINC == 0) %>%
  ggplot( aes(x = IMP_DEBTINC,
              fill = factor(TARGET_BAD_FLAG)) )+
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"))+
  labs(title = "Chart 5: Debt to Income Ratio vs Default",
       x = "Debt to Income Ratio",
       y = "Density",
       fill = "Defaulted Red Flag")

#Chart 6
#People with shorter credit line age tend to default on loans.
df %>%
  filter(M_CLAGE == 0) %>%
  ggplot( aes(x = IMP_CLAGE,
              fill = factor(TARGET_BAD_FLAG))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"))+
  labs(title = "Chart 6: Credit History Length vs Default",
       x = "Credit line age (months)",
       y = "Density",
       fill = "Defaulted Red Flag")

#Chart 7
#In this dataset, if the person's house value and debt-income ratio data is missing, then
#this person may default on the loan.

#Chart 8
#In those people whose debt-income ratio data is missing,
#over half of them will default on the loan. 
miss_cols = c("M_MORTDUE", "M_VALUE", "M_YOJ", "M_DEROG", "M_DELINQ",
               "M_CLAGE", "M_NINQ", "M_CLNO", "M_DEBTINC")
df_miss = df %>%
  select(TARGET_BAD_FLAG, all_of(miss_cols)) %>%
  pivot_longer(-TARGET_BAD_FLAG, names_to = "Feature", values_to = "Missing") %>%
  filter(Missing == 1)

p1 = ggplot(df_miss, aes(x = Feature,
                    fill = factor(TARGET_BAD_FLAG)))+
  geom_bar(position = "fill")+
#  geom_bar(alpha = 0.8)+
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"))+
  labs(title = "Chart 7: Default Proportion by Missing Data Flag",
       y = "Count",
       fill = "Defaulted")+
  coord_flip()

print(p1)

p2 = ggplot(df_miss, aes(x = Feature,
                         fill = factor(TARGET_BAD_FLAG)))+
#  geom_bar(position = "fill")+
  geom_bar(alpha = 0.8)+
  scale_fill_manual(values = c("0" = "lightblue", "1" = "red"))+
  labs(title = "Chart 8: Default Count by Missing Data Feature",
       y = "Count",
       fill = "Defaulted")+
  coord_flip()

print(p2)

