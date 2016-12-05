library(readr)
library(plyr)
library(MASS)
library(car)
setwd("~/Downloads")
# read in data for 2016 Q1-Q3
dec_16_Q1 <- read_csv('RejectStats_2016Q1.csv', skip = 1)
dec_16_Q2 <- read_csv('RejectStats_2016Q2.csv', skip = 1)
dec_16_Q3 <- read_csv('RejectStats_2016Q3.csv', skip = 1)
acc_16_Q1 <- read_csv('LoanStats_2016Q1.csv', skip = 1)
acc_16_Q2 <- read_csv('LoanStats_2016Q2.csv', skip = 1)
acc_16_Q3 <- read_csv('LoanStats_2016Q3.csv', skip = 1)

# binding data to generate comprehensive whole datasets.
dec_16 <- rbind(rbind(dec_16_Q1,dec_16_Q2),dec_16_Q3)
acc_16 <- rbind(rbind(acc_16_Q1,acc_16_Q2),acc_16_Q3)

sum(is.na(dec_16$`Amount Requested`)) / nrow(dec_16) # none missing
sum(is.na(dec_16$`Loan Title`)) / nrow(dec_16) # 110 missing values
sum(is.na(dec_16$`Debt-To-Income Ratio`)) / nrow(dec_16) # none missing
sum(is.na(dec_16$State)) / nrow(dec_16) # one missing

acc_16 = rename(acc_16, c("loan_amnt"="Amount Requested", "purpose"="Loan Title", "dti" = "Debt-To-Income Ratio", "addr_state" = "State", "emp_length" = "Employment Length", "policy_code" = "Policy Code", "issue_d" = "Application Date"))
# select only features that we're interested in. 
acc_16 <- acc_16[c("Amount Requested", "Loan Title", "Debt-To-Income Ratio", 
                   "State", "Employment Length", "Policy Code", "Application Date")]
dec_16 <- dec_16[c("Amount Requested", "Loan Title", "Debt-To-Income Ratio", 
                   "State", "Employment Length", "Policy Code", "Application Date")]

# flag denoting whether loan application was approved.
acc_16$'accepted?' = 1
dec_16$'accepted?' = 0

# re-formatting
dec_16$'Debt-To-Income Ratio' <- as.numeric(sub("%", "", dec_16$'Debt-To-Income Ratio'))

summary(factor(acc_16$`Employment Length`))
summary(factor(dec_16$`Employment Length`))

#' substitute a value in the loan title column of a dataframe to be another value of input. 
#' @param df, The data we are querying (data.frame)
#' @param initial, The value we are re-computing for (character)
#' @param target, The substitution value (character)
#' @return A data.frame with all initial values substituted by target value.
revalue <- function(df, initial, target){
  filter <- df[,'Loan Title'] == initial & !is.na(df[,'Loan Title'])
  df[filter,]$'Loan Title' = target
  return(df)
}

summary(factor(acc_16$`Loan Title`))
#  car        credit_card debt_consolidation   home_improvement              house 
# 3651              71079             189152              23783               1528 
# major_purchase            medical             moving              other   renewable_energy 
#           7937               3971               2458              21087                207 
# small_business           vacation            wedding 
#           3574               2433                  1 

# there was educational in 2015 data, where there isn't educational in 2016 data

summary(factor(dec_16$`Loan Title`))
# Business           Business Loan                     car           Car financing 
#     7490                   60971                  118641                    4629 
# Credit card refinancing             credit_card      Debt consolidation      debt_consolidation 
#                   36226                  357657                  125345                 1502492 
# Green loan             Home buying        Home improvement        home_improvement 
#        462                    2014                   14795                  181890 
# house          Major purchase          major_purchase                 medical 
# 50125                    6773                  123028                   85114 
# Medical expenses                  moving   Moving and relocation                   other 
#             4830                   84396                    4131                  453321 
# Other        renewable_energy          small_business                vacation 
# 21294                    7887                   64226                   44979 
# Vacation                 wedding                    NA's 
#     2552                       6                     110 

# massive renaming:
dec_16 <- revalue(dec_16, 'Other', 'other')
dec_16 <- revalue(dec_16, 'Vacation', 'vacation')
dec_16 <- revalue(dec_16, 'Business Loan', 'small_business')
dec_16 <- revalue(dec_16, 'Business', 'small_business')
dec_16 <- revalue(dec_16, 'Green loan', 'renewable_energy')
dec_16 <- revalue(dec_16, 'Debt consolidation', 'debt_consolidation')
dec_16 <- revalue(dec_16, 'Home improvement', 'home_improvement')
dec_16 <- revalue(dec_16, 'Medical expenses', 'medical')
dec_16 <- revalue(dec_16, 'Major purchase', 'major_purchase')
dec_16 <- revalue(dec_16, 'Home buying', 'house')
dec_16 <- revalue(dec_16, 'Credit card refinancing', 'credit_card')
dec_16 <- revalue(dec_16, 'Moving and relocation', 'moving')
dec_16 <- revalue(dec_16, 'Car financing', 'car')

summary(factor(dec_16$`Loan Title`))
#    car        credit_card debt_consolidation   home_improvement              house 
# 123270             393883            1627837             196685              52139 
# major_purchase            medical             moving              other   renewable_energy 
#         129801              89944              88527             474615               8349 
# small_business           vacation            wedding               NA's 
#         132687              47531                  6                110 

# formatting date into quarters
require(zoo)
acc_16$`Application Date` <- as.yearqtr(as.yearmon(acc_16$`Application Date`, "%b-%Y"))
dec_16$`Application Date` <- as.yearqtr(dec_16$`Application Date`, "%Y-%m-%d")

# drop missing values:
acc_16 <- acc_16[!is.na(acc_16$`Application Date`),]

# concatenation of the two dataframes. 
loan_16 <- rbind(acc_16, dec_16)
colnames(loan_16) <- c('amt_request', 'title', 'dti', 'state', 'emp_length', 'pol_code', 'date', 'result')
filter <- loan_16[,'title'] == 'wedding' & !is.na(loan_16[,'title'])
loan_16[filter,]$'title' = 'other'

#### loan_16 is the cleaned, concatenated comprehensive dataset ####





