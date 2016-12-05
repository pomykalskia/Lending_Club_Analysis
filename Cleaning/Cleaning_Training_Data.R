#Cleaning file
library(readr)
library(plyr)
library(MASS)
library(car)

# https://www.lendingclub.com/info/download-data.action  <-- download data
setwd("~/Downloads")


declined <- read_csv('RejectStatsD.csv', skip = 1) # skip the first line where there's a note.
head(declined)
# proportion of missing values
sum(is.na(declined$Risk_Score)) / nrow(declined) # 82.15%
sum(is.na(declined$`Amount Requested`)) / nrow(declined) # none missing
sum(is.na(declined$`Loan Title`)) / nrow(declined) # none missing
sum(is.na(declined$`Debt-To-Income Ratio`)) / nrow(declined) # none missing
sum(is.na(declined$State)) / nrow(declined) # none missing


# read the accepted dataset
accepted <- read_csv('LoanStats3d.csv', skip = 1) # skip the first line where there's a note.


# inspect accepted dataset
# what is the intersection of attributes for these two datasets?
# amount requested, loan title,
# 1. rename columns
accepted = rename(accepted, c("loan_amnt"="Amount Requested", "purpose"="Loan Title", "dti" = "Debt-To-Income Ratio",
                              "addr_state" = "State", "emp_length" = "Employment Length", "policy_code" = "Policy Code",
                              "issue_d" = "Application Date"))
accepted <- accepted[c("Amount Requested", "Loan Title", "Debt-To-Income Ratio",
                       "State", "Employment Length", "Policy Code", "Application Date")]
declined <- declined[c("Amount Requested", "Loan Title", "Debt-To-Income Ratio",
                       "State", "Employment Length", "Policy Code", "Application Date")]

# flag denoting whether loan application was approved.
accepted$'accepted?' = 1
declined$'accepted?' = 0

# re-formatting to prepare for the concatenation.
declined$'Debt-To-Income Ratio' <- as.numeric(sub("%", "", declined$'Debt-To-Income Ratio'))

summary(factor(accepted$`Employment Length`))
summary(factor(declined$`Employment Length`))

# dealing with loan title
summary(factor(accepted$`Loan Title`))
# car        credit_card   debt_consolidation   educational          home_improvement    house     major_purchase
# 3466       102025        250020               1                    25293               1438      7449
# medical    moving        other                renewable_energy     small_business      vacation  wedding
# 3938       2420          19204                224                  3364                2249      4
# NA's
# 2
summary(factor(declined$`Loan Title`))


#' substitute a value in the loan title column of a dataframe to be another value of input.
#' @param df, The data we are querying (data.frame)
#' @param initial, The value we are re-computing for (character)
#' @param target, The substitution value (character)
#' @return A data.frame with all initial values substituted by target value.
revalue <- function(df, initial, target){
  df[df$'Loan Title' == initial,]$'Loan Title' = target
  return(df)
}
# massive renaming and cleaning.
declined <- revalue(declined, 'smmoore2', 'other')
declined <- revalue(declined, 'thad31', 'other')
declined <- revalue(declined, 'loan', 'other')
declined <- revalue(declined, 'dougie03', 'other')
declined <- revalue(declined, 'freeup', 'other')
declined <- revalue(declined, 'althea9621', 'other')
declined <- revalue(declined, '10 months away from being an RN', 'other')
declined <- revalue(declined, 'Vacation', 'vacation')
declined <- revalue(declined, 'Green loan', 'renewable_energy')
declined <- revalue(declined, 'Other', 'other')
declined <- revalue(declined, 'Small Business Expansion', 'small_business')
declined <- revalue(declined, 'Business Line Of Credit', 'small_business')
declined <- revalue(declined, 'Business Loan', 'small_business')
declined <- revalue(declined, 'Business', 'small_business')
declined <- revalue(declined, 'Business Advertising Loan', 'small_business')
declined <- revalue(declined, 'Debt consolidation', 'debt_consolidation')
declined <- revalue(declined, 'Consolidation Loan', 'debt_consolidation')
declined <- revalue(declined, 'Consolidate debt', 'debt_consolidation')
declined <- revalue(declined, 'Auto Financing', 'car')
declined <- revalue(declined, 'Car financing', 'car')
declined <- revalue(declined, 'Need a decent rate on car financing', 'car')
declined <- revalue(declined, 'Credit card refinancing', 'credit_card')
declined <- revalue(declined, 'Moving and relocation', 'moving')
declined <- revalue(declined, 'Home improvement', 'home_improvement')
declined <- revalue(declined, 'Home buying', 'house')
declined <- revalue(declined, 'Major purchase', 'major_purchase')
declined <- revalue(declined, 'Medical expenses', 'medical')

# now do the application date roundings into quarters.
class(declined$`Application Date`)
class(accepted$`Application Date`)
dates <- as.Date("Dec/2015", "%b/%Y")
require(zoo)
accepted$`Application Date` <- as.yearqtr(as.yearmon(accepted$`Application Date`, "%b-%Y"))
declined$`Application Date` <- as.yearqtr(declined$`Application Date`, "%Y-%m-%d")

# dropping missing values.
accepted <- accepted[!is.na(accepted$`Application Date`),]
