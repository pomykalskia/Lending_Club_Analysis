## STAT6021 final project
## do you get a loan or not?
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

# concatenation of the two dataframes.
loan <- rbind(accepted, declined)
colnames(loan) <- c('amt_request', 'title', 'dti', 'state', 'emp_length', 'pol_code', 'date', 'result')
loan[loan$title == 'educational',]$title = 'other'
loan[loan$title == 'wedding',]$title = 'other'

#defining ref groups
loan$title<-as.factor(loan$title)
loan$emp_length<-as.factor(loan$emp_length)
loan$title <- relevel(loan$title, "other")
loan$emp_length <- relevel(loan$emp_length, "n/a")

# logistic regression building
log.fit = glm(result ~ ., data = loan, family="binomial")#state insignificant
summary(log.fit)
log.fit.2 = glm(result ~ .-state, data = loan, family="binomial")#state and date insignificant 

#final model
log.fit.3 = glm(result ~ .-state -date -pol_code, data = loan, family="binomial")
summary(log.fit.3)

#testing multicollinearity.
vif(log.fit.3)

#                 GVIF Df GVIF^(1/(2*Df))
# amt_request 1.104458  1        1.050932
# title       1.095492 13        1.003514
# dti         1.044612  1        1.022062
# emp_length  1.083903 11        1.003669

# no multicollinearity. our variables are good.

# k-fold CV
cv.lm(data=loan, form.lm=log.fit.3, m=5, plotit=F)
cv.binary(log.fit.3, nfolds = 5)
summary(factor(loan$title))
# Internal estimate of accuracy = 0.936
# Cross-validation estimate of accuracy = 0.936


# odds ratios
exp(coef(log.fit.3))



#testing on 2016 
# Predict loan acceptance using data from Q1, Q2, and Q3 from 2016

predict_loan_16 <- loan_16[c("amt_request", "title", "dti", "state", "emp_length", "pol_code", "date")]

predict1 <- predict(log.fit.3, data =predict_loan_16 , type="response")


ROC1 <- roc(loan_16$result[1:3280474], predict1)
plot(ROC1, col = "blue", main = "ROC Curve")


# Confusion matrix

predict1final <- as.data.frame(predict1)

# chose cut-off probability score of 50% to determine whether response was a 0 or 1
predict1final$predict1[predict1final$predict1 < .5] <- 0
predict1final$predict1[predict1final$predict1 > .5] <- 1

conf <- confusionMatrix(predict1final$predict1, loan_16$result[1:3280474])
conf


