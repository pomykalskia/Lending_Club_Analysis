## Analysis
## do you get a loan or not?

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


