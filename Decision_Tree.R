credit<-read.csv("/Users/adityabhat/Desktop/Machine Learning with R/chapter 5/credit.csv")
str(credit)
#random sampling so that the training data cntains both large and small loan amts
#as the loan amt is sorted in ascending order
set.seed(123)
train_sample<-sample(1000,900)
credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]
#testing the random sample
prop.table(table(credit_test$default))
prop.table(table(credit_train$default))

credit_model<-C5.0(credit_train[-17],credit_train$default)
summary(credit_model)
credit_pred<-predict(credit_model,credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

credit_boost10<-C5.0(credit_train[-17],credit_train$default,trials = 10)
summary(credit_boost10)
credit_boost10_pred<-predict(credit_boost10,credit_test)
CrossTable(credit_test$default, credit_boost10_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#adding penalizers
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,dimnames = matrix_dimensions)
credit_cost <- C5.0(credit_train[-17], credit_train$default,costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))