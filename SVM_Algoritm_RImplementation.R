letters<-read.csv("/Users/adityabhat/Desktop/Machine Learning with R/chapter 7/letterdata.csv")
letter_train<-letters[1:16000,]
letter_test<-letters[16001:20000,]
library(kernlab)
#training the classifier 
letter_classifier<-ksvm(letter~.,data=letter_train,kernel="vanilladot")
letter_classifier
#To make predictions 
letter_predictions <- predict(letter_classifier, letter_test)
#To obtain a matrix indicating the the number of letters predicted by comparing
#The values obtained from predction and test data
table(letter_predictions, letter_test$letter)
agreement <- letter_predictions == letter_test$letter
#to test the accuracy of classification
table(agreement)
#To show the percentage accuracy
prop.table(table(agreement))

#Using rbf kernel to improve the accuracy
letter_classifier_rbf <- ksvm(letter ~ ., data = letter_train,kernel = "rbfdot")
letter_predictions_rbf<-predict(letter_classifier,letter_test)

agreement_rbf<-letter_predictions_rbf == letter_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
