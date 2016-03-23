#Artificial neural networks
concrete <-read.csv("/Users/adityabhat/Desktop/Machine Learning with R/chapter 7/concrete.csv")

str(concrete)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#normalizing the values to increase the strength of ANN
concrete_norm <- as.data.frame(lapply(concrete, normalize))
concrete_train <- concrete_norm[1:773, ]
concrete_test<-concrete_norm[774:1030,]

#Training the model using neuralnet package
concrete_model <- neuralnet(strength ~ cement + slag
                            + ash + water + superplastic + coarseagg + fineagg + age,
                            data = concrete_train)
plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)

#Increasing the strength of the model by increasing the hidden layers
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)
plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
 predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
