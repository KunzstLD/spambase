#### using Neuralnet package ####
nn <- neuralnet(
  spam ~.,
  data = train,
  hidden = c(3, 3),
  act.fct = "logistic",
  linear.output = FALSE,
  stepmax = 1e6
)
plot(nn)
# predict on test data
pred <- predict(nn, test[, -c("spam")])
# classify as spam or not spam depending on probability
pred <- ifelse(pred > 0.5, 1, 0)
# ~ 7 % classification error
caret::confusionMatrix(as.factor(test$spam), as.factor(pred))