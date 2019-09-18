# create neural network
# TODO improve algorithmn, 
# does not converge in 1 of 1 repetition(s) within the stepmax sometimes
nn <- neuralnet(
  spam ~.,
  data = train,
  hidden = c(3, 3),
  act.fct = "logistic",
  linear.output = FALSE
)

# predict on test data
pred <- predict(nn, test[, -c("spam")])


# classify as spam or not spam depending on probability
pred <- ifelse(pred > 0.5, 1, 0)
# ~ 7 % classification error
caret::confusionMatrix(as.factor(test$spam), as.factor(pred))
