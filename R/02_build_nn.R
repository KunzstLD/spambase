# create test & training dataset
index <- sample(1:nrow(df_spam),round(0.75*nrow(df_spam)))
train <- df_spam[index,]
test <- df_spam[-index,]

# create neural network
# TODO Keras Model for spam data
# 1) different starting values; 
# 2) test different number of hidden layers: 2, 3, 5
# 3) Experiment with hidden units
# 4) weight decay vs no weight decay
# Think about standardization!
# Next meeting: 07.10 / 16 Uhr

# using Neuralnet package
# nn <- neuralnet(
#   spam ~.,
#   data = train,
#   hidden = c(3, 3),
#   act.fct = "logistic",
#   linear.output = FALSE,
#   stepmax = 1e6
# )
# plot(nn)

# # predict on test data
# pred <- predict(nn, test[, -c("spam")])

# # classify as spam or not spam depending on probability
# pred <- ifelse(pred > 0.5, 1, 0)
# # ~ 7 % classification error
# caret::confusionMatrix(as.factor(test$spam), as.factor(pred))