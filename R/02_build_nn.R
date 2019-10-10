# TODO:
# 1) different starting values; 
# 2) test different number of hidden layers: 2, 3, 5
# 3) Experiment with hidden units
# 4) weight decay vs no weight decay
# Next date: 18.10.2019/4 pm 

# ?What is: Loss, Accuaracy
# Loss: calculated during model process. Summation of errors for each
 # sample during the trainin process. Uses either negative log-likelihood (classification)
 # or reisdual sum of squares (regression). Implies how well a model behaves
 # after each iteration (epoch). 
# Accurracy: after model parameters are learned test data is fed to 
 # model: number of mistakes are recoreded and percentage of misclassification
 # is calculated

# create test & training dataset
index <- sample(1:nrow(df_spam), round(0.75 * nrow(df_spam)))
train <- df_spam[index, ]
test <- df_spam[-index, ]

# create labels vector
c(train, train_labels) %<-% list(train[, - c("spam")], train$spam)
c(test, test_labels) %<-% list(test[, - c("spam")], test$spam)

# Normalize training data
# centering by subtracting mean and dividing by sd
train <- scale(train, center = TRUE, scale = TRUE)

# Use means and standard deviations from training set to normalize test set
col_means_train <- attr(train, "scaled:center")
col_std_train <- attr(train, "scaled:scale")
test <- scale(test, center = col_means_train, scale = col_std_train)

# a simple model
build_model <- function() {
  # hidden units typically between 5 and 100
  model <- keras_model_sequential() %>%
    layer_dense(units = 29, activation = "relu",
                input_shape = dim(train)[2]) %>%
    layer_dense(units = 29, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

  model %>% compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = list("accuracy")
  )
  model
}
model <- build_model()
model %>% summary()

# training
# model %>% fit(train, train_labels, epochs = 5)

# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)
epochs <- 500

# Fit the model and store training stats
# The model is stopped when no improvement can be seen. Thereby 
# the patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

# run model with early stopping
model <- build_model()
history <- model %>% fit(
  train,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)
plot(history, metrics = "accuracy", smooth = FALSE) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0.5, 1)) +
  theme_bw()

# Accuracy on training set 
history$metrics$accuracy %>% summary()

# Evaluate model on test data
c(loss, accuracy) %<-% (model %>% evaluate(test, test_labels, verbose = 0))
accuracy

# Predictions
predictions <- model %>% predict(test)
# same as evaluation, just a little bit more detailed
predictions <- data.table(pred = as.factor(ifelse(predictions > 0.5, 1, 0)),
                          true_val = as.factor(test_labels))
caret::confusionMatrix(predictions$true_val,
                       predictions$pred)
# !here one could investigate further which predictions have been wrong

#### Using Weight decay ####
weight_decay_model <- function() {
  # hidden units typically between 5 and 100
  model_wd <- keras_model_sequential() %>%
    layer_dense(units = 29,
                activation = "relu",
                input_shape = dim(train)[2],
                kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    layer_dense(units = 29,
                activation = "relu",
                kernel_regularizer = regularizer_l2(l = 0.01)) %>%
    layer_dense(units = 1,
                activation = "sigmoid")

  model_wd %>% compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = list("accuracy")
  )
  model_wd
}
model_wd <- weight_decay_model()
# model_wd %>% summary()

# training
history_wd <- model_wd %>% fit(
  train,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

# plot loss 
data.table(baseline_loss_train = history$metrics$loss,
           baseline_loss_val = history$metrics$val_loss,
           wd_loss_train = history_wd$metrics$loss[1:38],
           wd_loss_val = history_wd$metrics$val_loss[1:38],
           Epoch = 1:38) %>%
           melt(., id.vars = "Epoch", value.name = "loss") %>%
           ggplot(., aes(x = Epoch, y = loss, color = as.factor(variable))) +
                 geom_line(size = 1.5) +
                 theme_bw()
plot(history_wd)
summary(history_wd$metrics$val_accuracy)

#### using Neuralnet package ####
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
