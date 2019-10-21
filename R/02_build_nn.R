# TODO:
# KFold cross validation
# Next date: 

# ?What is: Loss, Accuaracy
# Loss: calculated during model process. Summation of errors for each
 # sample during the training process. Uses either negative log-likelihood (classification)
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

# ---------------------------------------------------------------------------------
#### build a simple model ####
# ---------------------------------------------------------------------------------

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
# model %>% summary()

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

# plot Accuracy & loss
gridExtra::grid.arrange(
  plot(history, metrics = "accuracy", smooth = FALSE) +
    coord_cartesian(xlim = c(0, history$metrics$loss %>% length()), 
                    ylim = c(0.5, 1)) +
    ggtitle("Accuracy & Loss in base model") +
    theme_bw(),
  plot(history, metrics = "loss", smooth = FALSE) +
    coord_cartesian(xlim = c(0, history$metrics$loss %>% length()), 
                    ylim = c(0, 1)) +
    theme_bw(),
  ncol = 1
)

# Accuracy on training set 
history$metrics$accuracy %>% summary()

# Evaluate model on test data
c(loss, accuracy) %<-% (model %>% evaluate(test, test_labels, verbose = 0))
# accuracy
# loss

# Predictions
# same as evaluation, just a little bit more detailed
predictions <- model %>% predict(test)
predictions <- data.table(pred = as.factor(ifelse(predictions > 0.5, 1, 0)),
                          true_val = as.factor(test_labels))
caret::confusionMatrix(predictions$true_val,
                       predictions$pred)
# !here one could investigate further which predictions have been wrong

# ---------------------------------------------------------------------------------
#### Weight decay #### 
# ---------------------------------------------------------------------------------
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

# validation & training set have almost same accuracy in the end
# validation better accuarcy?
gridExtra::grid.arrange(
  plot(history_wd, metrics = "accuracy", smooth = FALSE) +
    coord_cartesian(xlim = c(0, history_wd$metrics$loss %>% length()), 
                    ylim = c(0.5, 1)) +
    ggtitle("Accuracy & Loss in weight decay model") +
    theme_bw(),
  plot(history_wd, metrics = "loss", smooth = FALSE) +
    coord_cartesian(xlim = c(0, history_wd$metrics$loss %>% length()), 
                    ylim = c(0, 1)) +
    theme_bw(),
  ncol = 1
)
# accuracy training & validation set ~ 96 %
history_wd$metrics$accuracy %>% summary()
history_wd$metrics$val_accuracy %>% summary()

# Predictions
predictions <- model_wd %>% predict(test)
predictions <- data.table(pred = as.factor(ifelse(predictions > 0.5, 1, 0)),
                          true_val = as.factor(test_labels))
caret::confusionMatrix(predictions$true_val,
                       predictions$pred)

# ---------------------------------------------------------------------------------
#### Dropout ####
# ---------------------------------------------------------------------------------
dropout_model <- function() {
  # hidden units typically between 5 and 100
  model_drop <- keras_model_sequential() %>%
    layer_dense(units = 29,
                activation = "relu",
                input_shape = dim(train)[2]) %>%
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 29,
                activation = "relu") %>%
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1,
                activation = "sigmoid")
  
  model_drop %>% compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = list("accuracy")
  )
  model_drop
}
model_drop <- dropout_model()

# training
history_dropout <- model_drop %>% fit(
  train,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

# validation set has higher accuracy than training?
gridExtra::grid.arrange(
  plot(history_dropout, metrics = "accuracy", smooth = FALSE) +
    coord_cartesian(xlim = c(0, history_dropout$metrics$loss %>% length()), 
                    ylim = c(0.5, 1)) +
    ggtitle("Accuracy & Loss in dropout model") +
    theme_bw(),
  plot(history_dropout, metrics = "loss", smooth = FALSE) +
    coord_cartesian(xlim = c(0, history_dropout$metrics$loss %>% length()), 
                    ylim = c(0, 1)) +
    theme_bw(),
  ncol = 1
)

# accuracy training & validation set ~ 95%
history_dropout$metrics$accuracy %>% summary()
history_dropout$metrics$val_accuracy %>% summary()

# Predaictions
predictions <- model_drop %>% predict(test)
predictions <- data.table(pred = as.factor(ifelse(predictions > 0.5, 1, 0)),
                          true_val = as.factor(test_labels))
caret::confusionMatrix(predictions$true_val,
                       predictions$pred)

