# set ggplot theme
theme_set(theme_light(base_size = 15, base_family = "Poppins"))

# compare loss  
data.table(baseline_loss_train = history$metrics$loss,
           baseline_loss_val = history$metrics$val_loss,
           wd_loss_train = history_wd$metrics$loss[1:45],
           wd_loss_val = history_wd$metrics$val_loss[1:45],
           dropout_loss_train = history_dropout$metrics$loss[1:45],
           dropout_loss_val = history_dropout$metrics$val_loss[1:45],
           Epoch = 1:45) %>%
  melt(., id.vars = "Epoch", value.name = "loss") %>%
  ggplot(., aes(x = Epoch, y = loss, color = as.factor(variable))) +
  geom_line(size = 0.8) +
  # annotate("text", x = 4, y = 1, family = "Poppins", size = 2.7, color = "gray20",
  #          label = "wd_loss_train")+
  # annotate("text", x = 10, y = 0.09, family = "Poppins", size = 2.7, color = "gray20",
  #          label = "baseline_loss_val")+
  # annotate("text", x = 4, y = 1, family = "Poppins", size = 2.7, color = "gray20",
  #          label = "wd_loss_train")+
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "Roboto Mono", size = 10)
        )

# compare loss  
data.table(wd_loss_train = history_wd$metrics$loss[1:81],
           wd_loss_val = history_wd$metrics$val_loss[1:81],
           dropout_loss_train = history_dropout$metrics$loss[1:81],
           dropout_loss_val = history_dropout$metrics$val_loss[1:81],
           Epoch = 1:81) %>%
  melt(., id.vars = "Epoch", value.name = "loss") %>%
  ggplot(., aes(x = Epoch, y = loss, color = as.factor(variable))) +
  geom_line(size = 0.8) +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(family = "Roboto Mono", size = 10)
  )
