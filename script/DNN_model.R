library(keras)

# Only for test use
Final_Table = Final_Table[Final_Table$test_time>="2017-06-01",]
Final_Table_observe = Final_Table[Final_Table$test_time>="2017-06-01",]
data_train = Final_Table[Final_Table$test_time<="2017-10-01",]
data_train_observe = Final_Table_observe[Final_Table_observe$test_time<="2017-10-01",]

data_test = Final_Table[Final_Table$test_time=="2017-10-17",]
data_test_observe = Final_Table_observe[Final_Table_observe$test_time=="2017-10-17",]


X_train = data_train[,-((ncol(data_train)-2):ncol(data_train))]
y_train = data_train$Recomm_Level
X_test  = data_test[,-((ncol(data_train)-2):ncol(data_train))]
y_test  = data_test$Recomm_Level
# save(file="data/DNN_test_sample.RData", X_train, y_train, X_test, y_test)
# End of data preparation

load("data/DNN_test_sample.RData")
DNN_model = function(X_train, y_train, X_test, y_test){
  # reshape
  x_data_train <- keras_array(as.matrix(X_train))
  # generate categorical y
  y_class <- c("B","BH","H","SH","S")
  y_train_category <- matrix(0, nrow=length(y_train), ncol = length(y_class))
  for (i in seq_along(y_class)){
    y_train_category[y_train == y_class[i], i] = 1
  }
  y_data_train=keras_array(y_train_category)
  
  
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 64, activation = 'relu', input_shape = c(n+12)) %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dense(units = 64, activation = 'relu') %>% 
    layer_dense(units = 32, activation = 'relu') %>% 
    layer_dense(units = 16, activation = 'relu') %>% 
    layer_dense(units= 5, activation = 'softmax')
  #layer_dense(units = 1, activation = 'sigmoid')

  #summary(model)
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adadelta()
  )
  
  history <- model %>% fit(
    x_data_train, y_data_train, 
    epochs = 50, batch_size = 64, 
    validation_split = 0.1
  )
  plot(history)
  
  x_data_test <- keras_array(as.matrix(X_test))
  y_pred <- model %>% predict_classes(x_data_test)
  y_pred <- factor(y_pred)
  levels(y_pred) = c("B", "BH", "H", "SH", "S")
  
  result = cbind(as.matrix(y_pred), y_test)
  
  result = result[order(result[,1],decreasing = F),]
}