library(randomForest)
library(dplyr)
RF_model = function(X_train, y_train, X_test, y_test=NULL, 
                    num_model=100, Sample_per_model=1000,
                    winner = 20, ntree = 20){
  
  type = c("B", "BH", "H", "SH", "S")
  type_sample = Sample_per_model / 5 # 5 types
  
  # Train RandomForest Model
  Final_count_test = matrix(0,nrow=nrow(X_test),ncol=5)
  colnames(Final_count_test) = type
  rownames(Final_count_test) = rownames(X_test)
  
  
  for (i in 1:num_model){
    used_sample = NULL
    for (j in seq_along(type)){
      used_sample_tmp = sample(length(which(y_train==type[j])),type_sample)
      used_sample_num = which(y_train==type[j])[used_sample_tmp]
      used_sample = c(used_sample, used_sample_num)
    }
    
    # setTxtProgressBar(pb, i)
    # used_sample = sample(nrow(X_train),Sample_per_model)
    
    X_train = X_train[used_sample, ]
    y_train = as.factor(y_train[used_sample])
    
    rf_model <- randomForest(x=X_train, y=y_train, ntree=ntree)
    
    # predict test set
    pred_Inc <- predict(rf_model,X_test,type = "prob")
    pred_Inc = pred_Inc[,c("B","BH","H","SH","S")]
    # winners = order(pred_Inc[,"B"], decreasing = T)[1:winner]
    # Final_count_test[winners,] = Final_count_test[winners,] + pred_Inc[winners,]
    Final_count_test= Final_count_test + pred_Inc
    
  }    
  prediction_table = Final_count_test
  # top_votes = nrow(Final_count_test)
  # top_stock = order(Final_count_test[,1],decreasing = T)[1:top_votes]
      
  # res_table <- cbind(Final_count_test,y_test)[top_stock,]
  # cat("Finished \n")
  # rownames(res_table) = rownames(X_test)[top_stock]
  prediction_table
}
