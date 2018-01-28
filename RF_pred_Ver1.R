suppressMessages(library(quantmod))
suppressMessages(library(dplyr))
suppressMessages(library(randomForest))
suppressMessages(library(ggplot2))
suppressMessages(library(xlsx))

rm(list=ls())
setwd("F:/Financial_Model")

source("script/get_endpoint.R")
n=65
alpha = 0.7

test_begin = as.Date("2016-06-01")
# test_end = as.Date("2016-06-05")
test_end = Sys.Date()

Result_monitor=matrix(0, nrow=as.numeric(test_end-test_begin)+1, ncol = 10)
for (k in test_begin:test_end){
  test_date = as.Date(k)
  test_date_c = as.character(test_date)
  
  input_data = paste("data/data_all/dataset_",test_date_c,".RData",sep="")
  
  current_line = as.numeric(test_date-test_begin)+1
  if (file.exists(input_data)){
    load(input_data)
    Data_norm = Data_Raw
    Data_norm[,1:n] = Data_norm[,1:n] / Data_Raw[,n]

    X_test_old = cbind(Data_norm, Data_Index)
    Y_test_pred_EMA = matrix(0, nrow=nrow(X_test_old),ncol=1)
    comp_case = complete.cases(X_test_old)
    X_test = X_test_old[comp_case,]
    
    train_sample_begin = as.Date(test_date)-20
    train_sample_end   = as.Date(test_date)-1
    
    for (i in train_sample_begin:train_sample_end){
      tmp_sample = as.character(as.Date(i))
      model_file = paste("result/model/model_",tmp_sample,".RData",sep="")
      if (file.exists(model_file)){
        load(model_file)
        Y_test_pred = predict(model, X_test)
        Y_test_pred[Y_test_pred<=0]=0
        if (i == train_sample_begin){
          Y_test_pred_EMA[comp_case,1] = Y_test_pred
        }else{
          Y_test_pred_EMA[comp_case,1] = alpha * Y_test_pred_EMA[comp_case,1] + (1-alpha)*Y_test_pred
        }
      }
    }
    Y_test_fin = data.frame(Pred_Val=round(Y_test_pred_EMA,2))
    rownames(Y_test_fin) = rownames(Data_Index)
    
    ## Performance Testing   
    Y_test = get_endpoint(Data_label,Data_Raw[,n])
    if(length(Y_test)>0){
      Y_test_fin$Real_val = Y_test
      # ggplot(data.frame(x=round(Y_test_pred_EMA,1), y=Y_test),aes(x=factor(x),y=y))+geom_violin(col="red")+geom_jitter()+theme_bw()+geom_hline(aes(yintercept=1),col="blue")
      Y_test_fin = Y_test_fin[order(Y_test_fin$Pred_Val, decreasing = T),]
    }
    
    sum_result = c(round(Y_test_fin$Real_val[1],3),
                   round(sum((Y_test_fin$Real_val[1:3]>0)*1)/3,3),
                   round(sum((Y_test_fin$Real_val[1:5]>0)*1)/5,3),
                   round(sum((Y_test_fin$Real_val[1:10]>0)*1)/10,3),
                   round(sum((Y_test_fin$Real_val>0)*1)/nrow(Y_test_fin),3),
                rownames(Y_test_fin)[1:5])
    
    Result_monitor[current_line, ] = sum_result
    cat(paste("Finished ", test_date_c, " Prediction!\n",sep=""))
  }
}

Result_monitor = as.data.frame(Result_monitor)
rownames(Result_monitor) = as.Date(test_begin:test_end)

Result_monitor_valid=Result_monitor[-which(Result_monitor$V5==0),]

write.table(Result_monitor_valid, file = "Prediction_EMA_20.txt", sep = "\t")
