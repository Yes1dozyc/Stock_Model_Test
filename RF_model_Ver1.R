suppressMessages(library(quantmod))
suppressMessages(library(dplyr))
suppressMessages(library(randomForest))

rm(list=ls())
setwd("F:/Financial_Model")

source("script/get_endpoint.R")
n=65
ntree=100

test_begin = as.Date("2018-01-24")
# test_end = as.Date("2016-01-03")
test_end   = Sys.Date()

for (test_date in test_begin:test_end){
  test_date = as.character(as.Date(test_date))
  input_data = paste("data/data_all/dataset_",test_date,".RData",sep="")
  if (file.exists(input_data)){
    load(input_data)
    
    Y = get_endpoint(Data_label,Data_Raw[,n])
    if(length(Y)==nrow(Data_Raw)){
      Y[Y==0] = -10
      # Y[Y==T] = "Positive"
      # Y[Y==F] = "Negative"
      # Y = as.factor(Y)
      
      
      Data_norm = Data_Raw
      Data_norm[,1:n] = Data_norm[,1:n] / Data_Raw[,n]
      
      X = cbind(Data_norm, Data_Index)
      
      X_train = X[complete.cases(X),]
      Y_train = Y[complete.cases(X)]
      model = randomForest(x=X_train, y=Y_train, ntree=ntree)
      
      # Y_pred = round(predict(model,X_train),0)
      # Y_pred[Y_pred<=0]=0
      # which(Y_pred != Y_train)
      # View(cbind(Y_train, Y_pred, Y_pred-Y_train))
      out=paste("result/model/model_",test_date,".RData",sep="")
      save(file=out, model, X_train, Y_train, test_date)
      cat(paste("Model Construted. - ", test_date, " -\n", sep=""))
    }
  }
}
