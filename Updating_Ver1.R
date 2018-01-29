# Updating script

suppressMessages(library(quantmod))
suppressMessages(library(dplyr))
suppressMessages(library(randomForest))

rm(list=ls())
setwd("F:/Financial_Model")
# load("data/data_newest.RData")

## source files
source("script/get_Symbols.R")
source("script/data_retrieve.R")
source("script/data_generating.R")
source("script/get_endpoint.R")


symbols = get_Symbols()
dataset = data_retrieve(symbols)
save(file="data/data_newest.RData", symbols, dataset)
# load("data/data_newest.RData")
curr_date = Sys.Date()

#### Part 1: Update Dataset (including Labels)
set.seed(6678)

n=65
p=14

starting_day = as.Date(curr_date-p)
# ending_day = starting_day
ending_day = curr_date

## Start creating Day-wise datasets
for (i in starting_day:ending_day){
  test_date = as.character(as.Date(i))
  if (length(dataset[test_date,])>2000){
    result = data_generating(symbols, dataset, test_date=test_date, n=n, p=p)
    
    raw_data_norm = result$rawdata / result$rawdata[,ncol(result$rawdata)]
    
    Data_Raw = cbind(result$rawdata, result$rawdata_index)
    Data_Index = result$indexing
    Data_label = result$label
    
    output = paste("data/data_all/dataset_",test_date,".RData",sep="")
    save(file=output,Data_Raw, Data_Index, Data_label, test_date, n, p)
    cat(paste("\nFinished ", test_date, " Data Generation!\n",sep=""))
  }else{
    cat(paste(test_date, " is a holiday!\n", sep=""))
  }
}

### Part 2: Update Models
alpha = 0.7
ntree=100

for (test_date in starting_day:ending_day){
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
