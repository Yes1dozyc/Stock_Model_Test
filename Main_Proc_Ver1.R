suppressMessages(library(quantmod))
suppressMessages(library(dplyr))

rm(list=ls())
setwd("F:/Financial_Model")
# load("data/data_newest.RData")

## source files
source("script/get_Symbols.R")
source("script/data_retrieve.R")
source("script/data_generating.R")
source("script/get_endpoint.R")


symbols = get_Symbols()


stock_list = lapply(symbols[1:50], function(x) data_retrieve_single(x))

stock_index = lapply(stock_list, function(x) data_generating_single(x, test_date))

dataset = data_retrieve(symbols)
save(file="data/data_newest.RData", symbols, dataset)


load("data/data_newest.RData")
# Hyper parameters
starting_day = as.Date("2018-01-25")
# ending_day = starting_day
ending_day = Sys.Date()

n=65
p=14

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

