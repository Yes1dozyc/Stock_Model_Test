library(lubridate)
source("script/Essential_Func.R")

generate_dataset = function(dataset, test_time, n=60, p=10, k=0.05, 
                            series=20, output_file='tmp/Train_Data.RData'){
  # Only use Open data for modeling
  Op_data <- Op(dataset)
  begin_time = as.character(test_time-730) # Training period = 730 natural days. (2 yrs)
  end_time = as.character(test_time)
  time_period = paste(begin_time,"::",as.character(test_time),sep="")
  Op_data <- Op_data[time_period]
  data <- as.data.frame(t(Op_data))
  
  last_trading_day = colnames(data)[ncol(data)-p-1]
  # Delete date with most NAs.
  data_avail <- apply(data,2,function(x) sum(is.na(x)*1))
  data <- data[,data_avail<nrow(data)*0.75] # if over 3/4 stocks are NA, delete that date.
  # Measure day-difference
  data_diff <- round(data[,-1] - data[,-ncol(data)],2)
  
  ## Generating Training Datasets
  Train_ID <- sapply(1:n, function(x) paste("D",x,sep=""))
  
  test_window = 30 # Leave 50 trading days for testing use
  begin_window = 65
  training_pool <- ncol(data)-test_window-p*2-n-begin_window
  pb <- txtProgressBar(min = 0, max = series*2, style=3)
  
  rand_set <- begin_window + sample(training_pool,series)
  
  ### Long Period data
  Final_Table_LP=NULL
  for (i in 1:series){
    setTxtProgressBar(pb, i)
    
    Final_x = Generate_data(data, data_diff, rand_set, i, Train_ID, n, p, k)
    Final_Table_LP <- rbind(Final_Table_LP,round(Final_x,2))
    
  }

  ### Short Period data (in later time window)
  testing_pool <- ncol(data)-test_window-p*2-n
  rand_set <- testing_pool + sample(test_window,series)
  Final_Table_SP=NULL
  for (i in 1:series){
    setTxtProgressBar(pb, i+series)
    
    Final_x = Generate_data(data, data_diff, rand_set, i, Train_ID, n, p, k)
    Final_Table_SP <- rbind(Final_Table_SP,round(Final_x,2))
  }
  
  ### Test data
  series <- 1
  rand_set <- ncol(data)-1-n-p # last day of trading, no observation window
  Final_Table_test=NULL
  for (i in 1:series){
    Final_x = Generate_data(data, data_diff, rand_set, i, Train_ID, n, p, k)
    Final_Table_test <- rbind(Final_Table_test,round(Final_x,2))
  }
  cat("\nDataset generated!\n")
  ## Save datasets
  
  save(file=output_file,Final_Table_LP,Final_Table_SP,Final_Table_test,last_trading_day)
  output_file
}