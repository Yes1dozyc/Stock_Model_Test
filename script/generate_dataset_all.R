library(lubridate)
source("script/Essential_Func.R")

generate_dataset_all =function(dataset, n=65, p=10, R=5, k=0.05,
                               start_time=as.Date("2014-01-01"), end_time=Sys.Date(),
                               output_file = "data/dataset_ready_newest.RData"
                               ){
  
  begin_time = as.character(start_time) # Training period = 730 natural days. (2 yrs)
  end_time = as.character(end_time)
  time_period = paste(begin_time,"::",end_time,sep="")
  data_use = dataset[time_period]
  Cl_data <- Cl(data_use)
  data_all <- as.data.frame(t(Cl_data))
  
  Train_ID <- sapply(1:n, function(x) paste("D",x,sep=""))
  Days_ID  <- sapply(1:p, function(x) paste("P",x,sep=""))
  
  data_avail <- apply(data_all,2,function(x) sum(is.na(x)*1))
  data_all <- data_all[,data_avail<nrow(data_all)*0.75] # if over 3/4 stocks are NA, delete that date.
  
  # Generate testing dataset.
  # nearest_last_trading_day = colnames(data_all)[n]
  predictable_period = 1:(ncol(data_all)-n-p+1)
  
  #begin_point = Sys.time()
  Final_Table=NULL
  Final_Table_observe=NULL
  pb <- txtProgressBar(min = 0, max = length(predictable_period), style=3)
  for (i in predictable_period){ #
    setTxtProgressBar(pb, i)
    data = data_all[,i:(i+n+p-1)]
    data = data[complete.cases(data),]
    data = round(data / data[,n], 3)
    
    colnames(data) <- c(Train_ID, Days_ID)
    
    data_diff = data[,-1] - data[,-ncol(data)]
    n_diff = n-1 # because the col `n-1` in data_diff represents the changes from n-1 to n day.

    data_implement <- data[,Days_ID] %>% mutate(
      Last_day = data[,paste("P",p,sep="")],
      Inc_days = apply((data[,Days_ID]-data[,n]),1,function(x) length(which(x > k))),
      Max_Inc  = apply((data[,Days_ID]-data[,n]),1,max),
      
      ## Endpoint - Loss
      Dec_days = apply((data[,Days_ID]-data[,n]),1,function(x) length(which(x< -k))),
      Max_Dec  = apply((data[,Days_ID]-data[,n]),1,min)
      
    )
    
    data_Fin <- data[,Train_ID] %>% mutate (
      ## Generate extra features on data
      Mean_val = round(apply(data[,Train_ID],1,mean),3),
      Max_val  = round(apply(data[,Train_ID],1,max),3),
      Min_val  = round(apply(data[,Train_ID],1,min),3),
      SD_val   = round(apply(data[,Train_ID],1,sd),3),
      
      #Day of Gain in training window
      Gain_1 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0)*1)) / n,3),
      Gain_2 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0.02)*1)) / n,3),
      Gain_3 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0.05)*1)) / n,3),
      Gain_4 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0.1)*1)) / n,3),
      
      #Day of Loss in training window
      Loss_1 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< 0)*1)) / n,3),
      Loss_2 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< -0.02)*1)) / n,3),
      Loss_3 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< -0.05)*1)) / n,3),
      Loss_4 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< -0.1)*1) / n),3),
      
      #Detailed Gain trending (# Gain Day)
      Trends_5D  = apply(data_diff[,(n_diff-4):n_diff],1,function(x) sum((x>0)*1))/5, #5days
      Trends_10D  = apply(data_diff[,(n_diff-9):n_diff],1,function(x) sum((x>0)*1))/10,
      Trends_20D  = apply(data_diff[,(n_diff-19):n_diff],1,function(x) sum((x>0)*1))/20,
      Trends_50D  = apply(data_diff[,(n_diff-49):n_diff],1,function(x) sum((x>0)*1))/50,
      
      #Total trending part 2(# Gain value)
      Gain_val_5D = data[,n] - data[,n-4], #5days
      Gain_val_10D = data[,n] - data[,n-9],
      Gain_val_20D = data[,n] - data[,n-19],
      Gain_val_50D = data[,n] - data[,n-49]
      
    )

    Recomm_Level = get_recomm_level(Final_x = data_implement, R = R, k = k, p = p)
    
    data_Fin$Recomm_Level = Recomm_Level
    data_Fin$test_time = as.Date(ymd_hms(colnames(data_all)[n-1+i]))
    data_Fin$stock_name = rownames(data)
    # data_implement = round(data_implement,2)
    # rownames(data_Fin) = rownames(data)
    
    Final_Table <- rbind(Final_Table,data_Fin)
    Final_Table_observe <- rbind(Final_Table_observe, data_implement)
  }
  #end_point = Sys.time()
  #used_time = end_point - begin_point
  
  # Generate Newest Dataset (without observation)
  nearest_first_test_day = ncol(data_all)-n-p+1
  predictable_period = 1:p
  
  Final_Table_test=NULL
  Final_Table_test_observe=NULL
  # pb <- txtProgressBar(min = 0, max = length(predictable_period), style=3)
  for (i in predictable_period){ #
    # setTxtProgressBar(pb, i)
    i_use = i + nearest_first_test_day
    data = data_all[,i_use:ncol(data_all)]
    data = data[complete.cases(data),]
    data = round(data / data[,n], 3)
    
    data_diff = data[,-1] - data[,-ncol(data)]
    n_diff = n-1 # because the col `n-1` in data_diff represents the changes from n-1 to n day.
    
    if (i < p){
      colnames(data) <- c(Train_ID, Days_ID[1:(length(Days_ID)-i)])
      
      for (j in (p-i+1):p){
        data[,paste("P",j,sep="")]=NA
      }
      
      data_implement <- data[,Days_ID] %>% mutate(
        Last_day = data[,paste("P",p-i,sep="")],
        Inc_days = apply((data[,(n+1):ncol(data)]-data[,n]),1,function(x) length(which(x>k))),
        Max_Inc  = apply((data[,(n+1):ncol(data)]-data[,n]),1, max, na.rm=T),
        
        ## Endpoint - Loss
        Dec_days = apply((data[,(n+1):ncol(data)]-data[,n]),1,function(x) length(which(x< -k))),
        Max_Dec  = apply((data[,(n+1):ncol(data)]-data[,n]),1,min, na.rm=T)
      )
    }else{
      colnames(data) <- Train_ID
    }
    
    data_Fin <- data[,Train_ID] %>% mutate (
      ## Generate extra features on data
      Mean_val = round(apply(data[,Train_ID],1,mean),3),
      Max_val  = round(apply(data[,Train_ID],1,max),3),
      Min_val  = round(apply(data[,Train_ID],1,min),3),
      SD_val   = round(apply(data[,Train_ID],1,sd),3),
      
      #Day of Gain in training window
      Gain_1 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0)*1)) / n,3),
      Gain_2 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0.02)*1)) / n,3),
      Gain_3 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0.05)*1)) / n,3),
      Gain_4 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x> 0.1)*1)) / n,3),
      
      #Day of Loss in training window
      Loss_1 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< 0)*1)) / n,3),
      Loss_2 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< -0.02)*1)) / n,3),
      Loss_3 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< -0.05)*1)) / n,3),
      Loss_4 = round(apply(data_diff[,1:n_diff],1,function(x) sum((x< -0.1)*1) / n),3),
      
      #Detailed Gain trending (# Gain Day)
      Trends_5D  = apply(data_diff[,(n_diff-4):n_diff],1,function(x) sum((x>0)*1))/5, #5days
      Trends_10D  = apply(data_diff[,(n_diff-9):n_diff],1,function(x) sum((x>0)*1))/10,
      Trends_20D  = apply(data_diff[,(n_diff-19):n_diff],1,function(x) sum((x>0)*1))/20,
      Trends_50D  = apply(data_diff[,(n_diff-49):n_diff],1,function(x) sum((x>0)*1))/50,
      
      #Total trending part 2(# Gain value)
      Gain_val_5D = data[,n] - data[,n-4], #5days
      Gain_val_10D = data[,n] - data[,n-9],
      Gain_val_20D = data[,n] - data[,n-19],
      Gain_val_50D = data[,n] - data[,n-49]
      
    )
    
    data_Fin$test_time = as.Date(ymd_hms(colnames(data_all)[i_use+n-1]))
    data_Fin$stock_name = rownames(data)
    # data_implement = round(data_implement,2)
    # rownames(data_Fin) = rownames(data)
    
    Final_Table_test <- rbind(Final_Table_test,data_Fin)
    if (i < p){
      Final_Table_test_observe <- rbind(Final_Table_test_observe, data_implement)
    }
  }
  
  save(file=output_file,Final_Table, Final_Table_observe, 
                          Final_Table_test, Final_Table_test_observe,
       n, p, k, R)
  output_file
  #Final_Table
}