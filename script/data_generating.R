data_generating = function(symbols, dataset, test_date, n=65, p=14){
  # Actual loop: 
  m <- length(symbols)
  pb <- txtProgressBar(min = 0, max = m, style=3)
  
  Data_train=NULL
  Data_raw  =NULL
  Data_label=NULL
  
  test_date_begin = as.character(as.Date(test_date)-100)
  test_date_end = test_date
  observed_date_begin = as.character(as.Date(test_date)+1)
  observed_date_end = as.character(as.Date(test_date)+p)
  
  Train_ID <- sapply(1:n, function(x) paste("D",x,sep=""))
  Observe_ID <- NULL
  
  for(i in 1:m) {
    stock <- symbols[i] 
    
    # specify the stock columns
    pat = paste(stock,"\\.",sep="")
    used_col = which(regexpr(pat, colnames(dataset))==1)
    
    if(length(used_col)==5){
      data = dataset[,used_col]

      data_curr = data[paste(test_date_begin,"::",test_date_end,sep="")]
      data_curr = data_curr[complete.cases(data_curr),]
      data_close = Cl(data_curr)
      
      data_observe = data[paste(observed_date_begin,"::",observed_date_end,sep="")]
      data_observe_cl = Cl(data_observe)
      Observe_ID <- sapply(1:nrow(data_observe_cl), function(x) paste("P",x,sep=""))
      
      if (nrow(data_close)>=n){
        data_close = data_close[(nrow(data_close)-n+1):nrow(data_close),]
        MACD_index = as.data.frame(MACD(data_close))[test_date,] # MACD
        
        # Relative Strength Index RSI (3day, 7day 14 day, etc.)
        RSI_index_3D = as.data.frame(RSI(data_close, n = 3))[test_date,] 
        RSI_index_7D = as.data.frame(RSI(data_close, n = 7))[test_date,] 
        RSI_index_14D = as.data.frame(RSI(data_close, n = 14))[test_date,]
        
        # (Commodity Channel Index) CCI
        CCI_index_3D = as.data.frame(CCI(data_close, n = 3))[test_date,]
        CCI_index_7D = as.data.frame(CCI(data_close, n = 7))[test_date,]
        CCI_index_14D = as.data.frame(CCI(data_close, n = 14))[test_date,]
        
        # Williams Percent Range
        WPR_data = HLC(data_curr)
        WPR_index_3D = as.data.frame(WPR(WPR_data, n=3))[test_date,]
        WPR_index_7D = as.data.frame(WPR(WPR_data, n=7))[test_date,]
        WPR_index_14D = as.data.frame(WPR(WPR_data, n=14))[test_date,]
        
        ADX_data = data_curr
        ADX_index_3D = as.data.frame(ADX(ADX_data, n=3))[test_date,]
        ADX_index_7D = as.data.frame(ADX(ADX_data, n=7))[test_date,]
        ADX_index_14D = as.data.frame(ADX(ADX_data, n=14))[test_date,]
        
        # SMA_index_3D = as.data.frame(SMA(data_close, n=3))[test_date,]
        # SMA_index_7D = as.data.frame(SMA(data_close, n=7))[test_date,]
        # SMA_index_14D = as.data.frame(SMA(data_close, n=14))[test_date,]
        
        Final_data = cbind(
                           MACD_index,
                           RSI_index_3D, RSI_index_7D, RSI_index_14D,
                           CCI_index_3D, CCI_index_7D, CCI_index_14D,
                           WPR_index_3D, WPR_index_7D, WPR_index_14D,
                           ADX_index_3D, ADX_index_7D, ADX_index_14D
                           # SMA_index_3D, SMA_index_7D, SMA_index_14D
        )
        #rownames(Final_data) = paste(stock,"[",test_date,"]",sep="")
        rownames(Final_data) = paste(stock,".Close",sep="")
        colnames(Final_data) = c(
                                 # "Open","High","Low","Close","Volume", 
                                 "MACD", "Signal", 
                                 "RSI_3Days", "RSI_7Days", "RSI_14Days",
                                 "CCI_3Days", "CCI_7Days", "CCI_14Days",
                                 "WPR_3Days", "WPR_7Days", "WPR_14Days",
                                 "ADX_3days_Dlp","ADX_3days_Dln","ADX_3days_DX","ADX_3days",
                                 "ADX_7days_Dlp","ADX_7days_Dln","ADX_7days_DX","ADX_7days",
                                 "ADX_14days_Dlp","ADX_14days_Dln","ADX_14days_DX","ADX_14days"
                                 # "SMA_3Days", "SMA_7Days", "SMA_14Days"
                                 )
        
        Data_train = rbind(Data_train, Final_data)
        Data_raw   = rbind(Data_raw, as.matrix(t(data_close)))
        Data_label = rbind(Data_label, as.matrix(t(data_observe_cl)))
      }
    }
    setTxtProgressBar(pb, i)
  }
  
  # curr_date = Sys.Date()
  # save(file="Data/Index_stock.RData", Data_train, Data_label, test_date, curr_date)

  colnames(Data_raw) = Train_ID
  if (length(Data_label)>0){
    colnames(Data_label) = Observe_ID
  }
  
  Dataraw_index = get_rawdata_index(Data_raw)
  
  result=list()
  result$indexing = Data_train
  result$rawdata = Data_raw
  result$rawdata_index = Dataraw_index
  result$label = Data_label
  result
}

get_rawdata_index = function(data){
  Train_data = round(data / data[,ncol(data)],2)
  Train_data_diff = Train_data[,-1] - Train_data[,-ncol(Train_data)]
  Final_x <- data.frame(
    ## Generate extra features on Train_data
    Mean_val = apply(Train_data,1,mean),
    Max_val  = apply(Train_data,1,max),
    Min_val  = apply(Train_data,1,min),
    SD_val   = apply(Train_data,1,sd),
    
    #Day of Gain in training window
    Gain_1 = apply(Train_data_diff,1,function(x) sum((x>0)*1)),
    Gain_2 = apply(Train_data_diff,1,function(x) sum((x>0.02)*1)),
    Gain_3 = apply(Train_data_diff,1,function(x) sum((x>0.05)*1)),
    Gain_4 = apply(Train_data_diff,1,function(x) sum((x>0.1)*1)),
    #Day of Loss in training window
    Loss_1 = apply(Train_data_diff,1,function(x) sum((x< 0)*1)),
    Loss_2 = apply(Train_data_diff,1,function(x) sum((x< -0.02)*1)),
    Loss_3 = apply(Train_data_diff,1,function(x) sum((x< -0.05)*1)),
    Loss_4 = apply(Train_data_diff,1,function(x) sum((x< -0.1)*1)),
    
    #Day of equal in training window
    Equal_1 = apply(Train_data_diff,1,function(x) sum((x==0)*1))
  )
  rownames(Final_x) = rownames(data)
  Final_x
}

data_generating_single = function(symbol_data, test_date, n=65, p=14){
  # Actual loop: 
  Data_train=NULL
  Data_raw  =NULL
  Data_label=NULL
  
  test_date_begin = as.character(as.Date(test_date)-100)
  test_date_end = test_date
  observed_date_begin = as.character(as.Date(test_date)+1)
  observed_date_end = as.character(as.Date(test_date)+p)
  
  Train_ID <- sapply(1:n, function(x) paste("D",x,sep=""))
  Observe_ID <- NULL
  
  data = symbol_data
  data_curr = data[paste(test_date_begin,"::",test_date_end,sep="")]
  
  result=list()
  if(length(data_curr)>0 && ncol(data_curr)==5 && nrow(data_curr)>=n){
    data_curr = data_curr[complete.cases(data_curr),]
    data_close = Cl(data_curr)
    
    data_observe = data[paste(observed_date_begin,"::",observed_date_end,sep="")]
    data_observe_cl = Cl(data_observe)
    Observe_ID <- sapply(1:nrow(data_observe_cl), function(x) paste("P",x,sep=""))
      
    if (nrow(data_close)>=n){
        data_close = data_close[(nrow(data_close)-n+1):nrow(data_close),]
        MACD_index = as.data.frame(MACD(data_close))[test_date,] # MACD
        
        # Relative Strength Index RSI (3day, 7day 14 day, etc.)
        RSI_index_3D = as.data.frame(RSI(data_close, n = 3))[test_date,] 
        RSI_index_7D = as.data.frame(RSI(data_close, n = 7))[test_date,] 
        RSI_index_14D = as.data.frame(RSI(data_close, n = 14))[test_date,]
        
        # (Commodity Channel Index) CCI
        CCI_index_3D = as.data.frame(CCI(data_close, n = 3))[test_date,]
        CCI_index_7D = as.data.frame(CCI(data_close, n = 7))[test_date,]
        CCI_index_14D = as.data.frame(CCI(data_close, n = 14))[test_date,]
        
        # Williams Percent Range
        WPR_data = data_curr[,2:4]
        WPR_index_3D = as.data.frame(WPR(WPR_data, n=3))[test_date,]
        WPR_index_7D = as.data.frame(WPR(WPR_data, n=7))[test_date,]
        WPR_index_14D = as.data.frame(WPR(WPR_data, n=14))[test_date,]
        
        ADX_data = data_curr
        ADX_index_3D = as.data.frame(ADX(ADX_data, n=3))[test_date,]
        ADX_index_7D = as.data.frame(ADX(ADX_data, n=7))[test_date,]
        ADX_index_14D = as.data.frame(ADX(ADX_data, n=14))[test_date,]
        
        # SMA_index_3D = as.data.frame(SMA(data_close, n=3))[test_date,]
        # SMA_index_7D = as.data.frame(SMA(data_close, n=7))[test_date,]
        # SMA_index_14D = as.data.frame(SMA(data_close, n=14))[test_date,]
        
        Final_data = cbind(
          MACD_index,
          RSI_index_3D, RSI_index_7D, RSI_index_14D,
          CCI_index_3D, CCI_index_7D, CCI_index_14D,
          WPR_index_3D, WPR_index_7D, WPR_index_14D,
          ADX_index_3D, ADX_index_7D, ADX_index_14D
          # SMA_index_3D, SMA_index_7D, SMA_index_14D
        )
        #rownames(Final_data) = paste(stock,"[",test_date,"]",sep="")
        rownames(Final_data) = colnames(symbol_data)[4]
        colnames(Final_data) = c(
          # "Open","High","Low","Close","Volume", 
          "MACD", "Signal", 
          "RSI_3Days", "RSI_7Days", "RSI_14Days",
          "CCI_3Days", "CCI_7Days", "CCI_14Days",
          "WPR_3Days", "WPR_7Days", "WPR_14Days",
          "ADX_3days_Dlp","ADX_3days_Dln","ADX_3days_DX","ADX_3days",
          "ADX_7days_Dlp","ADX_7days_Dln","ADX_7days_DX","ADX_7days",
          "ADX_14days_Dlp","ADX_14days_Dln","ADX_14days_DX","ADX_14days"
          # "SMA_3Days", "SMA_7Days", "SMA_14Days"
        )
        
        Data_train = rbind(Data_train, Final_data)
        Data_raw   = rbind(Data_raw, as.matrix(t(data_close)))
        Data_label = rbind(Data_label, as.matrix(t(data_observe_cl)))
    }
    colnames(Data_raw) = Train_ID
    if (length(Data_label)>0){
      colnames(Data_label) = Observe_ID
    }
    Dataraw_index = get_rawdata_index_single(Data_raw)
    
    result$indexing = Data_train
    result$rawdata = Data_raw
    result$rawdata_index = Dataraw_index
    result$label = Data_label
  }
  result
}


get_rawdata_index_single = function(data){
  Train_data = round(data / data[,ncol(data)],2)
  Train_data_diff = Train_data[,-1] - Train_data[,-ncol(Train_data)]
  Final_x <- data.frame(
    ## Generate extra features on Train_data
    Mean_val = mean(Train_data),
    Max_val  = max(Train_data),
    Min_val  = min(Train_data),
    SD_val   = sd(Train_data),
    
    #Day of Gain in training window
    Gain_1 = sum((Train_data_diff>0) *1),
    Gain_2 = sum((Train_data_diff>0.02) *1),
    Gain_3 = sum((Train_data_diff>0.05) *1),
    Gain_4 = sum((Train_data_diff>0.1) *1),
    #Day of Loss in training window
    Loss_1 = sum((Train_data_diff<0) *1),
    Loss_2 = sum((Train_data_diff< -0.02) *1),
    Loss_3 = sum((Train_data_diff< -0.05) *1),
    Loss_4 = sum((Train_data_diff< -0.1) *1),
    
    #Day of equal in training window
    Equal_1 = sum((Train_data_diff==0) *1)
  )
  rownames(Final_x) = rownames(data)
  Final_x
}
