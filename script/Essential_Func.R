replace_NA = function(t_data,x){
  while(is.na(t_data[x])){
    x = x+1
  }
  t_data[x]
}

Eval_score = function(Observe_data,Nearest_Inc_day, Nearest_Dec_day,p=10,k=0.05){
  gain=0
  if (Nearest_Inc_day>0 ){ # && Nearest_Inc_day < Nearest_Dec_day
    gain = (Observe_data[Nearest_Inc_day]-1)/Nearest_Inc_day*p
  #}else if(Nearest_Dec_day>0 ){
  #  gain = (Observe_data[Nearest_Dec_day]-1)/Nearest_Dec_day*p
  }else{
    gain = Observe_data[length(Observe_data)]-1
  }
  round(gain,3)
}


Generate_data <- function(data, data_diff, rand_set, i, Train_ID, n=30, p=10, k=0.05){
  Train_start <-   rand_set[i]+1
  Train_end   <-   rand_set[i]+n
  observe_start <- rand_set[i]+n+1
  observe_end   <- rand_set[i]+n+p
  
  flag_test=0 # determine whether this data is for test use (including label) or for real prediction.
  if (observe_end <= ncol(data)){
    flag_test=1
  }
  
  # Get Total Dataset
  if (flag_test == 1){
    total_data   <- data[,Train_start:observe_end] # Test case
  }else{
    total_data <- data[,Train_start:Train_end] # Real case
  }
  
  # Normalization
  data_diff_use = data_diff[complete.cases(total_data),]
  data_all_use <- data[complete.cases(total_data),]
  total_data <- total_data[complete.cases(total_data),]
  total_data_norm <- apply(total_data,2,function(x) x/total_data[,n])
  
  Train_data <- total_data_norm[,1:n]
  if (flag_test == 1){
    Observe_data <- total_data_norm[,-c(1:n)]
  }
  
  Train_data_diff = round(Train_data[,-1]-Train_data[,-ncol(Train_data)],2)
  
  # Add Features  
  Final_x <- as.data.frame(Train_data) %>% mutate(
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
    Equal_1 = apply(Train_data_diff,1,function(x) sum((x==0)*1)),
    
    #Total trending part 1 (# Gain Day)
    Trends_5D  <- apply(data_diff_use[,(Train_end-4):Train_end],1,function(x) sum((x>0)*1,na.rm=T)), #5days
    Trends_2wk <- apply(data_diff_use[,(Train_end-7):Train_end],1,function(x) sum((x>0)*1,na.rm=T)), #2wks
    Trends_mo  <- apply(data_diff_use[,(Train_end-21):Train_end],1,function(x) sum((x>0)*1,na.rm=T)), #1mo
    Trends_3mo  <- apply(data_diff_use[,(Train_end-65):Train_end],1,function(x) sum((x>0)*1,na.rm=T)), #3mo
    Trends_all  <- apply(data_diff_use[,1:Train_end],1,function(x) sum((x>0)*1,na.rm=T)), #Since 2016-01-01
    
    #Total trending part 2(# Gain value)
    Gain_val_1 <- data_all_use[,Train_end] - apply(data_all_use,1,function(x) replace_NA(x,Train_end-4)), #5days
    Gain_val_2 <- data_all_use[,Train_end] - apply(data_all_use,1,function(x) replace_NA(x,Train_end-7)), #2wks
    Gain_val_3 <- data_all_use[,Train_end] - apply(data_all_use,1,function(x) replace_NA(x,Train_end-21)), #1mo
    Gain_val_4 <- data_all_use[,Train_end] - apply(data_all_use,1,function(x) replace_NA(x,Train_end-65)), #3mo
    Gain_val_5 <- data_all_use[,Train_end] - apply(data_all_use,1,function(x) replace_NA(x,1))  #Since 2016-01-01
    
  )
  
  # Change Column Name
  tmp_name <- c(Train_ID,
                "Mean_val","Max_val","Min_val","SD_val",
                "Gain_1","Gain_2","Gain_3","Gain_4",
                "Loss_1","Loss_2","Loss_3","Loss_4",
                "Equal_1",
                "Trends_1","Trends_2","Trends_3","Trends_4","Trends_5",
                "GainV_1","GainV_2","GainV_3","GainV_4","GainV_5"
  )
  colnames(Final_x) <- tmp_name
  
  # Generate Label for test dataset
  if (flag_test == 1){
    Final_x <- Final_x %>% mutate(
      ## Endpoint - Gain
      Inc_days = apply((Observe_data-Train_data[,ncol(Train_data)]),1,function(x) length(which(x>k))),
      Max_Inc = apply((Observe_data-Train_data[,ncol(Train_data)]),1,max),
      
      ## Endpoint - Loss
      Dec_days = apply((Observe_data-Train_data[,ncol(Train_data)]),1,function(x) length(which(x < -k))),
      Max_Dec = apply((Observe_data-Train_data[,ncol(Train_data)]),1,min),
      
      Inc_days = ifelse(Inc_days>0, Inc_days, (-1-Dec_days)),
      
      ## Endpoint - Nearest k% increasing Day
      Nearest_Inc = apply((Observe_data-Train_data[,ncol(Train_data)]),1,function(x) ifelse(length(which(x>k))>0, min(which(x>k)),0)),
      Nearest_Dec = apply((Observe_data-Train_data[,ncol(Train_data)]),1,function(x) ifelse(length(which(x< -k))>0, min(which(x< -k)),0))
    )
    
    Eval_scores <- sapply(1:nrow(Observe_data), function(x) Eval_score(Observe_data[x,],Final_x$Nearest_Inc[x], Final_x$Nearest_Dec[x],p,k))
    #print(as.numeric(Eval_scores))
    Final_x <- Final_x %>% mutate(Final_score =as.numeric(Eval_scores))
    
    # Add Column Name for test labels
    colnames(Final_x) <- c(tmp_name,
                           "Inc_days","Max_Inc",
                           "Dec_days","Max_Dec",
                           "Near_Inc","Near_Dec",
                           "Final_score")
  }
  # Add Row Name for each stock
  rownames(Final_x) <- rownames(total_data)
  
  # Return matrix
  Final_x
}


get_recomm_level = function(Final_x, R = 5, k = 0.05, p = 10){
  result = sapply(1:nrow(Final_x), function(x){
    #cat(x)
    if(Final_x$Inc_days[x] >= R){
      "B"
    }else if(Final_x$Inc_days[x] > 0){
      "BH"
    }else if (Final_x[x,paste("P",p,sep="")]>=1){
      "H"
    }else if (Final_x$Dec_days[x] >=R || Final_x[x,paste("P",p,sep="")] < (1-k) ){
      "S"
    }else{
      "SH"
    }
  })
  result
}
