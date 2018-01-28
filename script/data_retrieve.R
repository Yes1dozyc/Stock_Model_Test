data_retrieve = function(symbols, start_date="2014-01-01", source="google"){
  dataset <- xts()
  options("getSymbols.warning4.0"=FALSE)
  options("getSymbols.yahoo.warning"=FALSE)
  
  # cool progress bar to see the % of completion
  n <- length(symbols)
  # pb <- txtProgressBar(min = 0, max = n, style=3)
  
  # Actual loop: 
  for(i in 1:length(symbols)) {
    symbol <- symbols[i] 
    # specify the "from" date to desired start date
    stock <- tryCatch(
      getSymbols(symbol,from=start_date, src=source),
      error = function(e) NA,
      warning = function(w) NA
    )
    if(!is.na(eval(stock))){
      dataset <- 
      rm(list = c(stock))
    }
    # setTxtProgressBar(pb, i)
  }
  
  latest_trading_date = rownames(as.data.frame(dataset))[nrow(dataset)]
  
  # save(file=output, dataset, symbols)
  # cat(paste("\nNewest Data obtained! The last trading date is ", latest_trading_date, "\n", sep=""))
  dataset
}

data_retrieve_single = function(symbol, start_date="2014-01-01", source="google"){
  
  options("getSymbols.warning4.0"=FALSE)
  options("getSymbols.yahoo.warning"=FALSE)
  result = NULL
  
  # specify the "from" date to desired start date
  stock <- tryCatch(
     getSymbols(symbol,from=start_date, src=source),
     error = function(e) NA,
     warning = function(w) NA
  )
  if(!is.na(eval(stock))){
     result = get(stock)
  }
  result
}