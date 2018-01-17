library(quantmod)
data_retrieve = function(source="google"){
  ###
  # curr_date = Sys.Date()
  symbols_info = stockSymbols()
  symbols_info$MarketCap  = gsub("\\$","",symbols_info$MarketCap)
  symbols_info$MarketCap  = gsub("M","E06",symbols_info$MarketCap)
  symbols_info$MarketCap  = gsub("B","E09",symbols_info$MarketCap)
  symbols_info$MarketCap  = as.numeric(symbols_info$MarketCap)
  
  # Only "Mid-Large Market-Cap" stocks are considered in the model.
  mkt_cap = 5000000000 # easy read: 5,000,000,000; 5B
  symbols <- symbols_info$Symbol[which(symbols_info$MarketCap>mkt_cap)] 
  
  
  dataset <- xts()
  options("getSymbols.warning4.0"=FALSE)
  options("getSymbols.yahoo.warning"=FALSE)
  
  # cool progress bar to see the % of completion
  n <- length(symbols)
  pb <- txtProgressBar(min = 0, max = n, style=3)
  
  # Actual loop: 
  for(i in 1:length(symbols)) {
    symbol <- symbols[i] 
    # specify the "from" date to desired start date
    stock <- tryCatch(
      getSymbols(symbol,from="2014-01-01", src=source),
      error = function(e) NA,
      warning = function(w) NA
    )
    if(!is.na(eval(stock))){
      dataset <- merge(dataset, OHLCV(get(stock)))
      rm(list = c(stock))
    }
    setTxtProgressBar(pb, i)
  }
  
  latest_trading_date = rownames(as.data.frame(dataset))[nrow(dataset)]
  
  save(file="data/data_newest_google.RData", dataset, symbols_info, symbols, latest_trading_date, mkt_cap)
  cat("\nNewest Data obtained! The last trading date is ")
  cat(latest_trading_date)
  cat("\n")
}