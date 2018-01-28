get_Symbols = function( mkt_cap_low  =    5000000000,
                        mkt_cap_high = 5000000000000){
  # Data retrieve
  symbols_info = stockSymbols(exchange = c("NASDAQ", "NYSE"))
  symbols_info$MarketCap  = gsub("\\$","",symbols_info$MarketCap)
  symbols_info$MarketCap  = gsub("M","E06",symbols_info$MarketCap)
  symbols_info$MarketCap  = gsub("B","E09",symbols_info$MarketCap)
  symbols_info$MarketCap  = as.numeric(symbols_info$MarketCap)
  
  # Only "Mid-Large Market-Cap" stocks are considered in the model.
 
  
  qualified_stock = intersect(which(symbols_info$MarketCap>mkt_cap_low), which(symbols_info$MarketCap<mkt_cap_high))
  symbols <- symbols_info$Symbol[qualified_stock] 
  
  save(file="data/used_symbols.RData",symbols, mkt_cap_low, mkt_cap_high)
  unique(symbols)
}