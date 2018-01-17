simulate_profit = function(data, stocks, Begin_money = 1000, i = 1, p=1.05){
  max_i = nrow(stocks)
  currency_flow=NULL
  current_money = Begin_money
  while( i <= max_i){
    choose_stock = stocks[i]
    test_stock_info = data[choose_stock,]
    begin_price = as.numeric(test_stock_info[1,i])
    if (! is.na (begin_price)){
      j = i + 1
      while( j <= max_i && (is.na(test_stock_info[1,j]) || (test_stock_info[1,j] < p * begin_price) && ((j - i) < 10) )){
        j = j + 1
      }
      ending_price = test_stock_info[1,j]
      current_money = as.integer (current_money * ending_price / begin_price)
      currency_flow = rbind(currency_flow, 
                            c(current_money,choose_stock, 
                              colnames(test_stock_info)[i],colnames(test_stock_info)[j],
                              begin_price, ending_price, round(ending_price/begin_price,3)))
      i = j
    }else{
      i = i+1
    }
  }
  colnames(currency_flow) = c("Current_Money", "Stock", "Buying_Day", "Selling_Day","Buying_Price", "Selling_Price", "Earning_ratio")
  currency_flow
}