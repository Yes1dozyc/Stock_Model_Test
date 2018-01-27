# Stock_Model_Test  
Test RandomForest On Stocks  

1. Precision are more important.  
  
    I need to find only a few stocks with positive perspectives. More stocks are meaningless because I cannot buy too many. However, negative stocks (result in loss) should be strongly avoided.  
  
    The worst situation, is I didn't buy any stock, which means I have no gain no loss on that day.  

2. use Random Forest as the predicting model  

    Why random Forest?  
 * it is easy to implement and use in R, which is current language used in this project.  
 * do not need feature selection.  
 * its principal is quite suitable to stock modeling:  
 1) decision based algorithm is easy and intuitive to understand; no black box.  
 2) each day the stock may have different traits, the model can be built based on each day's data, respectively.  
 3) consensus modeling is a good way to find the strongest candidates.  
 
3. How to define the dataset?   

1) Raw data need to be updated every time the model was constructed; -- data_retrieve()  
     The raw data is overwriten in data/data_newest.RData.  
     Historical file does not need to be kept.  
       
2) However, only missing days are needed to be updated; -- dataset_generating()  

  Three types of data need to be generated:  
  (1)  Raw data (the closing price of each stock in previous n Days)
  (2)  Basic statistics of Raw data -- get_rawdata_index(rawdata)
  (3)  Stock Indexing (measured by quantmod functions)
 


