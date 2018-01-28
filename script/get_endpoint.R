get_endpoint = function(data_observe, data_close, k=0.05){
  data_ob = data_observe /data_close  -1
 
  endpoint = apply(data_ob, 1, function(x) length(which(x>k)))
  # endpoint = apply(data_ob, 1, function(x) length(which(x>k))>0)
  
  endpoint
}