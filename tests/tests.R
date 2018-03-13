library(financialds)
rs <- getAllStocks()
code <-
  paste('SH',rs$SH[1,]$code,sep = '_')
getStock(code)
getFuture(code='hc_f',constact = '1710',dbuser = 'root',dbpwd = '111111')
top15 <- getContacts('hc_f','root','111111') #return top 15 volumn

