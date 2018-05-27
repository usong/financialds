library(financialds)
library(xts)
rs <- getAllStocks()
code <-
  paste('SH',rs$SH[1,]$code,sep = '_')
getStock(code)
getFuture(code='hc_f',constact = '1710',dbuser = 'root',dbpwd = '111111')
top15 <- getContacts('hc_f','root','111111') #return top 15 volumn

Sys.setlocale("LC_TIME","C")
library(DBI)
library(RSQLite)
library(microbenchmark)
library(TTR)
library(RcppBDT)
library(fasttime)
library(xts)
conn <-
  DBI::dbConnect(RSQLite::SQLite(), dbname = "C:/Users/Usong/Desktop/example.db3")
on.exit(dbDisconnect(conn))
dbListTables(conn)

getdata <- function() {
  rs <- dbSendQuery(
    conn,
    "select substr(datetime('now', 'localtime') , 1 ,10) || ' ' || UpdateTime as time  ,BidPrice1,AskPrice1 from DepthMarketData"
  )
  chunk <- NULL
  while (!dbHasCompleted(rs)) {
    chunk <- rbind(chunk , dbFetch(rs, 100000))
  }
  dbClearResult(rs)
  
  #fr <-
  #  xts(chunk[, -1], as.POSIXlt(chunk[, 1], origin = "1970-01-01"))
  #fr <-
  #  xts(chunk[, -1], order.by = strptime(chunk[, 1], format="%Y-%m-%d  %H:%M:%S"))
  fr <-
    xts(chunk[, -1], order.by = fastPOSIXct(chunk[, 1], tz='GMT'))
  
  #fr <-
  #  xts(chunk[, -1], order.by = RcppBDT::cToPOSIXct(chunk[, 1], tz='GMT'))
  k <- to_period(fr, period = 'minutes', k = 5)
  k$SMA10m <-  SMA(k[, grep('fr.Close', colnames(k))], 1)
  k$SMA5m <-  SMA(k[, grep('fr.Close', colnames(k))], 2)
}


microbenchmark(getdata(),times=5L)


library(quantmod)
r <- getFuture(
  src = "SQLite",
  dbname = "C:/Users/Usong/Desktop/example.db3"
)

chart_Series(k)
add_TA(fr[,'SMA5m'], col = "red", lwd = 1.5, legend = "5m", type = 'l',on=1)
add_TA(fr[,'SMA10m'], col = "blue", lwd = 1.5, legend = "10m", type = 'l',on=1)
add_TA(atr[,'tr'], col = "red", lwd = 1.5, legend = "10m", type = 'l',on=4)



