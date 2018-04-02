library(financialds)
library(WindR)
library(xts)
library(TTR)
library(quantmod)
w.start()
codes     = 'HC1805.SHF'
begintime = '2018-03-30'
endtime   = '2018-03-31'
fields    = 'open,high,low,close'
mat       = w.wsi(codes, fields, begintime , endtime, 'BarSize=5')$Data

fr <- xts(mat[, -1], as.POSIXct(mat[, 1], origin = "1970-01-01"))
colnames(fr) <- c('Open', 'High', 'Low', 'Close')
fr$SMA10m    <-  SMA(fr[, grep('Close', colnames(fr))], 10)
fr$SMA5m     <-  SMA(fr[, grep('Close', colnames(fr))], 5)
fr           <- na.omit(fr)
fr           <- fr['T09:00:00/T22:00:00']
instrument = 'windDataTest'
assign(instrument,
       fr,
       .GlobalEnv)
r <- getTrends(symbol = instrument,interval=c(3,4))

chart_Series(fr)
#mark range high or low point
add_TA(
  r$rangehigh,
  on = 1,
  col = 'purple',
  pch = 15,
  type = 'p',
  cex = 2
)
add_TA(
  r$rangelow,
  on = 1,
  col = 'red',
  pch = 15,
  type = 'p',
  cex = 2
)
