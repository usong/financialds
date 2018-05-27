library('rjson')
library(TTR)
library(xts)
library(financialds)
options(scipen = 200)
getSinaData <- function(code) {
  url <-
    paste("http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesMiniKLine5m?symbol=",
          code,sep="")
  raw.data <-
    try(readLines(url, warn = "F", encoding = "UTF-8"), silent = T)
  if (inherits(raw.data , "try-error")) {
    warning("connect to website failed.")
    return(NA)
  }
  
  json.data <- try(fromJSON(raw.data), silent = T)
  if (inherits(json.data , "try-error")) {
    warning("parse json data  failed.")
    return(NA)
  }
  mat <- matrix(0,
                ncol = 6,
                nrow = 0,
                byrow = T)
  for (row in json.data) {
    row <- unlist(row)
    entry <- c(as.POSIXct(row[1], format = "%Y-%m-%d %H:%M:%S"))
    len <- length(row)
    for (i in 2:len) {
      entry <- cbind(entry , as.numeric(row[i]))
    }
    mat <- rbind(mat, entry)
  }
  
  fr <- xts(mat[, -1], as.POSIXct(mat[, 1], origin = "1970-01-01"))
  colnames(fr) <- c('Open', 'High', 'Low', 'Close', 'Vol')
  fr$SMA10m <-  SMA(fr[, grep('Close', colnames(fr))], 10)
  fr$SMA5m <-  SMA(fr[, grep('Close', colnames(fr))], 5)
  return(fr)
}
#HC0/RB0
hcx <- getSinaData("HC1810")
rbx <- getSinaData("HC1805")
hcx           <- na.omit(hcx)
hcx           <- hcx['T09:00:00/T22:00:00']
rbx           <- na.omit(rbx)
rbx           <- rbx['T09:00:00/T22:00:00']
#价差
if( nrow(hcx) == nrow(rbx) ) {
  psub <-  hcx[,'Close'] - rbx[,'Close'] 
  plot(psub)
}
#收益率
hcxpratio <- hcx[,'Close'] / lag(hcx[,'Close'],1)  -1
rbxpratio <- rbx[,'Close'] / lag(rbx[,'Close'],1)  -1
prfilesub <- hcxpratio - rbxpratio
#观察收益率差的波动区间
plot(prfilesub)
#波动区间上正负0.002之间密集波动
#找出区间外的时间点
#0.002 区域上沿 下沿为open信号点
prfilesub[ prfilesub >  0.001]
prfilesub[ prfilesub <  -0.001]


range = '2018-05-02 09:05:00::2018-05-02 15:20:00'
merge(hcx[range,'Close'],
      rbx[range,'Close'],
      hcx[range,'Close']-rbx[range,'Close'],
      prfilesub[range,'Close'])


instrument = 'hcx'
assign(instrument,
       hcx,
       .GlobalEnv)
hcr <- getTrends(symbol = instrument,interval=c(6,7,8,9))
hcx <- hcx["2018-04-27::"]
hcr$rangehigh <- hcr$rangehigh["2018-04-01::"]
hcr$rangelow <- hcr$rangelow["2018-04-01::"]
chart_Series(hcx)
#mark range high or low point
add_TA(
  hcr$rangehigh,
  on = 1,
  col = 'purple',
  pch = 15,
  type = 'p',
  cex = 2
)
add_TA(
  hcr$rangelow,
  on = 1,
  col = 'yellow',
  pch = 15,
  type = 'p',
  cex = 2
)



#instrument = 'rbx'
#assign(instrument,
#       rbx,
#       .GlobalEnv)
#rbr <- getTrends(symbol = instrument,interval=c(3,4,5,6,7,8,9))



#instrument = 'sinaDataTest'
#assign(instrument,
#       fr,
#       .GlobalEnv)
#r <- getTrends(symbol = instrument,interval=c(6,7,8,9))

#chart_Series(fr)
#mark range high or low point
#add_TA(
#  r$rangehigh,
#  on = 1,
#  col = 'purple',
#  pch = 15,
#  type = 'p',
#  cex = 2
#)
#add_TA(
#  r$rangelow,
#  on = 1,
#  col = 'red',
#  pch = 15,
#  type = 'p',
#  cex = 2
#)





