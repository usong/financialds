
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
lx <- getSinaData("L1809")
ppx <- getSinaData("PP1809")
lx           <- na.omit(lx)
lx           <- lx['T09:00:00/T22:00:00']
ppx           <- na.omit(ppx)
ppx           <- ppx['T09:00:00/T22:00:00']

#价差
if( nrow(lx) == nrow(ppx) ) {
  psub <-  lx[,'Close'] - ppx[,'Close'] 
  plot(psub)
}
#收益率
lxpratio <- lx[,'Close'] / lag(lx[,'Close'],1)  -1
ppxpratio <- ppx[,'Close'] / lag(ppx[,'Close'],1)  -1
lpp_prfilesub <- lxpratio - ppxpratio
#观察收益率差的波动区间
plot(lpp_prfilesub)
#波动区间上正负0.002之间密集波动
#找出区间外的时间点
#0.002 区域上沿 下沿为open信号点
lpp_prfilesub[ lpp_prfilesub >  0.002]
lpp_prfilesub[ lpp_prfilesub <  -0.002]



range = '2018-05-02 09:05:00::2018-05-02 15:20:00'
merge(lx[range,'Close'],
      ppx[range,'Close'],
      lx[range,'Close']-ppx[range,'Close'],
      lpp_prfilesub[range,'Close'])