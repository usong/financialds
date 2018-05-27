############
#策略:
#Long:前日结算价低于前日收盘价
#    当日开盘价高于前日结算价
#Sell:
#    10点止盈
############

library(financialds)
library(blotter)
if (!require("rjson", quietly = T))
  stop("package:", dQuote("rvest"), "cannot be loaded.")
if (!require("xts", quietly = T))
  stop("package:", dQuote("xts"), "cannot be loaded.")
if (!require("quantmod", quietly = T))
  stop("package:", dQuote("quantmod"), "cannot be loaded.")

Sys.setenv(TZ='GMT')
fr <- getFuture(
  src = "SQLite",
  dbname = "C:/Users/Usong/Desktop/example.db3"
)
fr <- na.omit(fr)
fr <- fr['2018-05-23 17:55:59::']


try(rm("account.breakout", "portfolio.breakout", pos = .blotter),
    silent = TRUE)
try(rm("ltaccount",
       "ltportfolio",
       "initDate",
       "initEq",
       "Posn",
       "UnitSize",
       "verbose"),
    silent = F)

initDate = '2018-05-22'
initEq = 10000
currency("USD")
ltportfolio = 'breakout'
ltaccount = 'breakout'
instrument = 'HC_M5'
assign(instrument,
       fr,
       .GlobalEnv)


initPortf(ltportfolio, as.character(instrument), initDate = initDate)
initAcct(
  ltaccount,
  portfolios = ltportfolio,
  initDate = initDate,
  initEq = initEq
)
verbose = F

for (i in 2:NROW(fr)) {
  CurrentDate = index(fr[i,])
  
  endDate <- as.character( as.Date( index(fr[i,]) , format = '%Y-%m-%d'))
  
  CurrentDate <- paste( as.Date( index(fr[i,]) , format = '%Y-%m-%d'),"23:59:59",sep=" ")
  
  Posn = getPosQty(ltportfolio, Symbol=instrument, Date=endDate)
  message( " Posn = ",Posn)
  
  if (Posn == 0) {
    #enter
    if (fr[i, 'Close'] > fr[i, 'SMA10m']  &&
        fr[i, 'Close'] > fr[i, 'SMA5m']   &&
        fr[i, 'Open'] < fr[i, 'SMA10m']  &&
        fr[i, 'Open'] < fr[i, 'SMA10m']) {
      message(index(fr[i, ]) , " Insert order.")
      
      addTxn(
        'breakout',
        Symbol = as.character(instrument),
        TxnDate = index(fr[i, ]),
        TxnPrice = fr[i, ]$Close,
        TxnQty = 1 ,
        TxnFees = 0,
        verbose = verbose
      )
      
    }
  } else {
    #quit
    if (fr[i, 'Close'] < fr[i, 'SMA10m']  &&
        fr[i, 'Close'] < fr[i, 'SMA5m']   &&
        fr[i, 'Open'] > fr[i, 'SMA10m']  &&
        fr[i, 'Open'] > fr[i, 'SMA10m']) {
      message(index(fr[i, ]) , " Quit order.")
      addTxn(
        'breakout',
        Symbol = as.character(instrument),
        TxnDate = index(fr[i, ]),
        TxnPrice = fr[i, ]$Close,
        TxnQty = -1 ,
        TxnFees = 0,
        verbose = verbose
      )
      
    }
  }
  updatePortf('breakout', Dates =  endDate)
  updateAcct('breakout', Dates = endDate)
  updateEndEq('breakout', Dates = endDate)

}

ymin <- min(fr[,'Close'])
ymax <- max(fr[,'Close'])
#tr indictor
#atr <-  ATR(fr[,c("High","Low","Close")], n=14)
# Chart results with quantmod
chart.Posn(ltportfolio, Symbol = instrument, Dates = "2018::")
#chart_Series(fr)
#add_TA(fr[,'SMA10m'], col = "yellow", lwd = 1.5, legend = "10m", type = 'l')
add_TA(fr[,'SMA5m'], col = "red", lwd = 1.5, legend = "5m", type = 'l',on=1)
add_TA(fr[,'SMA10m'], col = "blue", lwd = 1.5, legend = "10m", type = 'l',on=1)
#add_TA(atr[,'tr'], col = "red", lwd = 1.5, legend = "10m", type = 'l',on=4)





ltportfolio = getPortfolio("breakout")
#ltaccount = getAccount("dailybreak")
#chart_Series(fr['2018-03-22'])


library(rzmq)
context = init.context(1L)
socket = init.socket(context,"ZMQ_REP")
bind.socket(socket,"tcp://*:5555")
on.exit( close.socket(socket) )
while(1) {
 
  msg <- receive.socket(socket, unserialize=FALSE, dont.wait=TRUE)
  if (!is.null(msg))
  {
    msg <- rawToChar(msg)
    if (grepl("exit", msg))
      break
    message(msg)
    message('hello!')
    ans <- "rec:ok!"
    send.raw.string(socket, data = "1111", send.more = FALSE)
  }
  
}


library(pbdZMQ, quietly = TRUE)
### Initial.
context <- zmq.ctx.new()
responder <- zmq.socket(context, .pbd_env$ZMQ.ST$REP)
zmq.bind(responder, "tcp://*:5555")

### Send and receive 5 times.
while (1) {
  msg <- zmq.recv(responder, 10L)
  
  
  if (!is.null(msg))
  {
    cat(msg$buf, "\n")
    if (grepl("exit", msg$buf)) {
      break
    }
    zmq.send(responder, "World")
  }
}

### Finish.
zmq.close(responder)
zmq.ctx.destroy(context)

