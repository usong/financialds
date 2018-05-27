# CTA Strategy
# 基于HC/RB 5分钟收益波动率区间交易策略
# 统计套利
library(WindR)
library(xts)
library(TTR)
library(quantmod)
w.start()
codes     = 'HC1805.SHF'
begintime = '2018-03-26'
endtime   = '2018-04-02'
fields    = 'open,high,low,close'
hc       = w.wsi(codes, fields, begintime , endtime, 'BarSize=5')$Data

codes     = 'RB1805.SHF'
rb       = w.wsi(codes, fields, begintime , endtime, 'BarSize=5')$Data


rbx <- xts(rb[, -1], as.POSIXct(rb[, 1], origin = "1970-01-01"))
colnames(rbx) <- c('Open', 'High', 'Low', 'Close')
rbx$SMA10m    <-  SMA(rbx[, grep('Close', colnames(rbx))], 10)
rbx$SMA5m     <-  SMA(rbx[, grep('Close', colnames(rbx))], 5)
rbx           <- na.omit(rbx)
rbx           <- rbx['T09:00:00/T22:00:00']

hcx <- xts(hc[, -1], as.POSIXct(hc[, 1], origin = "1970-01-01"))
colnames(hcx) <- c('Open', 'High', 'Low', 'Close')
hcx$SMA10m    <-  SMA(hcx[, grep('Close', colnames(hcx))], 10)
hcx$SMA5m     <-  SMA(hcx[, grep('Close', colnames(hcx))], 5)
hcx           <- na.omit(hcx)
hcx           <- hcx['T09:00:00/T22:00:00']

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
prfilesub[ prfilesub >  0.002]
prfilesub[ prfilesub <  -0.004]

#上沿short  下沿long

#查看时间点后的价格变动情况
range = '2018-04-02 21:15:00::2018-04-02 22:15:00'
merge(hcx[range,'Close'],
      rbx[range,'Close'],
      hcx[range,'Close']-rbx[range,'Close'],
      prfilesub[range,'Close'])

#2 价差 配对交易
#psub 价差从160下降到100
#做空hc  做多rb   
#rb收益0.08390564 - hc 收益率 0.06299886 = 0.02090678
#hc 收益率 0.06299886
coredata(hcx[nrow(hcx),'Close']) / coredata(hcx[1,'Close'])-1
#rb 收益率 0.08390564
coredata(rbx[nrow(rbx),'Close']) / coredata(rbx[1,'Close'])-1
