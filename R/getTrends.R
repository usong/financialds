######################
# getTrends function
#
######################
"getTrends" <-
  function(symbol , intervals = c(9)) {
    #基于价格找局部高点/低点 在K时间段内范围
    #时间分段  分段Index用Matrix存储
    #按时间段划分 每段内的高点，低点
    #通过对plow phigh 的K时间段分别设置(3,4,5,6,7,8,9)
    #K=9时拟合相对较好
    prehigh = 0
    wrt  = get(symbol)
    dates = index(wrt)
    phigh <- plow <- NULL
    
    for (interval in intervals) {
      mod = length(dates) %% interval
      if (mod) {
        dates <- c(dates , rep(NA , interval - mod))
      }
      md <- matrix(dates, ncol = interval , byrow = T)
      
      for (i  in 1:NROW(md)) {
        nalen = length(md[i, is.na(md[i,])])
        if (nalen) {
          interval = interval - nalen
        }
        if (interval < 3)
          next
        
        #每段的高点/低点
        for (j in 2:(interval - 1)) {
          #if (!is.na(md[i, j])) {
          if (md[i,!is.na(md[i,])]) {
            #pre high low
            tph  = as.numeric(wrt[as.POSIXct(md[i, j - 1] , origin = "1970-01-01") , 'High'])
            tpl  = as.numeric(wrt[as.POSIXct(md[i, j - 1] , origin = "1970-01-01") , 'Low'])
            
            #next high low
            tnh  = as.numeric(wrt[as.POSIXct(md[i, j + 1] , origin = "1970-01-01") , 'High'])
            tnl  = as.numeric(wrt[as.POSIXct(md[i, j + 1] , origin = "1970-01-01") , 'Low'])
            #cur high  low
            th    = as.numeric(wrt[as.POSIXct(md[i, j], origin = "1970-01-01") , 'High'])
            tl    = as.numeric(wrt[as.POSIXct(md[i, j], origin = "1970-01-01") , 'Low'])
            
            #局部高点 优化相邻bar 相同高点
            if ((th  >  tnh  &&  tph <  th) |
                (th  >= tnh  && tph <  th) |
                (th  >  tnh  &&  tph <=  th)) {
              phigh <-
                rbind(phigh , wrt[as.POSIXct(md[i, j], origin = "1970-01-01") , 'High'])
            } else if ((tl  <  tnl  && tpl  > tl)  |
                       (tl  <  tnl  && tpl  >= tl) |
                       (tl  <=  tnl && tpl > tl)) {
              #局部低点 优化相邻bar 相同低点
              plow <-
                rbind(plow , wrt[as.POSIXct(md[i, j], origin = "1970-01-01") , 'Low'])
            }
          }
        }
      }
    }
    
    phigh <- phigh[!duplicated(index(phigh))]
    plow <- plow[!duplicated(index(plow))]
    
    #波段高点 两低点中高点
    
    dates = index(plow)
    bdhigh <- NULL
    # 寻找波段高点
    j <- m <- NULL
    for (i in 1:(NROW(dates) - 1)) {
      j <- as.numeric(plow[dates[i] , 'Low'])
      m <- as.numeric(plow[dates[i + 1]  , 'Low'])
      range  = paste(format(dates[i]) ,
                     format(dates[i + 1]) ,
                     sep = "::")
      if (NROW(phigh[range])) {
        bdhigh <- rbind(bdhigh , phigh[range][which.max(phigh[range]) ,])
      }
    }
    
    
    #波段低点 两高点中低点
    dates = index(phigh)
    bdlow <- NULL
    # 寻找波段低点
    j <- m <- NULL
    for (i in 1:(NROW(dates) - 1)) {
      j <- as.numeric(phigh[dates[i] , 'High'])
      m <- as.numeric(phigh[dates[i + 1]  , 'High'])
      
      range  = paste(format(dates[i]) ,
                     format(dates[i + 1]) ,
                     sep = "::")
      if (NROW(plow[range])) {
        bdlow <- rbind(bdlow , plow[range][which.min(plow[range]) , ])
      }
    }
    
    #first point / last point repair
    if (index(first(phigh)) < index(first(plow))) {
      bdhigh <- rbind(bdhigh ,  first(phigh))
    } else {
      bdlow <- rbind(bdlow ,  first(plow))
    }
    
    if (index(last(phigh)) < index(last(plow))) {
      bdlow <- rbind(bdlow ,  last(plow))
    } else {
      bdhigh <- rbind(bdhigh ,  last(phigh))
    }
    
    return(list(
      partialhigh = phigh,
      partiallow = plow ,
      rangehigh = bdhigh,
      rangelow = bdlow
    ))
    
  }
