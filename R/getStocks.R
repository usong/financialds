######################
# getAllStocks function
# symbols  SZ/SH
######################
"getAllStocks" <-
  function(src = "eastmoney") {
    #if( is.character( markets ) ) {
    #  markets <- unlist( markets )
    #  for( each.market in markets ) {
    #    message(each.market)
    #    do.call()
    #
    #  }
    #}
    l <- do.call(paste("getAllStocks", src, sep = "."), list(markets = NULL))
  }

######################
# getAllStocks.eastmoney function
#
######################
"getAllStocks.eastmoney" <-
  function(markets = NULL, encoding = "GBK") {
    if (!require("rvest", quietly = T))
      stop("package:", dQuote("rvest"), "cannot be loaded.")
    url <- paste('http://quote.eastmoney.com/stocklist.html')
    ct <- read_html(url, encoding = encoding)
    equity.info <-
      ct %>% html_nodes("div.quotebody ul li") %>% html_text()
    equity.url <-
      ct %>% html_nodes("div.quotebody ul li a") %>% html_attr("href")
    equity.all <-
      data.frame(list = equity.info ,
                 addr = equity.url,
                 stringsAsFactors = F)

    equity.info <- lapply(equity.info, function(x) {
      str   <- gsub('[\\*|\\(|\\)]', "" , x)
      title  <- gsub('\\d{6}$', "" , str)
      code <- gsub(title, "" , str)
      return(c(code = code, title = title))
    })
    #数据清洗
    equity.info <- t(as.data.frame(equity.info))#t转置
    equity.info.df <- as.data.frame(equity.info, stringsAsFactors = F)
    row.names(equity.info.df) <- NULL
    equity.all <- cbind(equity.all[, -1],  equity.info.df)
    names(equity.all) <- c('url', 'code', 'title')

    #上证
    shs <- equity.all[grep('^60.{4}', equity.all$code), ]
    #深证
    szs <- equity.all[grep('^[00|30].{4}', equity.all$code), ]
    return(list(SH = shs, SZ = szs))
  }


######################
# getStock function
# symbols equity code
# authorityType  ba(后复权) fa(前复权)
######################
"getStock" <-
  function(symbols, src = "eastmoney", env = parent.frame(), ...) {
    if (is.character(symbols)) {
      tmp.symbols <- unlist(symbols)
      for (each.symbols in tmp.symbols) {
        l <- do.call(
          paste("getStock", src, sep = "."),
          list(each.symbols = each.symbols,
               env = env,
               ...)
        )
      }
    }
    return(symbols)
  }

######################
# getStock.eastmoney function
# each.symbols  'SH_xxxxxx'  'SZ_xxxxxx'
######################
"getStock.eastmoney" <-
  function(each.symbols,
           authorityType = "ba",
           env,
           from = "1990-01-01",
           to = Sys.Date()) {
    if (!require("rjson", quietly = T))
      stop("package:", dQuote("rvest"), "cannot be loaded.")
    if (!require("xts", quietly = T))
      stop("package:", dQuote("xts"), "cannot be loaded.")

    #tmp.id <- ifelse(grepl('^60.{4}',each.symbols ),
    #                 list(tag="1",market="SH"),
    #                 list(tag="2",market="SZ"))

    s <- unlist(strsplit(each.symbols, "_"))
    #s[1] market s[2] code
    tmp.id <- ifelse("SH" == s[1], "1", "2")
    url <-
      paste(
        "http://pdfm.eastmoney.com/EM_UBG_PDTI_Fast/api/js?",
        "rtntype=",
        5,
        "&id=",
        s[2],
        tmp.id,
        "&type=",
        "k",
        "&authorityType=",
        authorityType,
        sep = ''
      )
    #message(url)

    raw.data <-
      try(readLines(url, warn = "F", encoding = "UTF-8"), silent = T)
    if (inherits(raw.data , "try-error")) {
      warning("connect to website failed.")
      return(NA)
    }
    raw.data <- gsub('[\\(|\\)]', "", raw.data)

    json.data <- try(fromJSON(raw.data), silent = T)
    if (inherits(json.data , "try-error")) {
      warning("parse json data  failed.")
      return(NA)
    }

    if (is.null(env)) {
      stop(dQuote("local environment cann't null"))
    }
    #handle json  ochl
    mat <- matrix(0,
                  ncol = 5,
                  nrow = 0,
                  byrow = TRUE)
    for (row in json.data$data) {
      row <-  unlist(strsplit(row, ","))
      #date
      entry <- c(as.Date(row[1]))
      #ochl
      for (n in 2:5) {
        entry <- cbind(entry, as.numeric(row[n]))
      }
      mat <- rbind(mat, entry)
    }

    fr <- xts(mat[,-1], as.Date(mat[, 1], origin = "1970-01-01"))
    #ochl
    colnames(fr) <- c('Open', 'Close', 'High', 'Low')
    #na.omit(  as.numeric(fr[,-1]) )

    #re-order names
    cnames <- c('Open', 'High', 'Close', 'Low')
    corder <- pmatch(names(fr) , cnames)
    fr <- fr[, corder]
    from <- as.Date(from)
    to   <- as.Date(to)
    assign(each.symbols,
           fr[paste(from, to, sep = "/")],
           env)
    return(each.symbols)
  }

