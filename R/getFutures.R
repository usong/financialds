######################
# getContacts
# code
######################
"getContacts" <-
  function(code,
           dbuser,
           dbpwd,
           src = "MySQL",
           env = parent.frame()) {
    l <- do.call(
      paste("getContacts", src, sep = "."),
      list(
        code   = code,
        dbuser = dbuser,
        dbpwd  = dbpwd,
        env    = env
      )
    )
  }

######################
# ggetContacts.MySQL function
#
######################
"getContacts.MySQL" <-
  function(code,
           dbuser,
           dbpwd,
           dbname = "future_db",
           host = "localhost",
           src,
           env) {
    if (!require("RMySQL", quietly = T))
      stop("package:", dQuote("RMySQL"), "cannot be loaded.")
    
    options(warn = -1)
    op <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)
    con <- dbConnect(
      MySQL(),
      user = dbuser,
      password = dbpwd,
      dbname = dbname,
      host = host
    )
    on.exit(dbDisconnect(con))
    
    sql <-
      paste(
        "SELECT deliverymonth AS contact, SUM(volume) AS volume\
        FROM (\
        SELECT *\
        FROM future_daily p\
        WHERE p.deliverymonth IN (\
        SELECT c.deliverymonth\
        FROM future_daily c\
        WHERE c.productid=",
        sQuote(code),
        "GROUP BY c.deliverymonth) AND p.productid=",
        sQuote(code),
        "ORDER BY p.deliverymonth,p.date\
        ) AS hc_f\
        GROUP BY deliverymonth\
        HAVING SUM(volume)\
        ORDER BY SUM(volume) DESC\
        LIMIT 15;"
        )
    #message(sql)
    rs <- dbSendQuery(con, sql)
    chunk <- NULL
    
    while (!dbHasCompleted(rs)) {
      chunk <- rbind(chunk , dbFetch(rs, 10))
    }
    
    return(chunk)
    
  }
######################
# getfuture function
# code
######################
"getFuture" <-
  function(code,
           constact,
           dbuser,
           dbpwd,
           src = "MySQL",
           env = parent.frame()) {
    #set in GlobalEnv
    #if( sys.parent() == F ) { env = sys.frame }
    l <- do.call(
      paste("getFuture", src, sep = "."),
      list(
        code   = code,
        contact = constact,
        dbuser = dbuser,
        dbpwd  = dbpwd,
        env    = env
      )
    )
    return(l)
  }

######################
# getFuture.MySQL function
#
######################
"getFuture.MySQL" <-
  function(code = NULL,
           contact = NULL,
           dbuser,
           dbpwd,
           dbname = "future_db",
           host = "localhost",
           encoding = "GBK",
           env) {
    if (!require("RMySQL", quietly = T))
      stop("package:", dQuote("RMySQL"), "cannot be loaded.")
    if (!require("xts", quietly = T))
      stop("package:", dQuote("xts"), "cannot be loaded.")
    
    options(warn = -1)
    op <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)
    con <- dbConnect(
      MySQL(),
      user = dbuser,
      password = dbpwd,
      dbname = dbname,
      host = host
    )
    on.exit(dbDisconnect(con))
    
    sql <-
      paste(
        "SELECT DATE, open,high,low, close,presettlement,settlement,zd1,zd2,volume\
        FROM (\
        SELECT *\
        FROM future_daily p\
        WHERE p.deliverymonth IN (\
        SELECT c.deliverymonth\
        FROM future_daily c\
        WHERE c.productid=",
        sQuote(code),
        "GROUP BY c.deliverymonth) AND p.productid='hc_f'\
        ORDER BY p.deliverymonth,p.date\
        ) AS hc_f\
        WHERE deliverymonth=",
        sQuote(contact),
        "ORDER BY DATE;"
        )
    #message(sql)
    rs <- dbSendQuery(con, sql)
    chunk <- NULL
    
    while (!dbHasCompleted(rs)) {
      chunk <- rbind(chunk , dbFetch(rs, 10))
    }
    
    colpadding <- c('open', 'high', 'low')
    padnums <- nrow( chunk[chunk[, 'open'] == 0, ][, colpadding] )
    if( padnums > 0 ){
      chunk[chunk[, 'open'] == 0, ][, colpadding]  <-
        chunk[chunk[, 'open'] == 0, ][, 'close']
    }
    
    fr <- xts(chunk[, -1], as.Date(chunk[, 1]))
    info_cotact <- paste(code, contact, sep = "_")
    assign(info_cotact,
           fr,
           env)
    return(info_cotact)
  }
#getFuture(
#  code = 'hc_f',
#  constact = '1710',
#  dbuser = 'root',
#  dbpwd = '111111'
#)
