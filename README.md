### 使用 
获取股票数据
```r
> s <- getAllStocks()
> head(s$SH)
> head(s$SZ)
> getStock("SH_600000") 
[1] "SH_600000"
> getStock("SZ_000001") 
[1] "SZ_600000"
```
获取期货合约数据(本地MySQL)
```r
> getFuture(code='hc_f',constact = '1801',dbuser = 'foo',dbpwd = 'foo') 
[1] "hc_f_1801"
> getContacts('hc_f','root','111111') #return top 15 volumn
```


