library(quantmod)
library(data.table)

library(ggplot2)
theme_set(theme_minimal(base_size=16))
options(scipen = 999)

tickers<-data.table(readxl::read_excel('nasdaq_symbols.xlsx'))$Symbol

start <- as.Date("2010-01-01")
end <- as.Date("2018-11-30")


df<-data.frame()


ini<-Sys.time()
for(i in seq_along(tickers)){
  print(tickers[i])
  stock_i<-data.frame(getSymbols(tickers[i], from=start,to=end, auto.assign = F,src = "yahoo"))
  setnames(stock_i,c('open','high','low','close','volume','adj'))
  df<-data.table(rbind(df,data.frame(date=as.Date(rownames(stock_i)),company=tickers[i],stock_i)))
}
print(Sys.time()-ini)


df
unique(df$company)

ggplot(df, aes(x=as.Date(date), y=close, colour=company))+geom_line(show.legend = F)




f_return<-function(x){
  y<-numeric()
  y[1]<-NA
  for(j in 2:length(x)){
    y<-c(y,(x[j]/x[j-1]-1))
  }
  return(y)
}


ini<-Sys.time()
for(i in levels(unique(df$company))){
  print(i)
  df[df$company==i,daily_return:=f_return(df[df$company==i][['close']])]
}
print(Sys.time()-ini)

df

ggplot(df, aes(x=as.Date(date), y=daily_return, colour=company))+geom_line(show.legend = F)


mean_sd<-df[, .(mean_return=mean(daily_return, na.rm = T), 
       sd_return=sd(daily_return, na.rm = T)), by=company]

mean_sd
ggplot(mean_sd, aes(x=mean_return, y=sd_return, colour=company))+geom_point(show.legend = F)

mean_sd[, cluster_k:=factor(kmeans(mean_sd[, sapply(mean_sd, is.numeric), with=F], 7)$cluster)]
ggplot(mean_sd, aes(x=mean_return, y=sd_return, colour=cluster_k))+geom_point(show.legend = F)

