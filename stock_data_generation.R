library(quantmod)
library(data.table)

stocks<-c('FB','AMZN','AAPL','JPM','NFLX','GOOG')

start <- as.Date("2000-01-01")
end <- as.Date("2018-11-15")

for(i in seq_along(stocks)){
  print(stocks[i])
  df<-data.frame(getSymbols(stocks[i], from=start,to=end, auto.assign = F))
  df<-cbind(date=as.Date(rownames(df)), df)
  colnames(df)<-c('date','open','high','low','close','volume','adj')
  rownames(df)<-NULL
  
  fwrite(df, paste0('stocks/data_',stocks[i],'.csv'))
}

head(df)
str(df)

# real all data in the stocks folder and create a single data.frame containing 
# all the information, with a column indicating the stock name

stocks<-c('FB','AMZN','AAPL','JPM','NFLX','GOOG')
rm(df)

df<-data.frame()

for(i in seq_along(stocks)){
  print(stocks[i])
  if(stocks[i]=='JPM') next
  df<-rbind(df,data.frame(stock=stocks[i],read.csv(paste0('stocks/data_',stocks[i],'.csv'))))
}

df$date<-as.Date(df$date)
str(df)
df<-df[df$date>='2010-01-01',]

library(ggplot2)
theme_set(theme_minimal(base_size = 16))

ggplot(df, aes(x=date, y=close, colour=stock))+geom_line()

# read the df_states csv
# for each variable if there's a missing value, impute it as the average value
# of the variable sample without NAs

df_states<-read.csv('../df_states.csv', sep=';', stringsAsFactors = T)
df_states
sapply(df_states, function(x) sum(is.na(x)))


result_1<-data.frame(lapply(df_states, function(x) {for( j in 1:length(x)){
 
  if(is.na(x[j])){
    x[j]<-mean(x, na.rm=T)
  }
} 
  return(x)}
))

str(result_1)

sapply(result_1, function(x) sum(is.na(x)))


result_2<-copy(df_states)

for( i in seq_along(result_2)){
  Sys.sleep(0.1)
  print(colnames(result_2)[i])
  for( j in 1:nrow(result_2)){
    if(is.na(result_2[j,i])){
      result_2[j,i]<-mean(result_2[,i], na.rm=T)
    }
  }
  
}

result_2

sapply(result_2, function(x) sum(is.na(x)))

df_states
