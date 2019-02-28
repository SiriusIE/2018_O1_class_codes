# 1.
# Download the folder "stocks" from the campus (content area "datasets")
# Create a vector with stocks names as follows: stocks<-c("FB","AMZN","AAPL","NFLX","GOOG")
# Read all data in the stocks folder only when the csv name contains the symbols in the stocks vector, 
# and create a single data.frame with all the information, adding a column indicating the stock name.
# Create a line plot (time series plot) with the daily dates on the x axis and the stocks close price
# in the y axis, but only with data from 2010-01-01 and ahead.


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






# 2.
# Read the df_states csv
# For each variable in df_states, if there's a missing value, 
# impute it as the average value for that variable without NAs


df_states<-read.csv('../df_states.csv', sep=';', stringsAsFactors = T)
df_states
sapply(df_states, function(x) sum(is.na(x)))

result<-df_states

for( i in seq_along(result)){
  Sys.sleep(0.1)
  print(colnames(result)[i])
  for( j in 1:nrow(result)){
    if(is.na(result[j,i])){
      result[j,i]<-mean(result[,i], na.rm=T)
    }
  }
  
}

head(result)

sapply(result, function(x) sum(is.na(x)))

