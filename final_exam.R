# 1. 
# Load the iris dataset. 
# Clusterize the entities (flowers) using all numeric variables, using Kmeans and using hierarchical clustering (with Ward D2 algo)
# Analize the results comparing both clusters membership with the real Species. Analize the missclasification
# of species numerically and visually








# 2. 
# Create a report where you analize the relationship between the average 
# daily return and the volatility (sd), for each symbol.
# Is (daily) profitability related with (daily) volatility?
# Wich are the best portfolio stocks, in terms of the ratio mean/sd ?
# Create a suitable clustering of companies, using the mean daily return 
# and the sd of daily return as the spliting variables. Analize the results.
# Create a simple linear model that explains volatility with mean return and
# analize the results












library(nutshell)

data(dow30)

df<-dow30


f_return<-function(x){
  y<-numeric()
  y[1]<-NA
  for(j in 2:length(x)){
    y<-c(y,(x[j]/x[j-1]-1))
  }
  return(y)
}

for(i in levels(unique(df$symbol))){
  print(i)
  df[df$symbol==i,'daily_return']<-f_return(df[df$symbol==i,'Close'])
}


df$daily_return<-NA
for(i in levels(unique(df$symbol))){
  print(i)
  for(j in 2:nrow(df[df$symbol==i,])){
    df[df$symbol==i, ][j,'daily_return']<-df[df$symbol==i, ][j, 'Close']/df[df$symbol==i, ][j-1, 'Close']-1
  }
}
head(df)
str(df)

library(ggplot2)

ggplot(df, aes(x=as.Date(Date), y=daily_return, colour=symbol))+geom_line()

