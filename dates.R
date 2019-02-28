# standard date and timestamp format: 

Sys.Date()  # date

Sys.time()  # timestamp


df<-data.frame(year=2010:2019, month=1, day=1)

df
str(df)

# lets create a date variable

df$date<-as.Date(paste(df$year, df$month, df$day, sep='-'))

# and a time variable
df$hour<-sample(0:23, nrow(df))
df$min<-0
df$sec<-0
df
df$time<-paste(df$date,paste(df$hour, df$min, df$sec, sep=':'), sep=' ')
df
# use the strptime function to convert to a timestamp a character sequence
df$time<-strptime(paste(df$date,paste(df$hour, df$min, df$sec, sep=':')),format='%Y-%m-%d %H:%M:%S')
df
str(df)

df$date<-as.Date(df$time)

# lubridate package, great for working with dates

df<-df['time']

df

# you can extract year, month, day, hour... with lubridate functions:
library(lubridate)
df$year<-year(df$time)
df$month<-month(df$time)
df$day<-day(df$time)
df$hour<-hour(df$time)
df$wday<-wday(df$time)  # also look at weekdays (from base R)

