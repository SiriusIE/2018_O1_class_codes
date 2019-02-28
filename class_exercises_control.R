# 1.
library(lubridate)
if(hour(now())>=6&hour(now())<12){
  print("good morning!")
} 

x<-hour(now())
print(x)

if(x>=6&x<12){
  print("good morning!")
} else if (x>=12&x<17){
  print("good afternoon!")
} else if(x>=17&x<22){
  print("good evening!")
} else {
  print('good night!')
}



# 2. 
df<-read.csv('../df_states.csv',sep=';', stringsAsFactors = F)
str(df)


for(i in colnames(df)){
  if(class(df[,i])=='character'){
    df[, i]<-as.factor(df[, i])
  }
}

str(df)