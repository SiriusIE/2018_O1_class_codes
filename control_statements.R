#### CONDITIONALS ####
# if() {} 
# if() {} else {}
# if() {} else if () {} else {}

a <- 1
a

if(2>1){
  a <- a + 5
}

a

if(2>1) {
  print('greater')
} else {
  print('smaller')
}


x<-45
y<-2

if(x==y){
  print('x equals y')
} else if (x>y){
  print('x greater than y')
} else {
  print('x smaller than y')
}

if(length(ls())>0){
  rm(list=ls())
}

if(!'data.table'%in%installed.packages()){
  install.packages('data.table')
} else {
    print('data.table is already installed')
}

if(!'data.table'%in%installed.packages()){
  install.packages('data.table')

} else {
  print('data.table is already installed')

}
library(data.table)


setwd("/Users/ssobrinou/IE")
if(getwd()!="/Users/ssobrinou/IE/Class Codes"){
  setwd("/Users/ssobrinou/IE/Class Codes")
  
}

print(getwd())


m<-matrix(runif(12), ncol=3)
m

n<-matrix(runif(12), nrow=4)
n

if('j'%in%ls()) { rm(j) }

if(ncol(m)==nrow(n)){
  j<-m%*%n
} else {
  print("Error: incorrect dimensions for matrix multiplication")
}

if('j'%in%ls()) { print(j) }


# if can't be directly applied to a vector
x<-seq(1,10)
if(x%%2==0){
  'even'
} else {
  'odd'
}

# vectorised ifelse function:

?ifelse

x
ifelse(x%%2==0,yes='even',no='odd')
ifelse(test=x%%2==0, yes=paste0(x,' is even'), no=paste0(x, ' is odd'))

x<-sample(100000)
y<-ifelse(test=x%%13==0, yes=paste0(x,' is divisible by 13'), no=paste0(x, ' is not divisible by 13'))
y
y<-ifelse(test=x%%13==0, yes=TRUE, no=FALSE)
y

sort(x[which(y==TRUE)])


data(mtcars)

df<-data.table(mtcars)

str(df)


df[, powerful_car:=ifelse(hp>=150, TRUE, FALSE)]
df$powerful_car<-ifelse(df$hp>=150, TRUE, FALSE)

str(df)

df[, cyl_class:=ifelse(cyl>=6, '>6', '<=6')]
df


df[, cyl_class:=as.factor(ifelse(cyl>=6, '>6', '<=6'))]
str(df)
plot(df$cyl_class)


if(!'lubridate'%in%installed.packages()){
  install.packages('lubridate')
  library(lubridate)
} else {
  print('lubridate is already installed')
  library(lubridate)
}

print(now())

### exercise: 
# using a conditional structure, write a program that prints "good morning!" if the
# actual hour is in the interval [6, 12), "good afternoon!" if 
# it's in [12, 17), "good evening!" if it's in [17, 22) or 
# "good night!" if its in [22,6)



# FOR LOOPS ####

# applies a statement iteratively an arbitrary number of times, until a specified counter ends

for(i in 1:5) print(1:i)

for (i in 1:1000) {
  print(sqrt(i))
  
}

df<-iris
for ( i in 1:ncol(df)) {
  print(colnames(df)[i])
  print(class(df[,i]))
  
}

df<-read.csv('../df_states.csv', sep=';')
str(df)

for (gewfdbwe in names(df)) {
  print(class(df[,gewfdbwe]))
  
}


x<-seq(1:10)
results<-numeric()
for ( s in c(1, 3, 5)) {
  results[s]<-if(x[s]%%2==0) {'even'} else {'odd'}
}


### exercise: 
# read the df_states.csv dataset with the option stringsAsFactors=F
# making use of a for loop and a conditional statement, change any character variable to be a factor



# load the state dataset
data(state)
str(state.x77)

# lets create an algorithm that returns a vector, of length number of
# variables in state.x77, containing the state name with each higher value

winner<-character(ncol(state.x77))
names(winner)<-colnames(state.x77)

for (i in colnames(state.x77)){
  winner[i]<-rownames(state.x77)[which.max(state.x77[,i])]
}

winner

# many times we can achive this task by using much more simple vectorised functions

winner<-rownames(state.x77)[apply(state.x77,2,which.max)]
names(winner)<-colnames(state.x77)
winner

# using a for loop to programatically save some plots

for(i in 1:10){
png(filename = paste(paste('plot',i, sep='_'), 'png', sep='.'))
plot(hist(rnorm(1000)))
dev.off()
}


# double for loops are very common; they are needed when you have two posible iterators

# lets create the matrix of euclidean distances between all pairs of states

data(state)

data_center<-as.matrix(cbind(state.center$x,state.center$y))
colnames(data_center)<-c('x','y')
rownames(data_center)<-state.abb

data_center[1:10, ]

# we first create and emply matrix called distances to store the results
distances<-matrix(NA, nrow=nrow(data_center),
                  ncol=nrow(data_center))
rownames(distances)<-rownames(data_center)
colnames(distances)<-rownames(data_center)
distances[1:10,1:10]

# and iterate by row and by column, through every 
# pair of states, calculating their center distance, which by the way, is symmetrical

for (i in 1:nrow(distances)){
  for(j in 1:ncol(distances)){
    distances[i,j]<-sqrt((data_center[j,'x']-data_center[i,'x'])^2 + (data_center[j,'y']-data_center[i,'y'])^2)
   
  }
}
distances[1:10,1:10]
View(distances)


# lets ckech ans example

sort(distances['NY',],decreasing=F)


# breaking a for loop

for ( i in 1:10) {
  
  if(i==5){
    break
  }
  
  print(sqrt(i))
  

}


# skipping an iterationm, but continuing on the loop

for ( i in 1:10) {
  
  if(i==4){
    next
  }
  
  print(sqrt(i))
  
  
}


# WHILE LOOPS ####

# in while loops, you iterate a statement until some logical condition is met

k=0
while( k <= 10 ){
  print(sqrt(k))
  # k<-k+1
}


# apending through a for loop

df<-data.table()
df


for(i in 1:1000){
  df<-rbind(df,data.table(x=rnorm(1), y=runif(1), z=rnorm(1)))
}

df

# LAPPLY FAMILY OF FUNCTIONS ####
class(distances)


# apply: operates a function over the table margins: 1 for row and 2 for columns 
apply(distances,2,mean)
apply(distances,1,mean)


# lapply: operates a function over a list by default, or over a data.frame. Returns a list
# when applied to a data.frame, it applies the function to each column of the data.frame

df_states<-read.csv('../df_states.csv', sep=';')
saveRDS(df_states,'df_states.RData')
df_states<-readRDS('df_states.RData')

str(df_states)

lapply(df_states, class)
lapply(df_states, summary)


df<-data.frame(distances)
df$max_distance<-lapply(df, max)

a<-list(a=letters[10],
        b=runif(50),
        c=data.frame(x=1:3,y=5:7))

lapply(a,class)
lapply(a,summary)

lapply(a,mean)  # will return NA whenever the operation  is not allowed over the element class



# sapply does the same as lapply, but returns output in a simplified object (vector or matrix), if possible
lapply(df_states,class)
sapply(df_states,class)

sapply(df_states,is.numeric)
lapply(df_states[ ,names(df_states)[sapply(df_states,is.numeric)]],summary)





