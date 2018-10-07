### HELLO WORLD!!! 

# ...this is our first R Script!!


# this symbol (#) allows us to comment a line, so this line wont be run by R
# other wise an error would arise if we just wrote it plain


# Introduction to RStudio
# Console Statements
# Creating, Saving and Loading R Scripts
# Variable assigment
# Types of variables


#### WORKING DIRECTORY ###


getwd() # our first r function
#setwd() # to specify any path as working directory

list.files()
list.dirs('/Users/ssobrinou/Documents/')


#### VARIABLE ASSIGNMENT ####

x <- 1
x
print(x)  

y<- x + 1
print(y)


#### TYPES OF VARIABLES ####

# 5 basic type of variables in R: numeric, integer, logical, character and factor

# coertion methods in R allow us to easily swicth the nature of a variable
# as.numeric(), as.integer(), as.logical(), as.character(), as.factor()

# class() tells us the type of a variable/object


# numeric
x<-1.3    
class(x)
y=9
class(y)

# normally you will work with vectors
x<-c(3,2,4,5,1,6,3,9,2,7,3,6,5,7,8,1,2)  # the c() function combines elements creating a vector
x
class(x)

# integer
x<-1
class(x)
x<-as.integer(x)
print(x)
class(x)
x<-1L
class(x)


# logical
x<-TRUE
print(x)
class(x)
x<-c(T,F,T)
print(x)
class(x)

x<-as.numeric(x)
print(x)
class(x)

x<-as.logical(x)
print(x)
class(x)


# character
x<-'hello world!!'
print(x)
class(x)
x<-'1'
class(x)

x<-as.integer(x)
class(x)

# factor: used for cathegory grouping in some analysis (MALE/FEMALE, CONTINENT, LEVEL OF STUDIES)
x<-factor(c('A','B','A','C','B','A'),levels = c('A','B','C'))
class(x)
print(x)
plot(x)  # our first plot! as x is a factor R will bar-plot the counting of each level frome the factor variable 

x<-as.character(x) # from factor back to character, useful sometimes
x
# plot(x)  # R doesn't plot characters 

x<-as.numeric(x)
x  # cannot coerce from character to numeric



#### VECTORS AND BUILT-IN FUNCTIONS ####

# c()  combines elements creating vector. Can combine vectors too

x<-c(1,2,3,4,5)

# elemnts must be of the same class, otherwise c() forces to a feasible common class

x<-c(1,2,'a',TRUE)
print(x)
class(x)

# a:b generates a sequence from a to b with unitary step

x<-1:5

# seq(from, to, by) generates a sequence from a to b with step specified with the by argument
# type help(seq) or ?seq for further information


x<-seq(from=2,to=10,by=2)  # arguments in functions are always assigned with =

# x[n] returns the nth position in a vector
x[3]
# [n:m] returns positions n to m in a vector
x[2:4]
# [c(n,m)] returns positions n and m in a vector
x[c(1,3,5)]
x[c(1,3:5)]
x
x[c(2,1,3,4,5)]

seq(0,10,length.out = 12)
seq(12,-3,-5)

length(x) # gives the length of a vector

# sort(x, decreasing=FALSE), sorts a vector in decreasing or increasing mode

sort(x)
sort(x, decreasing = T)

# order() returns the positions of the vector elements in the desired order
# rather then the value of the ordered vector (like in sort())
order(x)
x<-c(60,55,21,13,47)
x
order(x)
order(-x)

x[order(x)] # equivalent to sort(x)


# rep(x,times) replicates x, n times
rep(3,5)

# min/max Vs which.min/which.max returns the position of the vector with min/max value

x<-c(60,55,21,13,47)

min(x)
max(x)

which.min(x)
which.max(x) # which.min/which.max returns the position of the vector with min/max value


x[which.min(x)] # equivalent to min(x)

# mean, median, var, sd ...
mean(x); median(x); var(x); sd(x)

# summary: applied to a numeric vector, returns a basic statistic summary
summary(x)  

summary(z<-c(2,3,NA))

y<-rep(1,length(x))

z<-x+y
print(z)

z+2

z+rep(2,4)

sum(y) # adds up the elements of a vector, resulting in a single number
sum(x,y) # adds the elements of both vectors, resulting in a single number
x+y # element wise adding, resulting in another vector

# is.na returns logic values TRUE/FALSE when missing values found

x<-c(60,55,21,13,47)
is.na(x)

y<-c(78,NA,34,NA,NA)
is.na(y)
as.numeric(is.na(y))


# think...how could we easily count the number of NA's in a vector?
sum(is.na(x))
sum(is.na(y))

# logic operators

x<-1
y<-2

x==2
x!=2

a=TRUE
!a
b=FALSE
!b

x>=0 & y<=3
x>=0 | y<=3
x>=0 && y<=3

z<-c(1:10)

z%%2==0 & z<=6
z%%2==0 && z<=6

y<-c(rep(3,4),2,1,rep(5,4))
length(z)==length(y)
print(z)
print(y)
z%%2==0 | y>4

help('&&')

# think...how could we easily check for the presence of any NA in a vector?
y<-c(78,NA,34,NA,NA)
sum(is.na(y))>0


# basic functions for characters

# c, rep, sort and order work as well

c('a','b','c')

rep('vcniqvncq',10)

x<-letters[1:10]

sort(x)
sort(x,decreasing = T)

order(x)


# paste(x,y,z,sep=';') concatenates text with a specified separator
# paste0 concatenates text without any separator

paste('a','b','c',sep=' ')
paste('a','b','c',sep=',')

paste0('a','b','c')

a<- 'R is a great software'

substr(a,1,1)
substr(a,10,15)
substr(a,14,50)

b <- c('Eddy','Shawn','Fred')

grep('S',b)
grep('S',b, value=T)

grep('dd',b, value=T)
