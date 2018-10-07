# Matrices
# Data Frames
# Lists

getwd()


# IN THIS SESSION WE'LL LEARN ABOUT TYPES OF 
# OBJECTS AND DATA CONTEINERS IN R


### MATRIX ####

# is the most basic data structure in R, allowing only for 
# numeric variables.
# Besides, all variables must be of the same length

# matrix operations are very optimised though

# let's create a matrix

m<-matrix(seq(1,12),nrow=3,ncol=4)
n<-matrix(seq(1,12),nrow=3,byrow=T)
m<-matrix(seq(1,12),nrow=3,byrow=F)

print(m)
print(n)

#View(m)

nrow(m) # number of rows
ncol(m) # number of rows

dim(m) # gets the dimension of the matrix


# colnames and rownames for naming the matrix cols and rows, always a best practice
colnames(m)<-paste0(rep('col',4),seq(1,4))
rownames(m)<-paste0(rep('row',3),seq(1,3))


# matrix accesing, filtering and subsetting:
print(m)
m[3,2] # returns element of row 3 and column 2
m[ ,1] # returns all rows of the first column (a vector)
m[1, ] # returns all columns of the first row (a vector)


m[c(1,3), ]
m[1:2,seq(2,4)]

# we can also subset a (named) matrix by column or row name
m[ ,'col3']  # when refering to its attributes names quotation is needed (simple or double quotes)
m['row1',c('col1','col3')] 

# basic matrix opeations

A <- matrix(runif(9),nrow=3,ncol=3)  # see ?runif
B <- matrix(runif(9),nrow=3,ncol=3)

print(A)
print(B)

b <- c(4,2,1)
print(b)

t(A)	# Transpose
A * B	# Element-wise multiplication
t(A) %*% B	# Matrix multiplication
crossprod(A,B)
crossprod(A)	# A'B and A'A respectively.
diag(2,5)	# Creates diagonal matrix with elements of x in the principal diagonal
diag(A)	# Returns a vector containing the elements of the principal diagonal
diag(4)	# If k is a scalar, this creates a k x k identity matrix
solve(A, b)	# Returns vector x in the equation b = Ax (i.e., A-1b)
solve(A)	# Inverse of A where A is a square matrix
y<-eigen(A)	# y$val are the eigenvalues of A
# y$vec are the eigenvectors of A
y<-svd(A)	# Single value decomposition of A
# y$d = vector containing the singular values of A
# y$u = matrix with columns contain the left singular vectors of A
# y$v = matrix with columns contain the right singular vectors of A
cbind(A,B)	# Combine matrices(vectors) horizontally (same number of rows)
rbind(A,B)	# Combine matrices(vectors) vertically (same number of columns)
rowMeans(A)	# Returns vector of row means
rowSums(A)	# Returns vector of row sums
colMeans(A)	# Returns vector of column means
colSums(A)	# Returns vector of column sums


# cbind, rbind, rowMeans/colMeans and  rowSums/colSums are also used within data.frames (matrices on steroids)


# you can also operate between a single number and a matrix

A+2
A-2
A*2
A/2
A^2
log(A)



### DATA.FRAME ####

# the most used data type in R, similar to matrix but allows for different types of variables
# all variables must be of the same length

# you can transform a matrix into a data.frame easily:

str(m)
m<-as.data.frame(m)
str(m)

# let's create a data frame with variables of different classes

n <- c(2, 3, 5) 
s <- c("aa", "bb", "cc") 
b <- c(TRUE, FALSE, TRUE) 
df <- data.frame(n, s, b)  
colnames(df)<-c('v_1','v_2','v_3')
df

# the str() function describes the structure of the data.frame

str(df)

# by the simbol $ we can access any of the variables in the data frame (always by column)

df$v_1

# matrix subsetting is also allowed

df[ ,1]
df[3,2]

# importing a native R dataset (just by calling data(x)...see library(help = "datasets"))
data("mtcars")

print(mtcars)

head(mtcars,n=10)  # show only the first 10 rows
tail(mtcars,n=10)  # show only the last 10 rows

str(mtcars)

summary(mtcars) # gives a summary of each individual variable

names(mtcars)
colnames(mtcars)
rownames(mtcars)

dim(mtcars)
nrow(mtcars)
ncol(mtcars)


mtcars$mpg

# with $ we can create a new variable

mtcars$model<-rownames(mtcars)

# to remove a variable we can either assign it a NULL value

mtcars$model<-NULL

# or explicitly eliminate it by its position
mtcars$model<-rownames(mtcars)
mtcars<-mtcars[,-12]
mtcars$model<-rownames(mtcars)
mtcars<-mtcars[,-ncol(mtcars)]

# accesing, filtering and subsetting (and slicing)

head(mtcars)
mtcars[1,1] 
mtcars[ ,1]  
mtcars[1:3, ]
mtcars[c(1,3,5),c(2,4,6)]
mtcars[,'mpg']
mtcars['Toyota Corolla','hp']

mtcars[1] # if we only specified one argument it will return a slice of the data.frame 
mtcars[c(7,3)]
mtcars['mpg']
mtcars[c('mpg','wt')]

#subseting through logical conditions

mtcars[c(rep(F,12),rep(T, 20)), c(T,rep(FALSE, ncol(mtcars)-1))]

# mtcars[cyl>6, ] # gives an error, we need to call the cyl variable with $ 
mtcars[mtcars$cyl>6,] 

mtcars[mtcars$mpg>15&mtcars$hp>150,c('wt','disp')]

subset(mtcars,hp>100&hp<200) # can use the subset function for simplicity of reading

subset(mtcars,subset=wt>=4,select=c('drat','mpg','wt'))
subset(mtcars,subset=wt>=4,select=1:5)
subset(mtcars,subset=wt>=4,select=-c(1:5))


# ordering a data.frame acording to one variable
mtcars[order(mtcars$mpg),]  # ascending order
mtcars[order(-mtcars$mpg),] # descending order


mtcars[order(mtcars$carb,mtcars$mpg),] # ordering by two variables

# aggregated operations (group by)
str(mtcars)

aggregate(x=mtcars,FUN = mean, by=list(mtcars$carb))
aggregate(x=mtcars,FUN = max, by=list(mtcars$carb))

aggregate(x=mtcars,FUN = mean, by=list(carb=mtcars$carb,vs=mtcars$vs))


# merging tables

data(mtcars)
mtcars$model<-rownames(mtcars)

b<-data.frame(model=mtcars$model,new_variable=rnorm(nrow(mtcars)))  # see ?rnorm

head(b)

c<-b[1:10, ]

mtcars2<-merge(x=mtcars,y=b,by='model')   # inner join
print(mtcars2)

mtcars3<-merge(x=mtcars,y=c,by='model')  # inner join
print(mtcars3)

mtcars4<-merge(x=mtcars,y=c,all.x=TRUE, by='model')  # left join
print(mtcars4)

colnames(c)[1]<-'modelo'
# mtcars4<-merge(x=mtcars,y=c,all.x=TRUE, by='model')  # left join
mtcars4<-merge(x=mtcars,y=c[1:10,],all.x=TRUE, by.x='model',by.y='modelo')
print(mtcars4)

c$new_gear<-sample(c(3,4),nrow(c),replace = T)

mtcars5<-merge(x=mtcars,y=c,by.x=c('model','gear') ,by.y=c('modelo','new_gear'))
print(mtcars5)

# rbind & cbind

cbind(mtcars,mtcars)
rbind(mtcars,mtcars)


cbind(id=seq(1,nrow(mtcars)),mtcars)

mtcars<-rbind(mtcars,mtcars[nrow(mtcars),])

mtcars

# duplicates

# duplicated & unique operate at a row level

duplicated(mtcars)
unique(mtcars)

unique(mtcars$cyl)
length(mtcars$disp)-length(unique(mtcars$disp))

mtcars[duplicated(mtcars), ]
mtcars[!duplicated(mtcars), ]

# NA treatment
# complete.cases operates at a row level
print(mtcars4)
complete.cases(mtcars4)
mtcars4[complete.cases(mtcars4), ]

# is.na operates at a element-wise level
is.na(mtcars4)  # applies to each position of the data.frame
mtcars4[is.na(mtcars4$new_variable)==TRUE, ] # applies to each element of the specified row
mtcars4[is.na(mtcars4$new_variable)==FALSE, ]
mtcars4[is.na(mtcars4$new_variable), ]
mtcars4[!is.na(mtcars4$new_variable), ]


# vectorized operations
data(mtcars)

apply(mtcars,MARGIN=2,FUN=mean) # mean operation by column
# same as
colMeans(mtcars)

apply(mtcars,1,sum)  # sum operation by row
# same as
rowSums(mtcars)

apply(mtcars,c(1,2),mean)

df<-data.frame(matrix(rnorm(10^6*100),nrow=10^6))
dim(df)

ini<-Sys.time()

a<-colMeans(df)

end<-Sys.time()
print(end-ini)

ini<-Sys.time()

b<-apply(df,2,mean)

end<-Sys.time()
print(end-ini)

plot(a,b)



### writing and reading tables ####

# write.csv/write.table for matrices and data.frames
write.csv(x = m, file = 'example_matrix.csv')


# load an (internal) R dataset (different to mtcars)
data("airquality")
head(airquality)

write.table(airquality,'example_data.frame.csv',row.names=FALSE, na="NA",quote= FALSE, sep=",")
write.csv(airquality,'example_data.frame.csv')

# recommended: data.table::fwrite()

library(data.table)
fwrite(airquality,'example_data.frame.csv',row.names=FALSE, na="NA",quote= FALSE, sep=";")


# reading the csv 
head(a<-read.csv('example_data.frame.csv',sep=","))
str(a)

a<-data.table::fread('example_data.frame.csv')
str(a)


### LISTS ####

# lists represent the most generic and flexible data containers in r
# a list is a set of ordered (and maybe named) objects of different type
# lists cab be composed of a mix of matrices, data.frames, other lists, and even model objects


a<-list(letters=letters[1:10],df=mtcars,x=1)

str(a)

# you can access list elements by using $, but only if the elements are named
# through [[]] we can access elements by name and by its position in the list
# besides, with [] we get a slice of the original list

# accesing elemnts in a list

# named lists: 
a$letters
a[['letters']]
a[[1]]

# unnamed lists
a[[1]]

# once you retrieve any element of the list, you work with that element according to its class

a[[1]][5]
a$df['cyl'>=3,'mpg']

a[['letters']][1]
a[[2]]['mpg']

# with single square brackets, you create a slice of the original list (returning a new list)
a[c(1,3)]  # sub list con el primer y segundo objeto de la list a
a['letters']

# lapply is a very powerful function to make vectorized operations over a list
lapply(a, class)
lapply(a, summary)


### writing and reading list (and any kind of object) ####
getwd()
saveRDS(a,'prueba.Rda')
readRDS('prueba.Rda')

