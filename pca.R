# library(help='datasets')

library(data.table)
library(ggplot2)
theme_set(theme_minimal(base_size=16))
library(corrplot)


# Example 1: quick PCA analysis on Iris data
data(iris)
str(iris)

numeric_data<-data.table(iris[, sapply(iris,is.numeric)])
sapply(numeric_data, sd)

##################################
# manual calculation of pr. comps
##################################

# Step 1: calculate the covariance or correlation matrix. 

cov_mat<-cov(numeric_data)       
cor_mat<-cor(numeric_data)       # same as cov(scale(numeric_data))


# Step 2: calculate the eigenvectors and eigenvalues from the cov/cor matrix

eigenvalues<-eigen(cov_mat)[['values']]    # already sorted in decreasing order
eigenvectors<-eigen(cov_mat)[['vectors']]

# the eigenvectors represent (sorted decresingly) the coefficients that linearly applied to X 
# gives the set of new variables uncorrelated among them and sorted in decreasing variance

# each eigenvalue represents the variance of the corresponding new variable (eigenvector*X)

eigenvalues
sum(eigenvalues)  # the sum of eigenvalues is the total variance of the original variables
identical(sum(eigenvalues),sum(diag(cov_mat)))  # :)

eigenvalues/sum(eigenvalues)         # so each eigenvalue represents the amount of variance captured by the new variable
sum(eigenvalues/sum(eigenvalues))
cumsum(eigenvalues)/sum(eigenvalues)


# diagonalization check

identical(solve(eigenvectors)%*%cov_mat%*%eigenvectors,diag(eigenvalues))
solve(eigenvectors)%*%cov_mat%*%eigenvectors-diag(eigenvalues)
corrplot(solve(eigenvectors)%*%cov_mat%*%eigenvectors-diag(eigenvalues),is.corr = F,cl.lim = c(-0.01,0.01))


# the eigenvalues represent the fraction of the total variance in X that
# are captured by each new variable built from the eigenvectors

qplot(x=1:length(eigenvalues),y=eigenvalues, geom=c('point','line'))+xlab('')

qplot(x=1:length(eigenvalues),y=eigenvalues/sum(eigenvalues), geom=c('point','line'))+xlab('')

qplot(x=1:length(eigenvalues),y=cumsum(eigenvalues)/sum(eigenvalues), geom=c('point','line'))+xlab('')


# Step 3: multiply eigenvectors by original variables 

new_vars<-as.matrix(numeric_data)%*%eigenvectors  # we create the linear combinations that give us the new variables

dim(new_vars)
new_vars

# nice properties: 
cov(new_vars)
cov_mat
diag(cov(new_vars))
eigenvalues
diag(cov_mat)

sum(diag(cov(new_vars)))
sum(diag(cov_mat))          # :) :) :)

round(t(eigenvectors)%*%eigenvectors)   # this is why that the linear combinations
                                        # will be uncorrelated, and this happens 
                                        # because the original matrix is simetric, 
                                        # since is a covariance matrix


cor(new_vars)
corrplot::corrplot(cor(new_vars), 'number')
corrplot::corrplot(cov(new_vars), 'number',is.corr = F)


# now let's do the equivalent with prcomp or princomp functions

pc<-prcomp(numeric_data)
plot(pc)

objects(pc)

pc$sdev

pc$sdev^2

eigenvalues

pc$sdev^2/sum(pc$sdev^2)

pc$rotation

# with just the two first components +95% of the total variance is explained

biplot(pc,choices = c(1,2)); grid()


pc$rotation # the coefficients of the optimal linear combination
eigenvectors

pc$x[, c(1,2)]    # the linear combinations, the new variables




# Example 2: PCA on states data
a<-data.table(name=rownames(state.x77),state.x77)
b<-data.table(name=rownames(USArrests), USArrests)

df<-merge(a,b, by='name')
str(df)


numeric_data<-as.matrix(df[,!'name'])
rownames(numeric_data)<-df$name
numeric_data[1:10, 1:10]   # notice that the scales vary hugely

corrplot(cor(numeric_data), 'number')  # variables are highly correlated


##################################
# manual calculation of pr. comps
##################################

cor_mat<-cor(numeric_data)

eigenvalues<-eigen(cor_mat)[['values']] 
eigenvectors<-eigen(cor_mat)[['vectors']]

eigenvalues
sum(eigenvalues)  # the sum of eigenvalues is the number of variables since variables scaled to one

eigenvalues/sum(eigenvalues) 
cumsum(eigenvalues)/sum(eigenvalues)

qplot(x=1:length(eigenvalues),y=eigenvalues, geom=c('point','line'))+xlab('')

qplot(x=1:length(eigenvalues),y=eigenvalues/sum(eigenvalues), geom=c('point','line'))+xlab('')

qplot(x=1:length(eigenvalues),y=cumsum(eigenvalues)/sum(eigenvalues), geom=c('point','line'))+xlab('')

# keeping the first n eigenvalues, which acount for almost x% of the total variance
# on the dataset

n=5

a<-eigenvectors[, c(1:n)]  # we take the first n eigenvectors
b<-scale(numeric_data)     # the original X matrix (scaled sin pc applied to cor matrix)

dim(a)
dim(b)

new_vars<-b%*%a  # we create the linear combinations that give us the new variables

dim(new_vars)
new_vars


### via prcomp

pc<-prcomp(numeric_data,center = T,scale. = TRUE)
screeplot(pc)

pc$rotation   # the eigenvectors
pc$x

### via princomp

pc_2<-princomp(numeric_data, cor = T)
screeplot(pc_2)

pc_2$loadings   # the eigenvectors

par(mfrow=c(3,4))
for(i in 1:n){
plot(pc$x[,i],new_vars[,i])  # some signs are switched
}

# we continue analyzing pc

pc$sdev^2/sum(pc$sdev^2)
cumsum(pc$sdev^2/sum(pc$sdev^2))

par(mfrow=c(1,1))
biplot(pc,choices = c(1,2))

pc$rotation[, c(1,2)]

ncol(pc$rotation)
par(mfrow=c(1,1))
for(i in 2:ncol(pc$x)) {
  # Sys.sleep(0.25)
   biplot(pc,choices = c(1,i))
}



# linear model with all original variables Vs PCs

numeric_data<-as.matrix(df[, !c('name','Income')])
rownames(numeric_data)<-df$name

lm0<-lm(Income~.,data=df[,!'name'])
summary(lm0)

pc<-prcomp(numeric_data,center=TRUE,scale. = TRUE)
par(mfrow=c(1,1))
qplot(x=1:ncol(numeric_data),y=cumsum(pc$sdev)/sum(pc$sdev), geom=c('point','line'))+xlab('')

new_vars<-pc$x  # we first take all of them

new_vars_1<-data.table(Income=df$Income,new_vars)


lm1<-lm(Income~., data=new_vars_1)
summary(lm1)


new_vars_2<-data.table(Income=df$Income,new_vars[, c(1,2)])
lm2<-lm(Income~., data=new_vars_2)
summary(lm2)

pc$rotation[, c(1,2)]



###########################################
# now lets see what PCA does in some especial cases:


# totally uncorrelated variables
set.seed(53713)
xnum<-matrix(rnorm(10000), nrow=100)
xnum[1:10,1:10]
dim(xnum)
corrplot::corrplot(cor(xnum[,1:10]), 'number')


eigenvalues<-eigen(cor(xnum))[['values']]
eigenvectors<-eigen(cor(xnum))[['vectors']]

qplot(x=1:length(eigenvalues),y=eigenvalues, geom=c('line','point'))+xlab('')

qplot(x=1:length(eigenvalues),y=eigenvalues/sum(eigenvalues), geom=c('point','line'))+xlab('')

qplot(x=1:length(eigenvalues),y=cumsum(eigenvalues)/sum(eigenvalues), geom=c('point','line'))+xlab('')

eigenvalues/sum(eigenvalues) 
cumsum(eigenvalues)/sum(eigenvalues)

summary(princomp(xnum,cor=T))
plot(princomp(xnum,cor=T))
princomp(xnum,cor=T)$loadings



# very hight correlated variables
set.seed(53713)
xnum2<-matrix(rnorm(10000), nrow=100)
for( i in 2:ncol(xnum2)){
xnum2[,i]<-xnum2[,1]+rnorm(100, sd = 0.1)
}
xnum2[1:10,1:10]
dim(xnum2)
corrplot::corrplot(cor(xnum2[,1:10]), 'number')


eigenvalues<-eigen(cor(xnum2))[['values']]
eigenvectors<-eigen(cor(xnum2))[['vectors']]

qplot(x=1:length(eigenvalues),y=eigenvalues, geom=c('line','point'))+xlab('')

qplot(x=1:length(eigenvalues),y=eigenvalues/sum(eigenvalues), geom=c('point','line'))+xlab('')

qplot(x=1:length(eigenvalues),y=cumsum(eigenvalues)/sum(eigenvalues), geom=c('point','line'))+xlab('')+ylim(0,1)

eigenvalues/sum(eigenvalues) 
cumsum(eigenvalues)/sum(eigenvalues)



summary(princomp(xnum2,cor=T))
plot(princomp(xnum2,cor=T))
princomp(xnum2,cor=T)$loadings

cor(xnum)[1:10,1:10]
cor(xnum2)[1:10,1:10]

princomp(xnum,cor=T)$loadings[1:10,1:10]
princomp(xnum2,cor=T)$loadings[1:10,1:10]




#####################
# another example:

raw_data<-fread('HousePrices.csv')

str(raw_data)

summary(MASS::stepAIC(lm(SalePrice~., data=raw_data)))

x_data<-raw_data[, !'SalePrice']

pc<-prcomp(x_data,scale.  = T)
pc$x

plot(pc)

biplot(pc, c(1,2))
pc$rotation[, c(1:5)]


newdata<-data.table(SalePrice=raw_data$SalePrice, pc$x[, c(1:4)])

summary(MASS::stepAIC(lm(SalePrice~., data=newdata)))


# more examples ...
data("swiss")

str(swiss)

pc<-princomp(swiss, cor=T)
plot(pc)
pc$sdev^2/sum(pc$sdev^2)

biplot(pc,c(1,2))
biplot(pc,c(1,3))
biplot(pc,c(2,3))

pc$loadings
pc$scores

library(rattle.data)

str(wine)

df<-wine[,-1]

pc<-prcomp(df, center = T, scale. = T)
pc

plot(pc)
pc$rotation
pc$sdev^2/sum(pc$sdev^2)
biplot(pc, c(1,2))
pc$rotation[, c(1,2)]

summary(wine$Type)


# loan dataset from kaggle

df<-fread("/Users/ssobrinou/Downloads/loan.csv")
str(df)
num_data<-df[, sapply(df, is.numeric), with=F]

# Amelia::missmap(num_data, col=c('black','white'))

num_missings<-sapply(num_data, function(x) sum(is.na(x))/length(x))



num_data<-num_data[, names(which(num_missings<0.25)), with=F]
num_data<-num_data[complete.cases(num_data), !c('id','member_id'), with=F]

str(num_data)

num_zero_var<-sapply(num_data, function(x) var(x)==0)
num_data<-num_data[, !names(which(num_zero_var)), with=F]

pc<-prcomp(num_data, center = T, scale. = T)
summary(pc)

plot(pc)

# biplot(pc, c(1,2))
