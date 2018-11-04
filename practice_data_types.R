
# 1. 
# 2. 
m0<-matrix(c(3,1,4,6,
             5,4,9,3,
             2,2,5,5,
             0,6,1,7,
             4,8,5,1,
             5,2,2,3,
             2,5,5,2),nrow=7,byrow=T) 
print(m0)

# 3. 
colnames(m0)<-paste('col',LETTERS[1:4],sep='_')
rownames(m0)<-paste('row',seq(1:7),sep='_')

print(m0)

# 4.
dim(m0)

# 5.
v0<-m0[4,3]
# 6.
v1<-m0[c(1,4,5),3]
# 7.
v2<-m0[2, ]

# 8. 
v3<-m0[nrow(m0),ncol(m0)]

# 9. 
m1<-m0*4
m1

# 10.
m2<-m0[c(1,4),1:3]%*%m1[c(1,2,4), ] # keeps rownames
m2

# 11.
df<-as.data.frame(m2)
df
str(df)


# 12. & 13.
df_0<-read.csv("/Users/ssobrinou/IE/df_states.csv", sep=';')  # if the file is not in your wd, write the whole path

# 14. 
str(df_0)

# 15.
summary(df_0)

# 16.
murder<-df_0$Murder
print(murder)

# 17. 
sum(is.na(murder))>0
sum(is.na(murder))

# 18.
murder<-murder[!is.na(murder)]

# 19.
murder2<-df_0[!is.na(df_0$Murder),'Murder']
print(murder2)

# 20.
murder<-df_0$Murder
mean(murder)  

mean(murder2) 

mean(murder,na.rm = T) 

# 21.
df_0$state_order<-seq(1:nrow(df_0))
head(df_0)
tail(df_0)

# 22.
df_0$state_order<-NULL

df_0<-as.data.frame(cbind(state_order=seq(1:nrow(df_0)),df_0))
head(df_0)

# 23.
df_1<-df_0[complete.cases(df_0), ]
df_1
dim(df_0); dim(df_1)
sum(is.na(df_0))
sum(is.na(df_1))

# 24.
df_2<-df_1[df_1$Income>5000|df_1$Murder<10, ]
dim(df_2)
dim(df_1)

# 25. 
df_0[order(df_0$HS.Grad), ]
help(order)
df_0[order(df_0$HS.Grad, na.last = F), ]


###### removing all elements in the global enviroment ######
rm(list=ls())
##### ----------------------------------------------- ######

# 26. 
install.packages('nutshell')

# 27.
library(nutshell)

# 28.
data(batting.2008)

head(batting.2008)

# 29.
str(batting.2008)



# 30.
sum(is.na(batting.2008)) 

# 31.
batting.2008[batting.2008$birthYear>1980, ]
# or:
subset(batting.2008,birthYear>1980)

# tip: to chek if two objects are exactly the same use the identical function:
identical(batting.2008[batting.2008$birthYear>1980, ],subset(batting.2008,birthYear>1980))


# 32. 
batting.2008[batting.2008$birthYear>1980&batting.2008$bats=='B', ]
# or
subset(batting.2008,birthYear>1980&bats=='B')



# 33.
batting.2008[batting.2008$birthYear>1980&batting.2008$bats=='B'&batting.2008$throws=='L', ]
# or
subset(batting.2008,birthYear>1980&bats=='B'&throws=='L')



# 34.
batting.2008[batting.2008$teamID=='OAK'|batting.2008$teamID=='DET', ]
# or
subset(batting.2008,teamID=='OAK'|teamID=='DET')

# in this case you could make use of the combine c() function and the %in% instruction (can't use == with a vector)
batting.2008[batting.2008$teamID %in% c('OAK','DET'), ]
# or
subset(batting.2008,teamID %in% c('OAK','DET'))

# 35.
batting.2008[batting.2008$birthYear>1985,c('nameLast','nameFirst','birthYear','teamID')]
# or
subset(batting.2008,birthYear>1985,select=c('nameLast','nameFirst','birthYear','teamID'))



# 36. 
batting.2008[batting.2008$birthYear>1985|batting.2008$height<=71,c('nameLast','nameFirst','birthYear','height','teamID')]
# or
subset(batting.2008,birthYear>1985|height<=71,select=c('nameLast','nameFirst','birthYear','height','teamID'))


# 37.
batting_2<-batting.2008[(batting.2008$birthYear>1985|batting.2008$birthYear<1965) &
                          (batting.2008$height<=71|batting.2008$height>=90),c('nameLast',"nameFirst", "birthYear","height")] 
# or
batting_2<-subset(batting.2008,(birthYear>1985|birthYear<1965) & (height<=71|height>=90),
                  select=c(nameLast,nameFirst,birthYear,height))  # you need to encapsulate the or conditions in parenthesis


# 38. 
batting.2008[batting.2008$birthYear>1985|batting.2008$height<=71,names(batting.2008)!='teamID']
# or
subset(batting.2008,birthYear>1985|height<=71,select=-teamID) 


# 39. 
batting.2008[batting.2008$birthYear>1985|batting.2008$height<=71,!names(batting.2008)%in%c('teamID','nameLast'))  
# or
subset(batting.2008,birthYear>1985|height<=71,select=!names(batting.2008)%in%c('teamID','nameLast'))

# 40. 
batting_2<-cbind(id_row=seq(1,nrow(batting_2)),batting_2)


library(nutshell) 

# 41.
data(medicare.payments.by.state)

# 42.
df_states<-read.csv('/Users/ssobrinou/IE/df_states.csv',sep=';')
str(df_states)

# 43.
df_medicare<-medicare.payments.by.state

# 44.
str(df_medicare)
str(df_states)


# 45.
df_medicare_unique<-df_medicare[!duplicated(df_medicare$State), ]
str(df_medicare_unique)

# 46. 
levels(df_medicare_unique$State)  # there are weird states like GU, MP, AS ...
levels(df_states$id)

# tip: setdiff(a,b) prints only the elements in a different from b
setdiff(levels(df_medicare_unique$State),levels(df_states$id)) # states in df_medicare_unique not present in df_states
setdiff(levels(df_states$id),levels(df_medicare_unique$State)) # states in df_medicare_unique not present in df_states

# 47. 
df_medicare_unique_truncated<-df_medicare_unique[!df_medicare_unique$State%in%c('NY','CA') , !names(df_medicare_unique)%in%c('Diagnosis.Related.Group','Footnote')]
str(df_medicare_unique_truncated)


# 48.
df_inner_join<-merge(df_medicare_unique_truncated,df_states,by.x='State',by.y='id')
head(df_inner_join)
str(df_inner_join)  # only 48 rows, the ones that match by state abbreviation


# 49.
df_left_join_1<-merge(df_medicare_unique_truncated,df_states,by.x='State',by.y='id',all.x=T)
head(df_left_join_1)
str(df_left_join_1)


# 50.
df_left_join_2<-merge(df_medicare_unique_truncated,df_states,by.x='State',by.y='id',all.y=T)
head(df_left_join_2)
str(df_left_join_2)



# 51.
df_outer_join<-merge(df_medicare_unique_truncated,df_states,by.x='State',by.y='id',all=T)
head(df_outer_join)
str(df_outer_join)


# 52.
set.seed(6541)
df_states$z<-sample(c('A','B','C','U','V','W'),nrow(df_states),replace=TRUE)
head(df_states)

set.seed(6541)
df_medicare_unique$z<-sample(LETTERS[1:6],nrow(df_medicare_unique),replace = TRUE)
head(df_medicare_unique)


# 53. 
df_join_ABC<-merge(df_states,df_medicare_unique,by.x=c('id','z'),by.y=c('State','z'))
print(df_join_ABC)

data(iris)

str(iris)



# 54. 
df_states_no_NAs<-df_states[complete.cases(df_states),]
aggregate(df_states_no_NAs[, c('Population','Income')], FUN = mean, by=list(division=df_states_no_NAs$division))
# or
df_states_no_NAs_2<-data.table(df_states_no_NAs)
df_states_no_NAs_2[, .(Population=mean(Population),Income=mean(Income)), by='division']

# 55. 
aggregate(df_states_no_NAs[, c('Murder','Life.Exp')], FUN = sd, by=list(region=df_states_no_NAs$region))
# or
df_states_no_NAs_2<-data.table(df_states_no_NAs)
df_states_no_NAs_2[, .(Murder=sd(Murder),Life.Exp=sd(Life.Exp)), by='region']

# 56. 
aggregate(df_states_no_NAs[, c('Population','Income')], FUN = mean, by=list(division=df_states_no_NAs$division, region=df_states_no_NAs$region))
df_states_no_NAs_2[, .(Population=mean(Population, na.rm = T),Income=mean(Income, na.rm = T)), by=c('division','region')]

# 57. 
aggregate(df_states[, c('Population','Income')], FUN = mean, by=list(division=df_states$division), na.rm=T)
# or
df_states_2<-data.table(df_states)
df_states_2[, .(Population=mean(Population, na.rm=T),Income=mean(Income, na.rm = T)), by='division']

# 58
df_states$state_initial<-substr(df_states$name,1,1)
aggregate(df_states[, c('Population','Income')], FUN = mean, by=list(state_initial=df_states$state_initial), na.rm=T)
# or
df_states_2[, state_initial:=substr(name,1,1)]
df_states_2[, .(Population=mean(Population, na.rm = T),Income=mean(Income, na.rm = T)), by='state_initial']

# 59. 
aggregate(df_states[, c('Population','Income')], FUN = max, by=list(region=df_states$region, state_initial=df_states$state_initial), na.rm=T)
data.table(df_states)[, .(Population=max(Population, na.rm = T),Income=max(Income, na.rm = T)), by=c('region','state_initial')]

# 60. 
data(iris)
aggregate(iris[, which(names(iris)!='Species')], FUN = median, by=list(Species=iris$Species))
data.table(iris)[, .(median_sepal_length=median(Sepal.Length),
                     median_sepal_width=median(Sepal.Width),
                     median_petal_length=median(Petal.Length),
                     median_petal_width=median(Petal.Width)), by='Species']

# 61.
library(nutshell)

data(batting.2008)
str(batting.2008)

df<-aggregate(batting.2008[, c('birthYear')], FUN=mean, by=list(teamID=batting.2008$teamID))
df[which.max(df$x),]
df[which.min(df$x),]
# or
df_2<-data.table(batting.2008)[, .(x=mean(birthYear)), by='teamID']
df_2[which.max(df_2$x),]
df_2[which.min(df_2$x),]
