#### WORKING WITH data.tableS ####
# From data.table to data.table
# Filtering and Subseting
# Merging data sets
# Grouped Operations



# data.table package ####
if(!"data.table" %in% installed.packages()) {install.packages("data.table")}
library(data.table)  


# data table basics
# data.table is a wrapper for data.table, with enhanced features
# a data.table is always a data.table, with all its capabilities

data(mtcars)

data_mtcars<-data.table(mtcars)

data_mtcars  # better display, but lost rownames!

data_mtcars<-cbind(model=rownames(mtcars),data_mtcars) # still a data.table

# get a variable (a single vector out of a column)
car_model<-data_mtcars$model 
car_model<-data_mtcars[['model']]
class(car_model)

# a slice of the data.table with one or more variables (subsetting columns)
car_model_and_mpg<-data_mtcars[ , list(model, mpg)]
car_model_and_mpg<-data_mtcars[ , .(model, mpg)]
class(car_model_and_mpg)

# row filtering and subsetting
data_mtcars[mpg>=20] # no need to preceed with $ the variable names
data_mtcars[mpg>=20,list(model,mpg,hp)]   
data_mtcars[mpg>=24 | am==1,c('model','mpg','hp')]
data_mtcars[mpg>=20 & disp>150,c(1,2,3)]

# add a new column
data_mtcars[ ,new_variable:=seq(1,nrow(mtcars))]

# add a new column only in some specific rows
data_mtcars[-1,new_variable_2:=runif(nrow(data_mtcars)-1)]
data_mtcars[c(10:20),new_variable_3:=10:20]
data_mtcars[mpg>20,new_variable_4:='mpg >20 !!!']

# delete a column
data_mtcars[ ,new_variable:=NULL]
# or data_mtcars$new_variable<-NULL

# we can make groupped opperation on the fly
data_mtcars[,carb:=as.factor(carb)]
data_2<-data_mtcars[mpg>=20,list(n=.N,
                                 mean_mpg=mean(mpg),
                                 mean_hp=mean(hp)),
                    by='carb'][order(carb)]
data_2




# merges with data.table

data_3<-data.table(x=runif(6),model=data_mtcars$model[sample(nrow(data_mtcars),6)])

new_row<-data.table(x=0.98, model = 'Toyota Verso')
data_3 <- rbind(data_3, new_row)

# merging the normal way: 

merge(data_mtcars, data_3, by='model')  # inner join

merge(data_mtcars, data_3, by='model', all.x=T, all.y=T) # outer join

merge(data_mtcars, data_3, by='model', all.x=T) # left join

merge(data_mtcars, data_3, by='model', all.y=T) # right join


# the data.table way: 
setkey(data_mtcars,model)
setkey(data_3,model)

data_mtcars[data_3, nomatch=0] # inner join

# left join
data_mtcars[data_3]
# right join
data_3[data_mtcars]

# duplicated treatment
data_mtcars[!duplicated(carb)]
data_mtcars[duplicated(model)]



# lets work with the state data.sets

rm(list=ls())
data(state)
ls()

help(state)

class(state.x77)

# lets first create coerce all data sets into data.tables, and create a unique id variable from state.abb
# we will also shuffle rows in each one of them

state.abb
l<-length(state.abb)

state_name<-data.table(id=state.abb, name=state.name)
state_name<-state_name[sample(l)]
head(state_name)

state_center<-data.table(id=state.abb, center_x=state.center$x, center_y=state.center$y)[sample(l)]
head(state_center)

state_area<-data.table(id=state.abb, area=state.area)
head(state_area)


state_division<-data.table(id=state.abb, division=state.division)[sample(l),]
head(state_division)


state_region<-data.table(id=state.abb, region=state.region)[sample(l),]
head(state_region)

state_data<-data.table(id=state.abb,state.x77)[sample(l),]
head(state_data)

# now, we'll merge all datasets into a single data.table with all information
# since all data sets are randomized rowwise, we cant just concatenate columns with cbind()

whole_data<-merge(state_name, state_data, by='id')
whole_data<-merge(whole_data, state_division, by='id')
whole_data<-merge(whole_data, state_region, by='id')
whole_data<-merge(whole_data, state_center, by='id')
whole_data<-merge(whole_data, state_area, by='id') # area and Area are not identical so we'll keep'em both by now

head(whole_data)
tail(whole_data)

str(whole_data)

# Let's turn name into a factor variable
whole_data[, name:=as.factor(name)]  
# or whole_data$name<-as.factor(whole_data$name)  


str(whole_data)

# we can achive this in a singe step with Reduce()

whole_data2 <- Reduce(f=merge,
                      x = list(state_name,
                               state_data,
                               state_division,
                               state_region,
                               state_center,
                               state_area))

str(whole_data2)

rm(whole_data2)



# summary of each variable in whole data

summary(whole_data)


# working with state_data:

# 1. states with al least 10 million people

most_popul<-whole_data[Population>=10000]

# 2. states with an Income above 5000

richest<-whole_data[Income>5000]

# 3. states with more than 10000 people and Income higher than 5000, but keeping only the variables id, name, Population and Income

popul_and_rich<-whole_data[Population>=10000&Income>5000,c('id', 'name', 'Population', 'Income')]


# 4. analyze the diffrence of Area and area, and keep only the smallest one
whole_data$Area-whole_data$area

with(whole_data, summary(Area-area))


# 5. create a new variable from the ratio of Murder and HS.Grad, and call it ratio_murder_hsGrad

whole_data[, ratio_murder_hsGrad:=whole_data$Murder/whole_data$HS.Grad]

# 6. which states names start with letter A

whole_data[which(whole_data$`Life Exp`>70)]

str(whole_data)

# 7. Calculate the mean for population, income and Murder by division, and order the result by decreasing mean of income
whole_data[, .(mean_population=mean(Population),
               mean_income=mean(Income),
               mean_murder=mean(Murder)), by='division'][order(-mean_income)]

