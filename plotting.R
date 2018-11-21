# R BASE CHARTS AND GGPLOT INTRODUCTION

# bar charts
# histogram & boxplots
# scatterplots
# line charts & time series


# http://www.r-graph-gallery.com for nice examples



# R BASE CHARTS ####

# managing the plotting space layout

plot(rnorm(10))  # this is a single plot

# lets create a multiplot

# par(...) # controls all of the base graphical features
par(mar=c(5,5,5,1)) # this controls the margins of the plotting space
par(mfrow=c(1,2)) # here we specify a matrix layout, with two rows and one column
plot(1:10)
plot(10:1)

par(mfrow=c(2,2)) 
plot(runif(10),col='black',main='chart 1')
plot(runif(10),col='red',main='chart 2')
plot(runif(10),col='green',main='chart 3')
plot(runif(10),col='blue',main='chart 4')

dev.off() # this clears all graphics and sets back the default layout 



data(state)
state_data<-data.frame(state.x77)


# barplots
help(barplot)
barplot(state_data$Life.Exp)

# you can customize it as much as you want
par(mar=c(7.25,4,4,1))
barplot(state_data[order(-state_data$Life.Exp),]$Life.Exp, ylim=c(65,75),xpd=FALSE, 
        names.arg=rownames(state_data[order(-state_data$Life.Exp),]),
        las=2, col='cornflowerblue',
        main='Life Expectancy (1977)'); grid()
legend(45,75,fill='cornflowerblue',legend='L.E')
abline(h=mean(state_data$Life.Exp),col='red')
text(50,mean(state_data$Life.Exp)+0.25,'Mean',col='red')


# histograms & boxplots

hist(state_data$Life)
hist(state_data$Life,probability=T,xlim=c(65,75),
     xlab='years',col='palegreen3',border = 'palegreen3') 


boxplot(state_data$Life)

set.seed(567)
x<-c(4.5, rnorm(100))
boxplot(x)


# lets make a boxplot for Life Expectancy segmented by a categorical variable

# we first bring on the Region
data(state)
state_data$Region<-state.region

boxplot(state_data$Life)
boxplot(state_data$Life~state_data$Region,
        col=c('red','orange','forestgreen','royalblue2'), 
        main='Life Expectancy by Region'); grid()

summary(state_data[state_data$Region=='South',]$Life.Exp)


# example from http://www.r-graph-gallery.com/
# Add data points
mylevels<-levels(state_data$Region)
levelProportions<-summary(state_data$Region)/nrow(state_data)

for(i in 1:length(mylevels)){
  
  thislevel<-mylevels[i]
  thisvalues<-state_data[state_data$Region==thislevel, "Life.Exp"]
  
  points(rep(i, length(thisvalues)), thisvalues, pch=20, col=rgb(0,0,0,.5)) 
  
}


# scatterplots

plot(state_data$HS.Grad,state_data$Income)

plot(state_data$HS.Grad,state_data$Income, col=state_data$Region, pch=19 , cex=1.3,
     main='HS.Grad Vs Income',xlab='Income',ylab='HS.Grad'); grid()
abline(mC <- lm(Income ~ HS.Grad, data = state_data))
legend(40, 6000, pch=19, col=1:4, levels(state_data$Region),
       bty='o',  box.col='gray', cex=.8)


# line plots and time series

plot(state_data$Income,type='o')

par(mar=c(5,5,5,5))
plot(state_data$Income,type='o',col='black', main='Income (1977)',
     xlab='',ylab='Income',xaxt='n'); grid()
axis(1,at=1:nrow(state_data),labels = rownames(state_data),las=2)
par(new = T)
plot(state_data$Area/1000,type='o',col='blue',
     axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(state_data$Area/1000)))
axis(1,at=1:nrow(state_data),labels = rownames(state_data),las=2)
mtext(side = 4, line=3, 'Area',col='blue')
legend('topright',
       legend=c('Income','Area'),
       lwd=1, col=c("black", "blue"))


# a usualy simpler option for visualizing multiple series in 
# one chart is to do it in just one axis, and probably you first
# need to scale the series (to equal mean and variance)
dev.off()
plot(state_data$Income,type='o',col='black', main='Income (1977)',
     xlab='',ylab='Income',xaxt='n'); grid()
axis(1,at=1:nrow(state_data),labels = rownames(state_data),las=2)

lines(state_data$Area,col='blue')  # lines() adds a new line to an existing plot

# now scaling the variables
plot(scale(state_data$Income),type='o',col='black', main='Income (1977)',
     xlab='',ylab='',xaxt='n'); grid()
axis(1,at=1:nrow(state_data),labels = rownames(state_data),las=2)

lines(scale(state_data$Area),col='blue')  



# time series charting

data("airmiles")

class(airmiles)

# a ts variable is a numeric/integer variable with a defined frecuency, a 
# start date and an end date

plot(airmiles,type='o'); grid()

# if a time series is not previously defined as a ts, we need to add the 
# dates correctly to the x axis


# GGPLOT INTRODUCTION

library(ggplot2)

barplot(state_data$Life.Exp)

theme_set(theme_minimal(base_size = 18))  # sets a theme for all ggplot charts

# check the ggthemes library to chose among lots of cool themes


ggplot(data=state_data,aes(x=rownames(state_data),y=Life.Exp))+
  geom_bar(stat='Identity')+theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(state_data$Income,type='p')

ggplot(data=state_data,aes(x=rownames(state_data),y=Income))+
  geom_point(size=1)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data=state_data,aes("Income",Income))+
  geom_boxplot()

state_data$Region<-state.region

ggplot(data=state_data,aes(Region,Income,colour=Region))+
  geom_boxplot()

ggplot(data=state_data,aes(x=Income))+
  geom_histogram()

data_airmiles<-data.frame(year=1937:1960,airmiles=as.integer(airmiles))
p<-ggplot(data_airmiles,aes(x=year,y=airmiles))+geom_line(lwd=1)
p+geom_point(size=3)

ggplot(data=state_data,aes(x=HS.Grad,y=Income))+geom_point()+geom_smooth()

p<-ggplot(data=state_data,aes(x=HS.Grad,y=Income))
print(p)
p2<-p+geom_point(size=5, alpha=0.5)
p2
p3<-p2+geom_smooth()
p3

ggplot(data=state_data,aes(x=HS.Grad,y=Income))+geom_point(size=5, alpha=0.5)+geom_smooth(method = lm)
ggplot(data=state_data,aes(x=HS.Grad,y=Income,colour=Region))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Region)




# ggplot = data + mapping + geometry

# point charts (scatterplots)

p<-ggplot(data=mtcars, mapping=aes(x=hp, y=mpg))
plot(p+geom_point())
plot(p+geom_line())
plot(p+geom_point()+geom_line())
plot(p+geom_point()+geom_smooth())

str(mtcars)
library(data.table)
mtcars<-data.table(model=rownames(mtcars), mtcars)

mtcars[, carb:=as.factor(carb)]
mtcars[, am:=as.factor(am)]

unique(levels(mtcars$carb))
p<-ggplot(mtcars, aes(x=hp, y=mpg, colour=carb))+geom_point()
plot(p)

p<-ggplot(mtcars, aes(x=hp, y=mpg, colour=am))+geom_point()
plot(p)
p<-ggplot(mtcars, aes(x=hp, y=mpg, colour=am))+geom_point()+facet_grid(facets = ~carb)
plot(p)



# barcharts

data(state)
state_data<-data.table(name=rownames(state.x77),state.x77)
state_data

state_data[, Region:=state.region]
state_data[, Division:=state.division]
str(state_data)


# stat='identity' when you want directly to represent the values of x in bars
p<-ggplot(state_data, aes(x=name, y=Income))+geom_bar(stat = 'identity')
p+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# reordering a factor: 
state_data[, name:=factor(name, levels=state_data[order(-Income)]$name)]
str(state_data)

p<-ggplot(state_data, aes(x=name, y=Income))+geom_bar(stat = 'identity')
p+theme(axis.text.x = element_text(angle = 90, hjust = 1))

p<-ggplot(mtcars, aes(x=model, y=mpg))+geom_bar(stat = 'identity')
p+theme(axis.text.x = element_text(angle = 90, hjust = 1))

p<-ggplot(mtcars, aes(x=model, y=hp, fill=am))+geom_bar(stat = 'identity')
p+theme(axis.text.x = element_text(angle = 90, hjust = 1))

p<-ggplot(mtcars, aes(x=model, y=hp, fill=carb))+geom_bar(stat = 'identity')
p+theme(axis.text.x = element_text(angle = 90, hjust = 1))


# stat='count' when you want to barplot a counting of factor levels across x cathegories
p<-ggplot(state_data, aes(x=Division, fill=Region))+geom_bar(stat = 'count')
p+theme(axis.text.x = element_text(angle = 90, hjust = 1))


p<-ggplot(mtcars, aes(x=am, fill=carb))+geom_bar(stat = 'count')
p

p<-ggplot(mtcars, aes(x=carb, fill=as.factor(cyl)))+geom_bar(stat = 'count')
p

p<-ggplot(mtcars, aes(x=carb, fill=am))+geom_bar(stat = 'count')
p

p<-ggplot(mtcars, aes(x=carb, fill=am))+geom_bar(stat = 'count', position = 'stack')
p
p<-ggplot(mtcars, aes(x=carb, fill=am))+geom_bar(stat = 'count', position = 'fill')
p
p<-ggplot(mtcars, aes(x=carb, fill=am))+geom_bar(stat = 'count', position = 'dodge')
p



# linecharts

time_index<-seq(from=as.Date('2001-07-05'), to=as.Date('2018-09-14'), by='1 day')
time_index[1:10]

df<-data.table(date=time_index, time_series=rnorm(length(time_index), mean = 100, sd=25))

p<-ggplot(df, aes(x=date, y=time_series))+geom_line(col='darkgray')
print(p)
print(p+geom_abline(slope = 0, intercept = mean(df$time_series), col='blue'))
print(p+geom_abline(slope = 0, intercept = c(mean(df$time_series),
                                             mean(df$time_series)-2*sd(df$time_series),
                                             mean(df$time_series)+2*sd(df$time_series)), col=c('blue','red','red')))


# melt and dcast for plotting  [ https://cran.r-project.org/web/packages/data.table/vignettes/datatable-reshape.html ]
library(data.table)
df<-fread('../melt_example.csv')
df
df[, dob_child1:=as.Date(dob_child1, format='%d/%m/%y')]
df[, dob_child2:=as.Date(dob_child2, format='%d/%m/%y')]
df[, dob_child3:=as.Date(dob_child3, format='%d/%m/%y')]
df
# long to wide format

df_long<-melt(df, id.vars = c("family_id", "age_mother"),
              measure.vars = c("dob_child1", "dob_child2", "dob_child3"))
df_long

df_wide<-dcast(df_long, family_id + age_mother ~ variable, value.var = 'value')
df_wide

# this is widely used to maked grouped plots with ggplot

ggplot(df,aes(x=family_id,y=dob_child1))+geom_point(size=2)+
  geom_point(aes(y=dob_child2),size=2,col='red')+
  geom_point(aes(y=dob_child3),size=2,col='green')

ggplot(df_long,aes(x=family_id,y=value, colour=variable))+geom_point(size=4)

library(data.table)
data(mtcars)
mtcars<-data.table(model=rownames(mtcars), scale(data.table(mtcars)[, .(hp, mpg, qsec)]))

mtcars_long<-melt(mtcars, id.vars = 'model')

mtcars_long

ggplot(mtcars_long, aes(x=model, y=value, fill=variable))+
  geom_bar(stat='identity')+coord_flip()

# instead of: 

ggplot(mtcars, aes(x=model))+
  geom_bar(aes(x=model, y=hp),stat='identity', fill='red')+
  geom_bar(aes(x=model, y=mpg),stat='identity', fill='green')+
  geom_bar(aes(x=model, y=qsec),stat='identity', fill='blue')+
  coord_flip()


# layout with multiple plots in ggplot

# install.packages('gridExtra')
library(gridExtra)
df <- data.frame(var1 = rnorm(200),
                 var2 = rnorm(200),
                 var3 = rnorm(200),
                 var4 = rnorm(200))

p1 <- ggplot(df) + geom_density(aes(x=var1))
p2 <- ggplot(df) + geom_density(aes(x=var2))
p3 <- ggplot(df) + geom_density(aes(x=var3))
p4 <- ggplot(df) + geom_density(aes(x=var4))

p3

grid.arrange(p1, p2, ncol=1)
grid.arrange(p1, p2, ncol=2)
grid.arrange(p1, p2, p3, ncol=3)
grid.arrange(p1, p2, p3, nrow=3)
grid.arrange(p1, p2, p3, p4, ncol=2)
grid.arrange(p1, arrangeGrob(p2,p3,p4, ncol=3), ncol=1)

# in long format:

df_long<-melt(df)

ggplot(df_long, aes(x=value, colour=variable))+geom_density(lwd=1.5)
ggplot(df_long, aes(x=value,colour=variable))+geom_density(lwd=1.5)+facet_grid(~variable)






