#####################################
# 1. create a function that takes a number between 0 and ten 
# and returns "pass" if it is greater or equal than 5 or fail if it's lower

pass_check<-function(grade,on_time,optional_part){
  if (grade<0|grade>10) {
    stop('non valid grade')}
  if (on_time==FALSE) {
    grade<-grade-1}
  if (optional_part==TRUE) {
    grade <- grade +1}
  if (grade >=5){
    return(print('pass'))
  } else {
    return(print('fail'))
  }
}

pass_check(grade=5,on_time = F, optional_part = F)
pass_check(grade=5,on_time = T, optional_part = T)
pass_check(grade=5,on_time = T, optional_part = T)


grades<-0:11
for (i in seq_along(grades)){
  pass_check(grade=grades[i],on_time = T, optional=F)
}

#####################################
# 2. Create a fizzbuzz function. It takes a single number as input.
# If the number is divisible by three, it returns “fizz”. If it’s divisible by five it returns “buzz”.
# If it’s divisible by three and five, it returns “fizzbuzz”.Otherwise, it returns the number.

fizzbuzz <- function(x) {
  
  stopifnot(is.numeric(x))
  
  if ((x %% 3==0) & (x %% 5==0)) {
    return("fizzbuzz")
  } else if (x %% 3==0) {
    return("fizz")
  } else if (x %% 5==0) {
    return("buzz")
  } else { 
    return(x) 
  }
}

fizzbuzz(6)
fizzbuzz(10)
fizzbuzz(11)
fizzbuzz(15)

sapply(1:100, fizzbuzz)


#####################################
# 3. create a function to convert all character variables to factor variables

df_states<-read.csv('../df_states.csv',sep=';',stringsAsFactors = F)

str(df_states)

char_to_fact<-function(df){
  for(variable in colnames(df)){
    if(class(df[,variable])=='character'){
      df[, variable]<-as.factor(df[, variable])
    }
  }
  
  return(df)
  
}

df_states<-char_to_fact(df_states)

str(df_states)


char_to_fact_2<-function(x){
  if(class(x)=='character'){
    return(as.factor(x))
  } else (return(x))
}



for(i in 1:ncol(df_states)){
  df_states[,i]<-char_to_fact_2(df_states[,i])
}

# or:
df_states[]<-lapply(df_states, char_to_fact_2)


str(df_states)


#####################################
# 4. Create a function that takes a vector and returns its standard deviation, 
# by computing the formula (not using var() nor sd()).

set.seed(567)
x<-rnorm(100000000, mean=1900,sd=45)


sd_function<-function(x){
  sum_squares<-0  # intialization of sum of square difference
  mu<-mean(x)     # mean of x
  for(i in seq_along(x)){
    sqr_diff<-(x[i]-mu)^2  # square difference for ith observation
    sum_squares<-sum_squares+sqr_diff  # incremental cummulative sum
  }
  variance<-sum_squares/(length(x)-1)  # just a division by sample size minus one
  standard_deviation<-sqrt(variance)   # sd = sqrt(var)
  
  return(standard_deviation)
  
}

library(lubridate)
ini<-now()

sd(x)
now()-ini
sd_function(x)
now()-ini




# or: 

sd_function_2<-function(x){
  mu<-mean(x)
  variance<-sum((x-mu)^2)/(length(x)-1)
  standard_deviation<-sqrt(variance) 
  return(standard_deviation)
}

sd_function_2(x)
sd_function(x)
sd(x)

ini<-now()
sd(x)
ini1<-now()-ini
print(ini1)
sd_function(x)
ini2<-now()-ini1
print(ini2)
sd_function_2(x)
now()-ini2


#####################################
# 5. Create a function that takes two vectors of the same length, 
# and returns the number positions that have an NA in both 
# vectors. (eg. f( x = c(1, 2, 3, NA, 9, NA, 8), y=c(5, 6, NA, NA, 7, NA, 2)) should return 2).

x<-c(1, 2, 3, NA, 9, NA, 8)
y<-c(5, 6, NA, NA, 7, NA, 2)


both_na<-function(x,y){
  result<-0
  for(i in seq_along(x)){
    if(is.na(x[i])&is.na(y[i])){    # sum of TRUEs at the same position
      result<-result+1
    }
  }
  return(result)
}


both_na(x,y)



# or: 

both_na_2<-function(x,y){
  return(sum(is.na(x)&is.na(y)))
}

both_na_2(x,y)



#####################################
# 6. Write a greeting function that says “good morning”, “good afternoon”, “good evening”, or “good night” depending 
# on the time of day. The function takes an argument of the type “2017-01-08 21:05:00”. 
## Hint: use the function hour() from the lubridate package to get the hour out of the time stamp argument.
## It’s at you choice to set the boundaries for morning, afternoon, evening and night.


greet <- function(time = lubridate::now()) {
  
  library(lubridate)
  
  hr <- hour(time)
  
  if (hr >= 5 & hr < 12) {
    print("good morning")
  } else if (hr >= 12 & hr <17) {
    print("good afternoon")
  } else if (hr >=17 & hr < 21) {
    print("good evening")
  } else {"good night"}
} 


greet(time="2018-11-08  21:05:00")

greet(Sys.time())
greet(now())


#####################################
# 7. Create a vector ”total_NAs” containing the number of missing positions for each variable in df_states

total_NAs<-numeric()

for(i in seq_along(df_states)){
  total_NAs[i]<-sum(is.na(df_states[, i]))
}
names(total_NAs)<-names(df_states)

total_NAs

# or:
total_NAs<-numeric()

for(i in names(df_states)){
  total_NAs[i]<-sum(is.na(df_states[, i]))
}
total_NAs

# or:
total_NAs<-sapply(df_states, function(var) { return(sum(is.na(var)))})
total_NAs



#####################################
# 8. Create a function that takes a data.frame, and saves a .png file to any folder in your local with an histogram 
# for each numeric variable in tha data.frame. The title of each histogram should be the variable’s name.

for(i in seq_along(df_states)){
  if(is.numeric(df_states[, i])){
    png(paste0('../plot_',colnames(df_states)[i],'.png'))
    hist(df_states[, i], main=colnames(df_states)[i])
    dev.off()
  }
}


  plot_and_save <- function(x) {
    png(filename = paste(paste('histogram',colnames(x), sep='_'), 'png', sep='.'))
    hist(x[,i],main=colnames(x))
    dev.off()
  }
  

lapply(df_states, FUN=plot_and_save)



#####################################
# 9. Install the package carData and load the library, and then load the dataset “Salaries”; 
# create a new factor variable (“level_serv”) in Salaries representing the years of service in four levels: 
# ↘ ‘<7’ if yrs.service is less than 7
# ↘ '[7,16)’ if yrs.service is greater or equal than 7 but less than 16 
# ↘'[16,27)’ if yrs.service is greater or equal than 16 but less than 27 
# ↘ ‘>=27’ if yrs.service is greater or equal to 27

if(!"carData" %in% installed.packages()) {install.packages("carData")}
library(carData)  

data(Salaries)

str(Salaries)


Salaries$level_serv<-'a'   # we initialize the variable as any string

for(i in 1:nrow(Salaries)){
  if(Salaries[i,'yrs.service']<7){
    Salaries[i,'level_serv']<-"<7"
  } else if(Salaries[i,'yrs.service']>=7&Salaries[i,'yrs.service']<16){
    Salaries[i,'level_serv']<-"[7,16)"
  } else if (Salaries[i,'yrs.service']>=16&Salaries[i,'yrs.service']<27){
    Salaries[i,'level_serv']<-"[16,27)"
  } else {
    Salaries[i,'level_serv']<-">=27"
  }
}

Salaries$level_serv<-as.factor(Salaries$level_serv)
str(Salaries)
plot(Salaries$level_serv)


# or: 
Salaries$level_serv<-NULL

Salaries$level_serv<-'a'
Salaries[Salaries$yrs.service<7, ]$level_serv<-"<7"
Salaries[Salaries$yrs.service>=7&Salaries$yrs.service<16,'level_serv']<-"[7,16)"
Salaries[Salaries$yrs.service>=16&Salaries$yrs.service<27,'level_serv']<-"[16,27)"
Salaries[Salaries$yrs.service>=27,'level_serv']<-">=27"
Salaries$level_serv<-as.factor(Salaries$level_serv)

str(Salaries)
plot(Salaries$level_serv)

# or: (vectorised ifelse)

Salaries$level_serv<-NULL

Salaries$level_serv<-factor(ifelse(Salaries$yrs.service<7, "<7",
                                   ifelse(Salaries$yrs.service<16,"[7,16)",
                                          ifelse(Salaries$yrs.service<27,"[16,27)",">=27"))))

str(Salaries)
plot(Salaries$level_serv)


#####################################
# 10.Create a boxplot of salaries by levels of level_serv.

# we first reorder the factor levels:
Salaries$level_serv<-factor(Salaries$level_serv, levels = c("<7","[7,16)","[16,27)",">=27"))

# base r:
with(Salaries, boxplot(salary~level_serv,     # we encapsulate the plot function inside with() to attach variables and avoid $
                       main='Salary by level of experience', 
                       col=c('tomato','palegreen', 'cornflowerblue', 'violet'))) ; grid()  # colorsd and grid are just optionals

# ggplot: 
library(ggplot2)

ggplot(Salaries, aes(y=salary, colour=level_serv))+geom_boxplot()

#####################################
# 11.Create a scatterplot between Salaries and yrs.service colouring the points according to the levels of rank.

# base r:
with(Salaries, plot(yrs.service,salary,
                    main='Salary by years of experience, depending on Rank', 
                    col=rank)); grid()  # colorsd and grid are just optionals

# ggplot: 

ggplot(Salaries, aes(x=yrs.service, y=salary, colour=rank))+geom_point()

#####################################
# 12.For each variable in Salaries create an multiplot with a boxplot on the left and an histogram on 
# the right if the variable is numeric or a single barplot if its a factor variable.

# base r:
for (var in names(Salaries)){
  
  Sys.sleep(1)  # optional, makes a tima pause in the loop (1 sec)
  print(var)    # optional, prints the iteration name
  if(is.numeric(Salaries[,var])){
    par(mfrow=c(1,2))
    boxplot(Salaries[,var])
    hist(Salaries[,var])
  } else if (is.factor(Salaries[,var])){
    par(mfrow=c(1,1))
    plot(Salaries[,var])
  }
}

# ggplot: 

library(gridExtra)

for (var in names(Salaries)){
  
  Sys.sleep(1)  
  print(var)    
  if(is.numeric(Salaries[,var])){
    
    p1 <- ggplot(Salaries) + geom_boxplot(aes(y=get(var)))  # have to use get() before calling the variable
    p2 <- ggplot(Salaries) + geom_histogram(aes(x=get(var))) # get() evaluates an expression (in quotes), for ggplot variables can't be quoted
    
    plot(grid.arrange(p1, p2, ncol=2))
    
  } else if (is.factor(Salaries[,var])){
    p <- ggplot(Salaries) + geom_bar(aes(x=get(var)), stat='count')
    plot(p)
  }
}

#####################################
# 13. Create a function that takes any numeric (or integer) variable from Salaries and performs the next analysis:
# ↘ step 1: Provide the summary of the variable, printing before an announcement as: "Statistical Summary of Centrality Meassures:"
# ↘ step 2: Calculate the variable standard deviation and the coefficient of variation printing before an announcement as: "Standard Deviation and Coef. of Variation”
# ↘ step 3: Represent in a multiplot the histogram for the numeric variable on the left and a boxplot by levels of gender

f_analysis<-function(x){
  
  #1
  print("Statistical Summary of Centrality Meassures:")
  print(summary(x))
  #2
  print("Standard Deviation and Coef. of Variation")
  print(paste('SD =',round(sd(x),2)))                             # rounding to 2 decimals
  print(paste('Coef.variation =',round(100*sd(x)/mean(x)),'%'))   # CV = Mean/SD, expresed in %
  #3
  par(mfrow=c(1,2))
  hist(x, main='', xlab='')
  boxplot(x~Salaries$sex); grid()
}

f_analysis(Salaries$salary)

#####################################
# 14.Create a subset dataframe with only the numeric (or integer) variables from Salaries. 
# Then create a correlation matrix ”cor_matrix” containing the correlation for each par of numeric variables.
# ↘ Use a double for loop for this task.
# ↘ You can use the function cor() that gives the correlation among two variables. But do not apply cor() to the whole data.frame, which gives the
# solution directly.

vars_numeric<-Salaries[, sapply(Salaries, is.numeric)]
head(vars_numeric)

# we create the empty matrix for results
cor_matrix<-matrix(0,nrow=ncol(vars_numeric), ncol=ncol(vars_numeric))
colnames(cor_matrix)<-colnames(vars_numeric)
rownames(cor_matrix)<-colnames(vars_numeric)

cor_matrix

for(i in 1:ncol(vars_numeric)){
  for(j in 1:ncol(vars_numeric)){
    cor_matrix[i,j]<-cor(vars_numeric[,i], vars_numeric[,j])
  }
}

cor_matrix

# check: 
cor(vars_numeric)  # :=)

