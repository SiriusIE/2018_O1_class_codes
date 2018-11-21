# FUNCTIONS ####

# functions are all over in R. In fact, c(), as.matrix(), rbind(), seq(), rep()...
# are all (pre-defined) functions. 
# A functions is an object that takes one or more inputs and returns an output
# inputs and output can take any form, even other functions

# we can create our own functions in R, in order to re-use and minimise our code


print_hello<-function(){}  # empty funcion, no aurgumets (input), no output

print_hello()   # here we call the function, returning NULL

print_hello<-function(){
  
  return('Hello')
  
}  # this function constantly returns 'Hello'

print_hello() 


sqr_number<-function(number){
  
  return(print(paste0('The square root of ',number, ' is ',number^0.5)))
}

sqr_number(6748439851385693)


# you can create as many steps as you may need inside a function

f<-function(x,y){
  
  x1<-x+56
  y1<-y-54
  
  solution<-x1+0.5*abs(y1-x1)
  
  solution
}


f(x=34,y=200)



# if statements are very often used inside functions

even_number<-function(number){
  
  if(!is.numeric(number)) { stop('Not a number') }  # stop for raising an error
  
  if(number%%2==0 & number!=0){
    print(paste0(number, ' is even'))
  } else if (number%%2!=0){
    print(paste0(number, ' is odd'))
  } else { 
    print('cero is not even nor odd')
    
  }
  
}


even_number(number=876231846129849)

even_number()

even_number(0)

even_number(number=seq(1:10))  # if statemets only apply to vectors of length 1


# char_number<-'876231846129849'
# tryCatch({even_number(number=char_number)}, finally = {even_number(as.numeric(char_number))})



# we could use a loop to iterate the if condition over a vector, 
# but in this case we would directly use the vectorised ifelse function

x<-c(1:10)

ifelse(x%%2==0, yes=paste0(x,' is even'), no=paste0(x, ' is odd'))


# function to calculate the standar deviation of a sample

set.seed(567)
x<-rnorm(100, mean=1900,sd=45)

sd_function<-function(x){
  
  variance<-sum((x-mean(x))^2)/(length(x)-1)
  standard_deviation<-sqrt(variance)
  
  return(standard_deviation)
  
}

sd_function(x)
sd(x)

# anonymous functions: defined inside another function, without name

plot(sapply(10:1000, function(n){
  return(sd_function(x=rnorm(n, sd=10)))
}),
type='l'); grid()
lines(abline(h=10))

# lets define the euclidean distance function

distance<-function(a,b){
  
  d <- sqrt((b[1]-a[1])^2 + (b[2]-a[2])^2)
  
  return(d)
}


# create a function that takes a data.frame, and converts every character 
# variable to a factor one


df_states<-fread('../df_states.csv', stringsAsFactors = F)

f_char_to_factor<-function(df=df_states){
  
  for(i in 1:ncol(df)){
    if(class(df[[i]])=='character'){
      df[[i]]<-as.factor(df[[i]])
    }
  }
  
  
  return(df)
}


str(df_states)
df_with_factors<-f_char_to_factor(df_states)
str(df_with_factors)


# create a function to confirm weather an integer number (greater than 2) is prime or not

is.prime<-function(integer){
  
  N_seq<-seq(2,integer-1)
  
  print(sum(integer%%N_seq==0)==0)
  
}

is.prime(3)
is.prime(4)
is.prime(integer=10)
is.prime(integer=31)
is.prime(integer=4183911)


# creating a fibbonaci sequence calculator

fib<-function(integer){
  result<-c(0,1)
  k<-1
  while(length(result)<integer){
    result[k+2]=result[k+1]+result[k]
    k<-k+1
  }
  return(result)
}

fib(15)
plot(fib(15)); grid()




# apply functions inside lapply

x<-sample(34:675,10)
lapply(x,function(x){return(mean(x*3-1))})


result_prime<-sapply(x,is.prime)
str(result_prime)



str(result_fib<-sapply(x,fib))

x<-list(a=10,b=20,c=30,d=40)
str(result_fib<-sapply(x,fib))


