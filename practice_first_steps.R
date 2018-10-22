
# 1. generate a sequence starting in 100, up to 1500, with a step of 50
x<-seq(100, 1500, 50)

# 2. print the first 10 rows of x
print(x[1:10])

# 3. which is the length of the previous sequence?
length(x)

# 4. generate a sequence from 0 to 1 of length 100
x<-seq(0,1,length.out = 100)

# 5. which is the step of the previous sequence?
x[2]-x[1]

# 6. use the function sample() (see ?sample) to generate a random sequence 
# out of number 1 to 100
set.seed(67846823)
x<-sample(1:100)
x

# 7. which is the minimum number? and the maximun? 
min(x)
max(x)

# 8. get a basic statistic summary of the sequence; calculate the variance and
# the standard deviation (DO NOT use sd)
summary(x)

var_x<-var(x)
print(var_x)
sd_x<-sqrt(var_x)

# 9. which position holds the maximun number?
which.max(x)
# which the minimun?
which.min(x)

# 10. which is the minimum number? and the maximun? ( DO NOT USE min() nor max() )
x[which.min(x)]
x[which.max(x)]

# 11. create a vector y with the elements from x that are bigger than 5
x>5
x[x>5]
y<-x[x>5]
print(y)

# 12. create a vector y with the elements from x that are bigger than 80 or smaller than 3
y<-x[x>80|x<3]

# 13. create a vector y with the elements from x that are bigger than 50 and multiple of 3
y<-x[x%%3==0&x>50]

# 14. create a vector y with the first 20 elements from x that are bigger than 5
y<-x[x>5]
y<-y[1:20]
# or:
y<-x[x>5][1:20]

# 15. which elements of x are even and bigger than 80 or multiple of 3 and smaller than 30?
x[(x%%2==0&x>80)|(x%%3==0&x<30)]

# 16. sort the vector x in decreasing order
sort(x, dec=T)

# 17. get the positions in increasing order
order(x)

# 18. get the positions in decreasing order
order(-x)

# 19. get the min of x without using min() nor which.min()
sort(x)[1]
# or:
x[order(x)][1]


# 20. get the max of x without using max() nor which.max() nor the number 100
sort(x)[length(x)]
# or:
x[order(x)][length(x)]


# 21. create a vector ("let") with letters a to i  (hint: check built in vector letters)
let<-letters[1:9]
let


# 22. create another vector ("num") with numbers 9 to 1
num<-c(9:1)
num

# 23. create a new vector ("let_num") with the character sequence: a-9, b-8 ... i-1
let_num<-paste(let,num,sep='-')
let_num

# 24. get elements 1st to 3rd and 5th to 9th from let_num
let_num[c(1:3,5:9)]

# 25. create a vector named j giving the next result: 
# “A”, “A”, “A”, “B”, “B”, “B”, “C”, “C”, “C” (using the rep function)

j<-c(rep('A',3), rep('B',3), rep('C',3))
# or: 
j<-c(rep(LETTERS[1:3], each=3))
print(j)

# 26. check if element 3rd of num is even
num[3]%%2==0

# 27. recover the numeric vector num (num2) from the new vector let_num
num2<-as.numeric(substr(let_num,3,3))

identical(num2,num)
class(num)
class(num2)  # not identical because num is an integer

plot(num,num2)  # numerically, they are the same =)

# 28. assign the value 27 to the first element of num (override)
num[1]<-27
num

# 29. check if any element of let_num is missing
sum(is.na(let_num))>0

# 30. make the last 2 elements of num missing
let_num[(length(let_num)-1):length(let_num)]<-NA
let_num

# 31. now get the total number of missing positions in num 
sum(is.na(let_num))

# 32. change the values 3rd and 6th of let to the letter b
let[c(3,6)]<-'b'

# 33. create a new vector f, coercing the variable j to be a factor
f<-as.factor(j)

# 34. get a counting of the levels of the factor f
summary(f)



