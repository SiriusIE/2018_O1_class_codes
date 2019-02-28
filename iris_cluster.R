data(iris)


cl<-kmeans(scale(iris[, names(iris)!='Species']), centers = 3)
cl

table(iris$Species,cl$cluster)

iris$cluster<-factor(cl$cluster)


library(ggplot2)
library(gridExtra)
p1<-ggplot(iris, aes(Petal.Length,Petal.Width, colour=cluster))+geom_point()
p2<-ggplot(iris, aes(Petal.Length,Petal.Width, colour=Species))+geom_point()

gridExtra::grid.arrange(p1, p2, ncol=2)


summary(lm(Sepal.Width~. , data=iris[, sapply(iris, is.numeric)]))




