#k-Means algoritham using the package in R
require("datasets")
data(iris)
str(iris)
summary(iris)
head(iris)
iris_new<-iris[]
iris_class<-iris_new[,5]
iris_new <- iris_new[,-5]
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
iris_new$Sepal.Length <- normalize(iris_new$Sepal.Length)
iris_new$Sepal.Width <- normalize(iris_new$Sepal.Width)
iris_new$Petal.Length <- normalize(iris_new$Petal.Length)
iris_new$Petal.Width <- normalize(iris_new$Petal.Width)

head(iris_new)

result <- kmeans(iris_new,3)

par(mfrow = c(2,2),mar=c(5,4,2,2))
plot(iris_new[c(1,2)],col=result$cluster)
plot(iris_new[c(1,2)],col=iris_class)
plot(iris_new[c(3,4)],col=result$cluster)
plot(iris_new[c(3,4)],col=iris_class)
