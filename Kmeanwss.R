#Within sum of square method
#Getting my data set
#tacking out the last column
myiris=iris[,]
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
myiris_new = myiris[,]
myiris_new=as.data.frame(lapply(myiris_new[,-5],normalize))
#myiris_new$Sepal.Width=normalize(myiris_new$Sepal.Width)
#myiris_new$Petal.Length=normalize(myiris_new$Petal.Length)
#myiris_new$Petal.Width=normalize(myiris_new$Petal.Width)
set.seed(120)
wss = c()
nc=10
for(i in 2:nc){
    wss <-c(wss,kmeans(myiris,centers = i)$tot.withinss)
}
plot(2:10,wss,xlab = "Number of cluster",ylab = "WSS")

