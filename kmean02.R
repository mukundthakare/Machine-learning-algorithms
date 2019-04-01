#Getting my data set
myiris<-iris
#tacking out the last column
myiris=myiris[-length(myiris)]
#generating the 150 random numbers between 1 and 3
set.seed(18190)
data<-round(runif(150,1,3))
#assigning to the data
myiris$cluster<-data
myiris$pred_cluster<-data
#divinding dataset into three clusters 
vector1 = subset(myiris,subset= (cluster==1))
vector2 = subset(myiris,subset= (cluster==2))
vector3 = subset(myiris,subset= (cluster==3))
cg1=c(0,0,0,0);cg2=c(0,0,0,0);cg3=c(0,0,0,0)
d1=0;d2=0;d3=0;D=0
D=0
while(D <  6){
  
    #To find the centroid for each cluster 
    cg1 = rbind(c(mean(vector1[,1]),mean(vector1[,2]),mean(vector1[,3]),mean(vector1[,4])))
    cg2 = rbind(c(mean(vector2[,1]),mean(vector2[,2]),mean(vector2[,3]),mean(vector2[,4])))
    cg3 = rbind(c(mean(vector3[,1]),mean(vector3[,2]),mean(vector3[,3]),mean(vector3[,4])))
    #for each example in each cluster we calculate the distance from other two clusters
    #reassigning the cluster number for examples in first clusters 
    d1_mean = c()
    for(i in nrow(vector1)){
      ex = vector1[i,c(-5,-6)]
      d1 = sqrt(sum((ex-cg1)^2))
      d1_mean = c(d1_mean,d1)
      d2 = sqrt(sum((ex-cg2)^2))
      d3 = sqrt(sum((ex-cg3)^2))
      min_dist=min(d1,d2,d3)
      
      if(min_dist==d1){
        vector1[i,6]<- 1
      }else if(min_dist==d2){
        vector1[i,6]<- 2
      }else{
        vector1[i,6]<- 3
      }
    }
    d2_mean = c()
    #reassigning the cluster number for example in second cluster
    for(i in nrow(vector2)){
      ex = vector2[i,c(-5,-6)]
      d1 = sqrt(sum((ex-cg1)^2))
      d2 = sqrt(sum((ex-cg2)^2))
      d2_mean = c(d2_mean,d2)
      d3 = sqrt(sum((ex-cg3)^2))
      min_dist=min(d1,d2,d3)
      
      if(min_dist==d1){
        vector2[i,6]<- 1
      }else if(min_dist==d2){
        vector2[i,6]<- 2
      }else{
        vector2[i,6]<- 3
      }
    }
    d3_mean = c()
    
    #reassigning the cluster number for example in third cluster
    for(i in nrow(vector3)){
      ex = vector3[i,c(-5,-6)]
      d1 = sqrt(sum((ex-cg1)^2))
      d2 = sqrt(sum((ex-cg2)^2))
      d3 = sqrt(sum((ex-cg3)^2))
      d3_mean = c(d3_mean,d3)
      min_dist=min(d1,d2,d3)
      
      if(min_dist==d1){
        vector3[i,6]<- 1
      }else if(min_dist==d2){
        vector3[i,6]<- 2
      }else{
        vector3[i,6]<- 3
      }
    }
    D = mean(d1_mean)+mean(d2_mean)+mean(d3_mean)

    #divinding dataset into three clusters 
    View(vector1)
    View(vector2)
    View(vector3)
    vector1 = subset(myiris,subset= (pred_cluster==1))
    vector2 = subset(myiris,subset= (pred_cluster==2))
    vector3 = subset(myiris,subset= (pred_cluster==3))
    
}
