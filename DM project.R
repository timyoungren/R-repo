data.dir<-'C:/Users/tim.youngren/Documents/Analytics/Kaggle/Facial Keypoints/'
training.file<-paste0(data.dir, 'training.csv')
test.file<-paste0(data.dir, 'test.csv')
im.train<-read.csv(training.file,stringsAsFactors=F)
im.train<-im.train$Image
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

im.pca <- prcomp(im.train, retx=TRUE, center=TRUE, scale=TRUE)
#
im.pca$x[1:4,1:10]
im.pca$rotation[1:4,1:10]
str(im.pca$sdev)
par(mfrow=c(1,1))
barplot(im.pca$sdev[1:50]^2,xlab="Eigen Values",ylab="Relative Magnitude",
        names.arg=1:50,axes=F)
#
im.pred<-predict(im.pca,im.train)

im.test  <- read.csv(test.file, stringsAsFactors=F)
str(im.test)
im.test <- foreach(im = im.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
test.pred<-predict(im.pca,im.test)

cosineDist <- function(x,y){
  (x%*%y)/(sqrt(rowSums(x^2)*(sum(y^2))))
}


a<-25   ## N. That is the number of PCA dimensions to use in the prediction.
b<-24    ## Which picture in the Test dataset to find matches for.

faceMatch<- function(a,b){
  x<-im.pred[,1:a]
  y<-test.pred[b,1:a]
  z<-18
  
  knn<-cosineDist(x,y)
  knn<-cbind(1:7049,knn)
  knn.winners<-head(order(-knn[,2]),z)
  
  par(mfrow=c(4,5))
  im <- matrix(data=rev(im.test[b,]), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255),xlab=b,ylab="")
  
  for (i in 1:z ) {
    im <- matrix(data=rev(im.train[knn.winners[i],]), nrow=96, ncol=96)
    image(1:96, 1:96, im, col=gray((0:255)/255),xlab=knn[knn.winners[i],2],ylab="")
  }
  plot(knn[knn.winners[1:z],2],ylab="Cosine Similarity")
  knn[knn.winners,]
  
  return(knn[knn.winners[1:z],])
}
faceMatch(10,x)
faceMatch(25,x)
faceMatch(50,x)
faceMatch(100,x)
x<-x+7
x<-94
faceMatch(10,x)
x<-x+1
faceMatch_Euclid<- function(a,b){
  x<-im.pred[1:7049,1:a]
  y<-test.pred[b,1:a]
  
  x<-rbind(y,x)
  knn<-rdist(x) 
  knn.winners<-head(order(knn[1,2:7050]),18)
  
  knn[1,]
  
  par(mfrow=c(4,5))
  im <- matrix(data=rev(im.test[b,]), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255),xlab=b,ylab="")
  for (i in 1:18 ) {
    im <- matrix(data=rev(im.train[knn.winners[i],]), nrow=96, ncol=96)
    image(1:96, 1:96, im, col=gray((0:255)/255),xlab=knn[1,knn.winners[i]+1],ylab="")
  }
  plot(knn[1,knn.winners[1:18]+1],ylab="Euclidian Distance")
  #knn[knn.winners]
  
  return(knn[1,knn.winners[1:18]+1])
}


x<-1
x<-x+1
graphData<-faceMatch_Euclid(100,x)
graphData<-cbind(graphData,faceMatch(25,x))
graphData<-cbind(graphData,faceMatch(50,x))
graphData<-cbind(graphData,faceMatch(100,x))

faceMatch_noPCA<- function(a,b){
  
  knn<-cosineDist(im.train[,1:a],im.test[b,1:a])
  knn<-cbind(1:7049,knn)
  knn.winners<-head(knn[order(-knn[,2])],18)
  
  par(mfrow=c(4,5))
  im <- matrix(data=rev(im.test[b,]), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255),xlab=b)
  for (i in 1:18 ) {
    im <- matrix(data=rev(im.train[knn.winners[i],]), nrow=96, ncol=96)
    image(1:96, 1:96, im, col=gray((0:255)/255),xlab=knn[knn.winners[i],2],ylab="")
  }
  plot(knn[knn.winners[1:18],2],ylab="Cosine Similarity")
  knn[knn.winners,]
  return(knn[knn.winners,])
}

x<-1

faceMatch_noPCA(9216,x)
faceMatch(2,x)
faceMatch(3,x)
faceMatch(4,x)
faceMatch(5,x)
faceMatch(6,x)
faceMatch(7,x)
faceMatch(8,x)
faceMatch(9,x)
faceMatch(10,x)
faceMatch(25,x)
faceMatch(50,x)
faceMatch(100,x)
x<-x+1

matplot(1:5,graphData,ylab="Cosine Similarity",type="l")

x<-im.train[1:20,]
y<-im.test[13,]
knn<-cosineDist(im.train,y)
knn<-cbind(1:7049,knn)
knn.winners<-head(knn[order(-knn[,2])],6)
knn[knn.winners,]

data.dir<-'C:/Users/tim.youngren/Documents/admin type stuff/Analytics/U of C MScA/2014 03 Data Mining/Project/'
data.path<-paste0(data.dir, 'blah.csv')
fails<-read.csv(data.path,stringsAsFactors=F)
par(mfrow=c(1,1))
matplot(t(fails))
colnames(fails)<-c("testnum","NoPCA","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA25","PCA50","PCA100")
rownames(fails)<-c("t1","t2","t3","t4","t5","t6","t25","t29")
barplot(fails[1,-1],names.arg=rownames(fails))

par(mfrow=c(4,6))
for (i in x ) {
  im <- matrix(data=rev(im.train[i,]), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255),xlab=i,ylab="")
}
x<-x+24

par(mfrow=c(2,4))

x<-c(12,22,34,50,2176,2195,2675,3174)
for (i in 1:8 ) {
  im <- matrix(data=rev(im.train[x[i],]), nrow=96, ncol=96)
  image(1:96, 1:96, im, col=gray((0:255)/255),xlab="",ylab="")
}
