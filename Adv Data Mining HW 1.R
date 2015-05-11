X <- rnorm(100,mean=100,sd=5)
epsilon <- rnorm(100,mean=0,sd=1)

par(mfrow=c(1,2))
plot(X)
plot(epsilon)

# #3, part 1 (Ch.6, Q.9, a & b for 50-50 and 70-30 test train split)

library(leaps)
library(ISLR)
#ls('package:ISLR')
set.seed(10)
fix(College)

##### 50-50 test train split
ind5050 <- sample(2, nrow(College), replace=TRUE, prob=c(0.5, 0.5))
#ind5050
trainData5050 <- College[ind5050==1,]
testData5050 <- College[ind5050==2,]
collegelm5050<-lm(Apps~.,trainData5050)
#summary(collegelm5050)
college5050pred <- predict(collegelm5050,testData5050)
c5050TestError <- mean((testData5050$Apps-college5050pred)^2); c5050TestError
##### 70-30 test train split
ind73 <- sample(2, nrow(College), replace=TRUE, prob=c(0.7, 0.3))
#ind73
trainData73 <- College[ind73==1,]
testData73 <- College[ind73==2,]
#head(trainData73)
collegelm73<-lm(Apps~.,trainData73)
#summary(collegelm73)
college73pred <- predict(collegelm73,testData73)
c73TestError <- mean((testData73$Apps-college73pred)^2); c73TestError

# #3, part 2 (code 20 different 70-30 test train splits and report summary stats)
c20MSE <- NULL
x <- 20              # Will work for any value <(777/20 -> 39), but any value will give a fresh mix with each increment.
for (i in 1:20 ) {
  ind_i        <-  c(ind73[(length(ind73)-x*i):length(ind73)],ind73[1:(length(ind73)-(x*i+1))])
  trainData_i  <-  College[ind_i==1,]
  testData_i   <-  College[ind_i==2,]
  collegelm_i  <-  lm(Apps~.,trainData_i)
  cPred_i      <-  predict(collegelm_i,testData_i)
  cTestError_i <-  mean((testData_i$Apps-cPred_i)^2)
  c20MSE       <-  c(c20MSE,cTestError_i)  
}
c20MSE
boxplot(c20MSE,ylab="MSE",main="MSE for 20 folds of 70-30 split\n of the College dataset")
hist(c20MSE,main="MSE for 20 folds of 70-30 split\n of the College dataset")
mean(c20MSE)
sd(c20MSE)
median(c20MSE)
min(c20MSE)
max(c20MSE)


# #3, part 3 (best model for best subset for all Cp, adjr2, BIC, CV)

college<-na.omit(College)
college.fit<-regsubsets(Apps~.,college,nvmax = 17)
summary(college.fit)
f <- summary(college.fit) # nice short name
fitscores <- cbind(1-f$rsq,1-f$adjr2,f$cp,f$bic) # now smallest is best for every criteria
colnames(fitscores) <- c('1-rsq','1-adjr2','cp','bic') # name the columns
bestModel <- apply(fitscores,2,which.min)
bestModel

# Of course some people like graphs-
#par(mfrow = c(2,2)) # this causes for plots per page in a 2x2
#plot(f$rsq ,xlab=" Number of Variables ",ylab=" R^2",type="l")
#plot(f$adjr2 ,xlab=" Number of Variables ",ylab="adj R^2", type="l")
#points (13, f$adjr2 [13], col ="red",cex =2, pch =20)
#plot(f$cp ,xlab=" Number of Variables ",ylab=" cp", type="l")
#points (12, f$cp [12], col ="red",cex =2, pch =20)
#plot(f$bic ,xlab=" Number of Variables ",ylab=" bic", type="l")
#points (10, f$bic[10], col ="red",cex =2, pch =20)

coef(college.fit,12)
coef(college.fit,13)
coef(college.fit,10)

k <- 12
folds <- sample(1:k,nrow(college),replace=TRUE)

predict.regsubsets <- function(object,newdata,id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

cv.errors <- matrix(NA,k,17,dimnames=list(NULL,paste(1:17)))
for(j in 1:k){
  best.fit <- regsubsets(Apps~.,data=college[folds!=j,],nvmax=17)
  for(i in 1:17){
    pred <- predict(best.fit,college[folds==j,],id=i)
    cv.errors[j,i] <- mean((college$Apps[folds==j]-pred)^2)
  }
}
cv.errors
which.min(apply(cv.errors,2,mean))
coef(best.fit,11)
clm <- lm(Apps~1+Private+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+Outstate+Room.Board+PhD+Expend+Grad.Rate,data=college)


# #3, part 4 (MSE, adjr2, AIC, BIC, Cp for 12 folds plus the average)

k <- 12
folds <- sample(1:k,nrow(college),replace=TRUE)
cv.errors <- matrix(NA,k,17,dimnames=list(NULL,paste(1:17)))
cv.Cp <- NULL
cv.AIC <- NULL
cv.BIC <- NULL
cv.adjr2 <- NULL
for(j in 1:k){
  best.fit <- regsubsets(Apps~.,data=college[folds!=j,],nvmax=17)
  bf <- summary(best.fit)
  cv.Cp <- rbind(cv.Cp,bf$cp)
  cv.AIC <- rbind(cv.AIC,bf$cp/((bf$cp*(nrow(college[folds!=j,])-1)-bf$rss)/(2*c(1:17))))
  cv.BIC <- rbind(cv.BIC,bf$bic)
  cv.adjr2 <- rbind(cv.adjr2,bf$adjr2)
  for(i in 1:17){
    pred <- predict(best.fit,college[folds==j,],id=i)
    cv.errors[j,i] <- mean((college$Apps[folds==j]-pred)^2)
  }
}

#   I'm not sure why my AIC calculation is so bad. I tried to isolate sigma hat squared in Cp 
#   then divide Cp by that to get AIC. That seems like it should work looking at the formulas 
#   for both in the book. I'm also not sure why BIC (not my calculation) is negative.
q4p4 <- cbind(apply(cv.errors,2,mean),apply(cv.adjr2,2,mean),apply(cv.AIC,2,mean),apply(cv.BIC,2,mean),apply(cv.Cp,2,mean))
colnames(q4p4) <- c("MSE","adjr2","AIC","BIC","Cp")
q4p4

# #4
data.dir<-'C:/Users/tim/Documents/U of C MScA/2015 02 ADM & Predictive Analytics/assignments/'
filename<-paste0(data.dir, 'finData.csv')
FinData<-read.csv(filename,stringsAsFactors=F)
head(FinData[,2:10])
finData <-FinData[,2:10]
head(finData)

k <- 10
d <- 8
folds <- sample(1:k,nrow(finData),replace=TRUE)

fin_cv.errors <- matrix(NA,k,d,dimnames=list(NULL,paste(1:d)))
for(j in 1:k){
  best.fitfin <- regsubsets(AMZN~.,data=finData[folds!=j,])
  for(i in 1:d){
    pred <- predict(best.fitfin,finData[folds==j,],id=i)
    fin_cv.errors[j,i] <- mean((finData$AMZN[folds==j]-pred)^2)
  }
}
fin_cv.errors
which.min(apply(fin_cv.errors,2,mean))
