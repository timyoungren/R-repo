# Hitters - creating random variables then regsubsetting/lasso-ing

library(ISLR)
library(leaps)
set.seed(10) # for reproducibility always set the seed !
ls('package:ISLR')
fix(Hitters) # this allows one to see the data
names(Hitters)
summary(Hitters)

hitters = na.omit(Hitters) # notice the change in notation- sometimes one wants to go back

#hitters$AHits=hitters$CHits/hitters$Years # TY
#hitters$ARBI=hitters$CRBI/hitters$Years # TY

summary(hitters)
#head(hitters[,1:5])
hitters_x <- hitters
hitters_rowcount <- nrow(hitters_x)
for(x in 1:5){
  xmean <- mean(hitters[,x])
  xsd <- sd(hitters[,x])
  hitters_x <- cbind(hitters_x,rnorm(hitters_rowcount,mean=xmean,sd=xsd))
}

colnames(hitters_x)[21] <- "rAtBat"
colnames(hitters_x)[22] <- "rHits"
colnames(hitters_x)[23] <- "rHmRun"
colnames(hitters_x)[24] <- "rRuns"
colnames(hitters_x)[25] <- "rRBI"

ind73 <- sample(2, nrow(hitters_x), replace=TRUE, prob=c(0.7, 0.3))
trainData  <-  hitters_x[ind73==1,]
testData   <-  hitters_x[ind73==2,]

bbfit.full=regsubsets(Salary~.,trainData,nvmax=24,method="exhaustive") #
summary(bbfit.full)

# HITTERS LASSO

library(glmnet)

x <- model.matrix(Salary~.,trainData)[,-1]
y <- trainData$Salary
gr <- seq(10,-2,length=100)
grid <- 10^gr
lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)
#dim(coef(lasso.mod))
coef(lasso.mod)[,66]
cv.lasso <- cv.glmnet(x,y,alpha=1)
plot(cv.lasso)

bestlam <- cv.lasso$lambda.min
bestlam
lasso.best <- glmnet(x,y,alpha=1,lambda=bestlam)
coef(lasso.best)
