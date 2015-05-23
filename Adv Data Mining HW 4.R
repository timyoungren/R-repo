# 3 ========== 3 ========== 3 ========== 3 ========== 3 ========== 3
par(mfrow = c(1,1))
prob3 <- seq(0,1,0.01)
classification_error <- 1-prob3
gini <- prob3*(1-prob3)
cross_entropy <- prob3*log(prob3)*-1
plot(prob3, classification_error, ylab='Value', xlab='Probability', type='l', col='blue')
lines(prob3, gini, type='l')
lines(prob3, cross_entropy, col='red')
legend("topright", legend=c("Classification Error","Cross-entropy","Gini"),
       col=c('blue','red','black'),pch=1)


# 5 ========== 5 ========== 5 ========== 5 ========== 5 ========== 5
data1 <- c(0.1,0.15,0.2,0.2,0.55,0.55,0.6,0.65,0.7,0.75)
majority_vote <-ifelse(length(which(data1>0.5))/length(data1)>0.5,T,F)
majority_vote
average_probability <-ifelse(mean(data1)>0.5,T,F)
average_probability


# 7 ========== 7 ========== 7 ========== 7 ========== 7 ========== 7
library(randomForest)
set.seed(10)
train <- sample (1:nrow(Boston), nrow(Boston)*0.7)
ntree7 <- c(seq(25,275,25),seq(300,1900,100),seq(2000,5000,250))
testError <- rep(0,length(ntree7))
testMAE <- rep(0,length(ntree7))
testSDE <- rep(0,length(ntree7))
for(i in 1:length(ntree7)){
  rf <- randomForest(medv∼., data=Boston, subset=train, mtry=6, ntree=ntree7[i])
  yhat <- predict(rf, newdata=Boston[-train,])
  testError[i] <- mean((yhat-Boston[-train,'medv'])^2)
  testMAE[i] <- mean(yhat-Boston[-train,'medv'])       # just curious what this would look like
  testSDE[i] <- sd(yhat-Boston[-train,'medv'])         # just curious again
}
plot(ntree7, testError, type='l', xlab='Number of Trees', ylab='Test MSE')
par(mfrow=c(1,2))
plot(ntree7, testMAE, col='red')
plot(ntree7, testSDE, col='blue')
mean(Boston$medv)
sd(Boston$medv)


# 12 ========= 12 ========= 12 ========= 12 ========= 12 ========= 12
data()
attach(OJ)
head(OJ)
names(OJ)
set.seed(10)
train <- sample (1:nrow(OJ), nrow(OJ)*0.7)
unique(OJ$Purchase)
OJ$purchase <- ifelse(OJ$Purchase=='CH',1,0)

# BOOST
boost.OJ <- gbm(purchase~.-Purchase, data=OJ[train,], distribution="bernoulli", n.trees=2000, interaction.depth=1)
yhat.boost <- predict(boost.OJ, newdata=OJ[-train,], n.trees=2000, type='response')
range(yhat.boost)
boost.MSE <- mean((yhat.boost-OJ$purchase[-train]))

# BAGGING
bag.OJ <- randomForest(purchase∼.-Purchase, data=OJ, subset=train, mtry=17, ntree =2000, cutoff=c(0.5,1-0.5))
yhat.bag <- predict(bag.OJ, newdata=OJ[-train,], type='response')
bag.MSE <- mean((yhat.bag-OJ$purchase[-train])^2)

# RANDOM FOREST
rf.OJ <- randomForest(purchase∼.-Purchase, data=OJ, subset=train, ntree =2000, cutoff=c(0.5,1-0.5))
yhat.rf <- predict(rf.OJ, newdata=OJ[-train,], type='response')
rf.MSE <- mean((yhat.rf-OJ$purchase[-train])^2)

# LINEAR REGRESSION
lm.OJ <- lm(purchase∼.-Purchase, data=OJ, subset=train, )
yhat.lm <- predict(lm.OJ, newdata=OJ[-train,])
lm.MSE <- mean((yhat.lm-OJ$purchase[-train])^2);lm.MSE

# LOGISTIC REGRESSION
lr.OJ <- glm(purchase∼.-Purchase, data=OJ, subset=train, family=binomial())
yhat.lr <- predict(lr.OJ, newdata=OJ[-train,], type='response')
lr.MSE <- mean((yhat.lr-OJ$purchase[-train])^2); lr.MSE

par(mfrow=c(1,1))
plot(c(bag.MSE,rf.MSE,lm.MSE,lr.MSE,boost.MSE), ylab='MSE', pch=19, col=c(1,3:5,2))
text(x=c(1.2,2:4,4.8),y=c(.134,.127,.124,.12,.043), labels=c('bagging','random\nforest','linear\nregression','logistic\nregression','boosting'))
boost.MSE/mean(c(bag.MSE,rf.MSE,lm.MSE,lr.MSE))

detach(OJ)
