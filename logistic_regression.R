# Homework assignment to create a logistic regression
# I created a dataset to simulate a relief pitcher's pitch count when he gives up
# the first homerun of the outing. So the outings have varying lengths and some
# end in a homerun while some don't.

final<-NULL
x<-c(rexp(10,rate=1.2),rexp(10,rate=1.1),rexp(10,rate=1),rexp(40,rate=.95))>=4
length(x)<-min(round(rnorm(1,mean=30,sd=8),0),length(x))
fhrpc<-if (min(which(x))>500) {c(length(x),T)} else {c(min(which(x)),F)}
final<-rbind(final,fhrpc);nrow(final)

colnames(final)<-c("PitchCount","FirstHR")
rownames(final)<-c(1:70)
final<-as.data.frame(final)

relief_hr.model<-glm(FirstHR~PitchCount,data=final,family=binomial(link=logit))
summary(relief_hr.model)
predict(relief_hr.model,newdata=data.frame(PitchCount=15),type='response')
predict(relief_hr.model,newdata=data.frame(PitchCount=16),type='response')
predict(relief_hr.model,newdata=data.frame(PitchCount=17),type='response')
