


qnorm(0.95,lower.tail = T)


Auto<-read.csv('Auto.csv')





str(Auto)


X<-model.matrix(mpg~weight+year, data=Auto)
head(X,10)


y<-Auto$mpg
beta_hat<-solve(t(X)%*%X)%*%t(X)%*%y
beta_hat


y_hat<-X%*%beta_hat
head(y_hat,10)


(SSE<-t(y-y_hat)%*%(y-y_hat))
n<-length(y)
(RSE<-SSE/(n-3))



summary(lm(mpg~weight+year, data=Auto))

cons<-qnorm((1-0.99)/2, lower.tail = F)
modelq1<-(lm(mpg~weight+year, data=Auto))

std<-summary(modelq1)$coef[,2]
b_smm<-summary(modelq1)$coef[,1]
(cbind(b_smm-cons*std, b_smm+cons*std))
new_cars =data.frame(weight =c(3500, 5000), year =c(76, 81))

predict(modelq1, newdata=new_cars)
predict(modelq1, newdata=new_cars, interval="confidence", level=0.99)
predict(modelq1, newdata=new_cars, interval="prediction", level=0.99)



null<-(lm(mpg~1, data=Auto))
anova(null, modelq1)

str(Auto)
Auto$X

satm<-lm(mpg~., data=Auto)
satm2<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin+name, data=Auto)
anova(modelq1, satm2)






dflong <-read.csv("Qmixed.csv", header=TRUE)
dflong



str(dflong)

dflong$id<-factor(dflong$id)



library(nlme)
model2<-lme(score~treat+time+treat*time,random =~1|id, data=dflong)
summary(model2)


lmeFit <-lme(CD4~obstime, random =~obstime|patient, data = aids, method = "REML")

X<-model.matrix(model2, dflong)
X

Z<-model.matrix(score~treat+time+treat*time, data=dflong)

Z<-model.matrix(score~id-1,dflong)
Z=Z[,order(colnames(Z))]
Z

length(dflong$score)

sig<-14.14214^2*Z%*%t(Z) + 11.40175^2*diag(1, length(dflong$score))
sig
print(sig, digits=5)

model.matrix(score~treat+time+treat*time,random =~treat|id, data=dflong)



y<-dflong$score


(BBeta<-solve(t(X)%*%solve(sig)%*%X)%*%t(X)%*%solve(sig)%*%y)
BBeta


(b<-14.14214^2*diag(1,10)%*%t(Z)%*%solve(sig)%*%(y-X%*%BBeta))

(Y<-X%*%BBeta + Z%*%b)
y

str(dflong)
dflong$score<-(dflong$score)

try<-lme(score~1, dflong, list(dflong=~1), method="ML")



summary(model2)
