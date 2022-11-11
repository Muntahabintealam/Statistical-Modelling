### Statistical Modeling 1
### Weekly assingments 1

############################
### Question 1

library(tidyverse)
library(plotly)

data<-read.table("paper.txt", sep="\t", dec=".", header=TRUE)
attach(data)

coplot(strength~hardwood |pressure)
plot(hardwood,strength)
plot(pressure, strength)

plot_ly()%>%
  add_markers(x=~hardwood,y=~pressure, z=~strength) 

model.main<-lm(strength~hardwood+pressure)
summary(model.main)

# a)

coef(model.main)
coef(model.main)[3]

# b)

sigma(model.main)^2

# c)

fitted(model.main)
fitted(model.main)[1]
head(data)
predict(model.main, newdata=data.frame(hardwood=2, pressure=400))

# d)

newdata<-data.frame(hardwood=7,pressure=550)
predict(model.main, newdata=newdata, interval="confidence", level=0.95)

# e)

newdata<-data.frame(hardwood=7,pressure=550)
predict(model.main, newdata=newdata, interval="prediction", level=0.8)

newdata<-data.frame(hardwood=seq(1,9,0.1),pressure=550)
pred<-predict(model.main, newdata=newdata, interval="prediction",level=0.8)
pred1<-predict(model.main, newdata=newdata, interval="prediction",level=0.5)
pred2<-predict(model.main, newdata=newdata, interval="prediction",level=0.25)


plot(hardwood, strength, type="n")
points(hardwood, strength, pch=19)
lines(newdata$hardwood,pred[,1])
lines(newdata$hardwood,pred[,2],col="red")
lines(newdata$hardwood,pred[,3],col="red")
lines(newdata$hardwood,pred1[,2],col="green")
lines(newdata$hardwood,pred1[,3],col="green")
lines(newdata$hardwood,pred2[,2],col="yellow")
lines(newdata$hardwood,pred2[,3],col="yellow")


fit<-predict(model.main, newdata=newdata, interval="confidence",level=0.95)
lines(newdata$hardwood,fit[,2],col="blue")
lines(newdata$hardwood,fit[,3],col="blue")


newdata<-expand.grid(hardwood=seq(1,9,0.1),pressure=seq(400,650,1))
mu.hat<-predict(model.main, newdata=newdata)
plot_ly()%>%
  add_markers(x=~newdata$hardwood,y=~newdata$pressure, z=~mu.hat)


# f)

model.1<-lm(strength~hardwood)
anova(model.1, model.main,test="F")
anova(model.1, model.main,test="F")$F[2]

summary(model.main)
model.2<-lm(strength~pressure)
anova(model.2, model.main,test="F")



############################
### Question 2

data<-read.table("makiwaraboard.txt", sep="\t", dec=".", header=TRUE)
attach(data)

plot_ly()%>%
  add_markers(x=~WoodType,y=~BoardType, z=~Deflection)

model.main<-lm(Deflection~factor(WoodType)+factor(BoardType))
summary(model.main)
interaction.plot(WoodType, BoardType, fitted(model.main))

model.12<-lm(Deflection~factor(WoodType)+factor(BoardType)+factor(WoodType):factor(BoardType))
summary(model.12)
interaction.plot(WoodType, BoardType, fitted(model.12))

# a)

newdata<-data.frame(WoodType="Oak", BoardType="Tapered")
predict(model.main, newdata=newdata)

# b)

interaction.plot(WoodType, BoardType, fitted(model.main))
newdata<-expand.grid(WoodType=levels(factor(WoodType)), BoardType=levels(factor(BoardType)))

predict(model.main, newdata=newdata)

data.frame(newdata,predict(model.main, newdata=newdata))
# right answer i. - Cherry

newdata<-expand.grid(WoodType=levels(factor(WoodType)), BoardType=levels(factor(BoardType)))
data.frame(newdata,predict(model.12, newdata=newdata))
interaction.plot(WoodType, BoardType, fitted(model.12))

# c)

summary(model.12)

betahat<-coef(model.12)

k1<-c(0,0,0,0,0,1,0,0)
k2<-c(0,0,0,0,0,0,1,0)
k3<-c(0,0,0,0,0,0,0,1)

K<-cbind(k1,k2,k3)

q<-3
Wald<-(t(t(K)%*%betahat)%*%solve(t(K)%*%vcov(model.12)%*%K)%*%t(K)%*%betahat)/q
Wald
p.value<-pf(Wald, q, summary(model.12)$df[2], lower.tail = FALSE)
p.value

anova(model.main,model.12, test="F")

# d)

newdata<-data.frame(WoodType=c("Cherry","Oak"), BoardType=c("Stacked","Tapered"))
mu.hat<-predict(model.12, newdata=newdata)


x1<-cbind(c(1,1,0,0,0,0,0,0)) ### Cherry and Stacked
x2<-cbind(c(1,0,0,1,1,0,0,1)) ## Oak and Tapered
betahat<-cbind(coef(model.12))

pred<-(t(x2)-t(x1))%*%betahat
pred



# e)

newdata<-data.frame(WoodType=c("Oak","Cherry"), BoardType=c("Tapered","Stacked"))
predict(model.12, newdata=newdata)

x1<-cbind(c(1,1,0,0,0,0,0,0)) ### Cherry and Stacked
x2<-cbind(c(1,0,0,1,1,0,0,1)) ## Oak and Tapered
betahat<-cbind(coef(model.12))

pred<-(t(x2)-t(x1))%*%betahat
pred


# 80% prediction inteval,  lower and upper limits

sigma2<-sigma(model.12)^2
lower<-pred-qt(0.9,df=328)*sqrt(sigma2*(2+(t(x2)-t(x1))%*%solve(vcov(model.12))%*%(x2-x1)))
upper<-pred+qt(0.9,df=328)*sqrt(sigma2*(2+(t(x2)-t(x1))%*%solve(vcov(model.12))%*%(x2-x1)))
lower
upper
T<-pred/sqrt(sigma2*(2+(t(x2)-t(x1))%*%solve(vcov(model.12))%*%(x2-x1)))
T
p<-2*pt(abs(T),df=328, lower.tail = FALSE)
p























