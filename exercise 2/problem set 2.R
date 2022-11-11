### Statistical Modeling 1
### Problem set 2


############################
### Question 1
############################

data<-read.table("canoladiesel.txt", sep="\t", dec=".", header=TRUE)
attach(data)

## a)

plot(Time, Yield)

inverse.normal<-glm(Yield~Time, family=gaussian(link="inverse"), data=data)
summary(inverse.normal)

newdata<-data.frame(Time=40)
predict(inverse.normal,newdata=newdata, type="response")

plot(Time, Yield)
lines(seq(15,45,1), predict(inverse.normal,newdata=data.frame(Time=seq(15,45,1)), type="response"))


## b)

model.identity<-glm(Yield~Time, family=Gamma(link="identity"), data=data)
model.inverse<-glm(Yield~Time, family=Gamma(link="inverse"), data=data)
model.log<-glm(Yield~Time, family=Gamma(link="log"), data=data)

plot(Time, Yield)
lines(seq(15,45,1), predict(model.identity,newdata=data.frame(Time=seq(15,45,1)), type="response"), col="blue")
lines(seq(15,45,1), predict(model.inverse,newdata=data.frame(Time=seq(15,45,1)), type="response"), col="black")
lines(seq(15,45,1), predict(model.log,newdata=data.frame(Time=seq(15,45,1)), type="response"), col="red")


AIC(model.identity)
AIC(model.inverse)
AIC(model.log)

### Log-link!

# MSE values

mean(residuals(model.identity, type="response")^2)
mean(residuals(model.inverse, type="response")^2)
mean(residuals(model.log, type="response")^2)

## c)

model.Gamma<-glm(Yield~Time+Temp+Methanol, family=Gamma(link="log"), data=data)
summary(model.Gamma)

newdata<-data.frame(Time=40,Temp=260,Methanol=1.75)
pred<-predict(model.Gamma, newdata=newdata, type="response")
pred

eta<-predict(model.Gamma, type="link", newdata=newdata, se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

lower<-exp(link.lowerbound) 
upper<-exp(link.upperbound)
c(lower,pred,upper)

### Extra -- Gamma vs. Normal

newdata<-data.frame(Time=seq(15,45,1))
pred<-predict(model.log, newdata=newdata, type="response")

eta<-predict(model.log, type="link", newdata=newdata, se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

lower<-exp(link.lowerbound) 
upper<-exp(link.upperbound)

plot(Time, Yield)
points(Time, Yield, pch=19)
lines(newdata$Time, pred, col="black", lwd=2)
lines(newdata$Time, lower, col="blue", lwd=2)
lines(newdata$Time, upper, col="blue", lwd=2)

model.normal<-glm(Yield~Time, family=gaussian(link="log"), data=data)
pred<-predict(model.normal, newdata=newdata, type="response")

eta<-predict(model.normal, type="link", newdata=newdata, se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

lower<-exp(link.lowerbound) 
upper<-exp(link.upperbound)

lines(newdata$Time, pred, col="green", lwd=2)
lines(newdata$Time, lower, col="red", lwd=2)
lines(newdata$Time, upper, col="red", lwd=2)


## d)

model.Gamma<-glm(Yield~Time+Temp+Methanol, family=Gamma(link="log"), data=data)
model.H0<-glm(Yield~Time+Temp, family=Gamma(link="log"), data=data)
anova(model.H0, model.Gamma, test="F")


## e)

model.Normal<-glm(Yield~Time+Temp+Methanol, family=gaussian(link="log"), data=data)
model.IG<-glm(Yield~Time+Temp+Methanol, family=inverse.gaussian(link="log"), data=data)

pairs(data.frame(fitted(model.Normal, type="response"),
                 fitted(model.Gamma, type="response"),
                 fitted(model.IG, type="response")))

par(mfrow=c(2,2))

plot(fitted(model.Normal, type="response"),residuals(model.Normal, type="pearson")^2)
plot(fitted(model.Gamma, type="response"),residuals(model.Gamma, type="pearson")^2)
plot(fitted(model.IG, type="response"),residuals(model.IG, type="pearson")^2)

shapiro.test(residuals(model.Normal, type="pearson"))
shapiro.test(residuals(model.Gamma, type="pearson"))
shapiro.test(residuals(model.IG, type="pearson"))

## ii. - Gamma or iii. - Inverse Gaussian.


############################
### Question 2
############################

rats<-read.table("ratstime.txt", sep="\t", dec=".", header=TRUE)
attach(rats)

boxplot(time~poison)
boxplot(time~treat)
boxplot(time~poison+treat)

# a)

main.normal<-glm(time~poison+treat, family=gaussian(link="identity"), data=rats)
main.normalL<-glm(time~poison+treat, family=gaussian(link="log"), data=rats)
main.normalI<-glm(time~poison+treat, family=gaussian(link="inverse"), data=rats)

par(mfrow=c(2,2))
interaction.plot(poison, treat, time)
interaction.plot(poison, treat, fitted(main.normal, type="response"))
interaction.plot(poison, treat, fitted(main.normalL, type="response"))
interaction.plot(poison, treat, fitted(main.normalI, type="response"))

AIC(main.normal)
AIC(main.normalL)
AIC(main.normalI)

# MSE values

mean(residuals(main.normal, type="response")^2)
mean(residuals(main.normalL, type="response")^2)
mean(residuals(main.normalI, type="response")^2)

################## Identity Link

main.normal<-glm(time~poison+treat, family=gaussian(link="identity"), data=rats)
main.gamma<-glm(time~poison+treat, family=Gamma(link="identity"), data=rats)
main.IG<-glm(time~poison+treat, family=inverse.gaussian(link="identity"), data=rats)

par(mfrow=c(2,2))
interaction.plot(poison, treat, time)
interaction.plot(poison, treat, fitted(main.normal, type="response"))
interaction.plot(poison, treat, fitted(main.gamma, type="response"))
interaction.plot(poison, treat, fitted(main.IG, type="response"))

par(mfrow=c(2,2))
plot(fitted(main.normal, type="response"), residuals(main.normal, type="pearson")^2)
plot(fitted(main.gamma, type="response"), residuals(main.gamma, type="pearson")^2)
plot(fitted(main.IG, type="response"), residuals(main.IG, type="pearson")^2)

shapiro.test(residuals(main.normal, type="pearson"))
shapiro.test(residuals(main.gamma, type="pearson"))
shapiro.test(residuals(main.IG, type="pearson"))

MSE.normal<-mean(residuals(main.normal, type="response")^2)
MSE.normal
MSE.gamma<-mean(residuals(main.gamma, type="response")^2)
MSE.gamma
MSE.IG<-mean(residuals(main.IG, type="response")^2)
MSE.IG

## My choice: Gamma or Inverse Gaussian

################## Inverse Link

main.normal<-glm(time~poison+treat, family=gaussian(link="inverse"), data=rats)
main.gamma<-glm(time~poison+treat, family=Gamma(link="inverse"), data=rats)
main.IG<-glm(time~poison+treat, family=inverse.gaussian(link="inverse"), data=rats)

par(mfrow=c(2,2))
interaction.plot(poison, treat, time)
interaction.plot(poison, treat, fitted(main.normal, type="response"))
interaction.plot(poison, treat, fitted(main.gamma, type="response"))
interaction.plot(poison, treat, fitted(main.IG, type="response"))

par(mfrow=c(2,2))
plot(fitted(main.normal, type="response"), residuals(main.normal, type="pearson")^2)
plot(fitted(main.gamma, type="response"), residuals(main.gamma, type="pearson")^2)
plot(fitted(main.IG, type="response"), residuals(main.IG, type="pearson")^2)

## https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html

bp.normal<-lm(I(residuals(main.normal, type="pearson")^2)~fitted(main.normal, type="response")) 
bp.gamma<-lm(I(residuals(main.gamma, type="pearson")^2)~fitted(main.gamma, type="response")) 
bp.IG<-lm(I(residuals(main.IG, type="pearson")^2)~fitted(main.IG, type="response")) 
anova(bp.normal)
anova(bp.gamma)
anova(bp.IG)

shapiro.test(residuals(main.normal, type="pearson"))
shapiro.test(residuals(main.gamma, type="pearson"))
shapiro.test(residuals(main.IG, type="pearson"))

MSE.normal<-mean(residuals(main.normal, type="response")^2)
MSE.normal
MSE.gamma<-mean(residuals(main.gamma, type="response")^2)
MSE.gamma
MSE.IG<-mean(residuals(main.IG, type="response")^2)
MSE.IG

## My choice: Inverse Gaussian

## b)

main.gammaIdentity<-glm(time~poison+treat, family=Gamma(link="identity"), data=rats)
summary(main.gammaIdentity)

main.gammaLog<-glm(time~poison+treat, family=Gamma(link="log"), data=rats)
summary(main.gammaLog)

main.gammaInverse<-glm(time~poison+treat, family=Gamma(link="inverse"), data=rats)
summary(main.gammaInverse)

par(mfrow=c(2,2))
interaction.plot(poison, treat, fitted(main.gammaIdentity, type="response"))
interaction.plot(poison, treat, fitted(main.gammaLog, type="response"))
interaction.plot(poison, treat, fitted(main.gammaInverse, type="response"))

AIC(main.gammaIdentity)
AIC(main.gammaLog)
AIC(main.gammaInverse)

mean(residuals(main.gammaIdentity, type="response")^2)
mean(residuals(main.gammaLog, type="response")^2)
mean(residuals(main.gammaInverse, type="response")^2)

full.gammaIdentity<-glm(time~poison*treat, family=Gamma(link="inverse"), data=rats)
interaction.plot(poison, treat, fitted(full.gammaIdentity, type="response"))


# c)

model.H0<-glm(time~poison+treat, family=inverse.gaussian(link="log"), data=rats)
summary(model.H0)
interaction.plot(poison, treat, fitted(model.H0, type="response"))

model.H1<-glm(time~poison*treat, family=inverse.gaussian(link="log"), data=rats)
summary(model.H1)
interaction.plot(poison, treat, fitted(model.H1, type="response"))

anova(model.H0, model.H1, test="F")


# d)

main.gammaLog<-glm(time~poison+treat, family=Gamma(link="log"), data=rats)
summary(main.gammaLog)

newdata<-data.frame(poison="II",treat="B")
pred<-predict(main.gammaLog, newdata=newdata, type="response")
pred

xf<-cbind(c(1,1,0,1,0,0))
xf

phi<-summary(main.gammaLog)$dispersion
Var.Yf<-phi*(pred^2)

D.f<-pred
Var.ef<-Var.Yf+(D.f^2)*t(xf)%*%vcov(main.gammaLog)%*%xf

lower.yf<-pred-qnorm(0.9)*sqrt(Var.ef)
upper.yf<-pred+qnorm(0.9)*sqrt(Var.ef)

lower.yf
upper.yf

## https://cran.r-project.org/web/packages/ciTools/vignettes/ciTools-glm-vignette.html


## e)

newdata<-data.frame(poison=c("I","II"),treat=c("D","B"))
pred<-predict(main.gammaLog, newdata=newdata, type="response")
pred

x1f<-cbind(c(1,0,0,0,0,1)) ### I and D
x2f<-cbind(c(1,1,0,1,0,0)) ## II and B
Xf<-t(cbind(x1f,x2f))

k<-cbind(c(-1,1))

phi<-summary(main.gamma)$dispersion
Var.Y1f<-phi*(pred[1]^2)
Var.Y2f<-phi*(pred[2]^2)

D.f<-diag(pred)   ####!!!!!!!!!

Var.ef<-Var.Y1f+Var.Y2f+t(k)%*%D.f%*%Xf%*%vcov(main.gamma)%*%t(Xf)%*%D.f%*%k

lower.diff<-(pred[2]-pred[1])-qnorm(0.9)*sqrt(Var.ef)
upper.diff<-(pred[2]-pred[1])+qnorm(0.9)*sqrt(Var.ef)
lower.diff
upper.diff



