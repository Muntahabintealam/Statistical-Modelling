### Statistical Modeling 1 
### Problem Set 5
### 2022

############################
### Question 1

data<-read.table("canopycover.txt", sep="\t", dec=".", header=TRUE)
attach(data)

plot(basalarea,canopycover)
coplot(canopycover~basalarea|species)
plot(dbh.mean,canopycover)
coplot(canopycover~dbh.mean|species)


## a)

## Best assumption: Beta-distribution with logit link. 

## b)

library(betareg)

model.main<-betareg(canopycover~basalarea+species, data=data, link=c("logit"))
summary(model.main)

model.12<-betareg(canopycover~basalarea*species, data=data, link=c("logit"))
summary(model.12)

plot(basalarea,canopycover)
points(basalarea[species=="pine"], fitted(model.main, type="response")[species=="pine"], col="red", pch=19, cex=2)
points(basalarea[species=="spruce"], fitted(model.main, type="response")[species=="spruce"], col="blue", pch=19, cex=2)
points(basalarea[species=="pine"], fitted(model.12, type="response")[species=="pine"], col="orange", pch=19, cex=2)
points(basalarea[species=="spruce"], fitted(model.12, type="response")[species=="spruce"], col="violet", pch=19, cex=2)

model.main<-betareg(canopycover~basalarea+dbh.mean+species, data=data, link=c("logit"))
summary(model.main)

coplot(fitted(model.main, type="response")~basalarea| dbh.mean*species )

newdata<-data.frame(basalarea=20, dbh.mean=15, species="pine")
predict(model.main, newdata=newdata, type="response")

model.1323<-betareg(canopycover~(basalarea+dbh.mean)*species, data=data, link=c("logit"))
summary(model.1323)

AIC(model.main)
AIC(model.1323)

plot(fitted(model.main, type="response"),fitted(model.1323, type="response"))

## c)

model.main<-betareg(canopycover~basalarea+dbh.mean+species, data=data, link=c("logit"))
summary(model.main)
newdata<-data.frame(basalarea=20, dbh.mean=15, species="pine")
predict(model.main, newdata=newdata, type="response")

eta<-predict(model.main, newdata=newdata, type="link") 
xf<-t(t(c(1,20,15,0)))

eta<-predict(model.main, newdata=newdata, type="link") 
cov.eta<-t(xf)%*%vcov(model.main)[-5,-5]%*%xf

link.lower<-eta-qnorm(0.975)*sqrt(cov.eta)
link.upper<-eta+qnorm(0.975)*sqrt(cov.eta)
link.lower
link.upper

lowerbound<-exp(eta-qnorm(0.975)*sqrt(cov.eta))/(1+exp(eta-qnorm(0.975)*sqrt(cov.eta)))
upperbound<-exp(eta+qnorm(0.975)*sqrt(cov.eta))/(1+exp(eta+qnorm(0.975)*sqrt(cov.eta)))
lowerbound
upperbound

plogis(link.lower)
plogis(link.upper)


###########

model.normal<-glm(canopycover~basalarea, data=data, family=gaussian(link="logit"))
summary(model.normal)
plot(basalarea, fitted(model.normal, type="response"))

model.normal<-glm(canopycover~basalarea+dbh.mean+species, data=data, family=gaussian(link="logit"))
summary(model.normal)
eta.normal<-predict(model.normal, newdata=newdata, type="link", se.fit=TRUE) 

lowerbound<-exp(eta.normal$fit-qnorm(0.975)*eta.normal$se.fit)/(1+exp(eta.normal$fit-qnorm(0.975)*eta.normal$se.fit))
upperbound<-exp(eta.normal$fit+qnorm(0.975)*eta.normal$se.fit)/(1+exp(eta.normal$fit+qnorm(0.975)*eta.normal$se.fit))
lowerbound
upperbound


model.IG<-glm(canopycover~basalarea+dbh.mean+species, data=data, family=inverse.gaussian(link="logit"))
summary(model.IG)
eta.IG<-predict(model.IG, newdata=newdata, type="link", se.fit=TRUE) 
lowerbound<-exp(eta.IG$fit-qnorm(0.975)*eta.IG$se.fit)/(1+exp(eta.IG$fit-qnorm(0.975)*eta.IG$se.fit))
upperbound<-exp(eta.IG$fit+qnorm(0.975)*eta.IG$se.fit)/(1+exp(eta.IG$fit+qnorm(0.975)*eta.IG$se.fit))
lowerbound
upperbound



## d)

library(mvtnorm)

eta.f<-predict(model.main, newdata=newdata, type="link")
phi.hat<-coef(model.main)[5]

xf<-t(t(c(1,20,15,0)))
etarow<-c(t(xf),0)
phirow<-c(rep(0,length(xf)),1)
A<-rbind(etarow,phirow)

cov.etaphi<-A%*%vcov(model.main)%*%t(A)
etaphi.star<-rmvnorm(1000, mean = c(eta.f,phi.hat), sigma = cov.etaphi)

muf.star<-exp(etaphi.star[,1])/(1+exp(etaphi.star[,1]))
phi.star<-etaphi.star[,2]
p.star<-muf.star*phi.star
q.star<-phi.star*(1-muf.star)

yf.star<-rbeta(1000, shape1=p.star, shape2=q.star)

lower.bound<-quantile(yf.star, c(0.1))
upper.bound<-quantile(yf.star, 1-c(0.1))
lower.bound
upper.bound

## e)

model.interaction<-betareg(canopycover~basalarea*species+dbh.mean*species, data=data, link=c("logit"))
summary(model.interaction)

model.All2interaction<-betareg(canopycover~basalarea*species+basalarea*dbh.mean+dbh.mean*species, data=data, link=c("logit"))
summary(model.All2interaction)

AIC(model.main)
AIC(model.interaction)
AIC(model.All2interaction)

library(lmtest)
lrtest(model.main, model.interaction)

model.H0<-betareg(canopycover~basalarea*species, data=data, link=c("logit"))
summary(model.H0)
lrtest(model.H0, model.interaction)
waldtest(model.H0, model.interaction)


############################
### Question 2


data<-read.table("NitrogenYield.txt", sep="\t", dec=".", header=TRUE)
attach(data)

plot(Nitrogen, Yield)
library(nlme)

## a) 

model.p<-lm(Yield~Nitrogen+I(Nitrogen^2), data=data)
summary(model.p)
coef(model.p)[3]

plot(Nitrogen, Yield)
lines(Nitrogen, fitted(model.p), col="red")


## b) 

model.e<-glm(Yield~log(Nitrogen),family=gaussian(link="log"), data=data)
summary(model.e)
predict(model.e, newdata=data.frame(Nitrogen=150), type="response")

plot(Nitrogen, Yield)
lines(Nitrogen, fitted(model.p), col="red")
lines(Nitrogen, fitted(model.e, type="response"), col="blue")

## c) 

model.a<-nls(Yield~SSasymp(Nitrogen, Asym,R0,lrc), data=data)
summary(model.a)

plot(Nitrogen, Yield)
lines(Nitrogen, fitted(model.p), col="red", lwd=3)
lines(Nitrogen, fitted(model.e, type="response"), col="blue")
lines(Nitrogen, fitted(model.a, type="response"), col="black", lwd=3)

## d) 

model.mm<-nls(Yield~SSmicmen(Nitrogen, Vm, K), data=data)
summary(model.mm)
newdata<-data.frame(Nitrogen=150)
predict(model.mm,newdata=newdata)

plot(Nitrogen, Yield)
lines(Nitrogen, fitted(model.p), col="red")
lines(Nitrogen, fitted(model.e, type="response"), col="blue")
lines(Nitrogen, fitted(model.a, type="response"), col="black")
lines(Nitrogen, fitted(model.mm, type="response"), col="green", lwd=3)

gmodel<-glm(Yield~I(1/Nitrogen), family=gaussian(link="inverse"), data=data)

lines(Nitrogen, fitted(gmodel, type="response"), col="brown", lwd=3)


AIC(model.p)
AIC(model.e)
AIC(model.a)
AIC(model.mm)


## e) 

model.a<-nls(Yield~SSasymp(Nitrogen, Asym,R0,lrc), data=data)
summary(model.a)

beta<-coef(model.a)
cov.beta<-vcov(model.a)

predict(model.a, newdata=data.frame(Nitrogen=150), type="response")

library(mvtnorm)
beta.star<-rmvnorm(1000, mean = beta, sigma = cov.beta)

Asym<-beta.star[,1]
R0<-beta.star[,2]
lrc<-beta.star[,3]

mu.star<-Asym+(R0-Asym)*exp(-exp(lrc)*newdata$Nitrogen)

conf.lowerbound<-quantile(mu.star, c(0.025))
conf.upperbound<-quantile(mu.star, c(0.975))
conf.lowerbound
conf.upperbound

sigma2<-sigma(model.a)[1]^2
yf.star<-rnorm(10000, mean=mu.star, sd=sqrt(sigma2))
pred.lowerbound<-quantile(yf.star, c(0.1))
pred.upperbound<-quantile(yf.star, c(0.9))

pred.lowerbound
pred.upperbound

newdata<-data.frame(Nitrogen=0:200)

pred.lowerbound<-numeric()
pred.upperbound<-numeric()

for(i in 1:dim(newdata)[1]){
  
  mu.star<-Asym+(R0-Asym)*exp(-exp(lrc)*newdata$Nitrogen[i])
  
  sigma2<-sigma(model.a)[1]^2
  yf.star<-rnorm(10000, mean=mu.star, sd=sqrt(sigma2))
  pred.lowerbound[i]<-quantile(yf.star, c(0.1))
  pred.upperbound[i]<-quantile(yf.star, c(0.9))
  
}

plot(Nitrogen, Yield)
lines(Nitrogen, fitted(model.a, type="response"), col="black")
lines(newdata$Nitrogen, pred.lowerbound, col="red")
lines(newdata$Nitrogen, pred.upperbound, col="red")



############################
### Question 3

data<-read.table("caffeine.txt", sep="\t", header=TRUE, dec=".")
attach(data)
head(data)

## a)

boxplot(Caffeine~Brand+Formulation)

interaction.plot(Brand, Formulation, Caffeine)
coplot(Caffeine~factor(Brand)|factor(Formulation))
coplot(Caffeine~factor(Formulation)|factor(Brand))

model.a<-lm(Caffeine~Brand+Formulation)
summary(model.a)
interaction.plot(Brand, Formulation, fitted(model.a))

model.b<-lm(Caffeine~Brand*Formulation)
summary(model.b)
interaction.plot(Brand, Formulation, fitted(model.b))

plot(fitted(model.b), residuals(model.b))

anova(model.a, model.b, test="F")

model.c<-glm(Caffeine~Brand+Formulation, data=data, family=gaussian(link="log"))
summary(model.c)
interaction.plot(Brand, Formulation, fitted(model.c, type="response"))

model.d<-glm(Caffeine~Brand+Formulation, data=data, family=gaussian(link="inverse"))
summary(model.d)
interaction.plot(Brand, Formulation, fitted(model.d, type="response"))

model.d2<-glm(Caffeine~Brand*Formulation, data=data, family=gaussian(link="inverse"))
summary(model.d2)
interaction.plot(Brand, Formulation, fitted(model.d2, type="response"))

anova(model.d, model.d2, test="F")

plot(fitted(model.b, type="response"),residuals(model.b, type="pearson")^2)
shapiro.test(residuals(model.Normal, type="pearson"))


## b)

xf<-t(t(c(1,0,0,0)))

pred<-t(xf)%*%coef(model.b)
X<-model.matrix(model.b)
sigma2<-summary(model.b)$sigma^2
cov.error<-sigma2*(1+t(xf)%*%solve(t(X)%*%X)%*%xf)
cov.error

t<-qt(0.9, df=236)

lowerbound<-pred-t*sqrt(cov.error)
upperbound<-pred+t*sqrt(cov.error)
lowerbound
upperbound


## c)

model.H0<-lm(Caffeine~Formulation)
summary(model.H0)
interaction.plot(Brand, Formulation, fitted(model.H0))

anova(model.H0, model.b, test="F")





