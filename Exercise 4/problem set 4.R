### Statistical Modeling 1 
### Problem set 4
############################
### Question 1


data<-read.table("leukoplakia.txt", sep="\t", dec=".", header=TRUE)
attach(data)

## a)

interaction.plot(factor(Alcohol), factor(Smoker), Leukoplakia)
table(Alcohol, Smoker)

model<-glm(Leukoplakia~factor(Alcohol)+factor(Smoker), family=binomial(link="logit"), data=data)
summary(model)

interaction.plot(factor(Alcohol), factor(Smoker), fitted(model,type="response"))
data.frame(data, fitted(model,type="response"))
newdata<-data.frame(Alcohol=3, Smoker=1)
predict(model, newdata=newdata, type="response")

##  Some further considerations

model.ancova<-glm(Leukoplakia~Alcohol*factor(Smoker), family=binomial(link="logit"), data=data)
summary(model.ancova)
newdata<-expand.grid(Alcohol=1:4, Smoker=0:1)
pred<-predict(model.ancova, newdata=newdata, type="response")
interaction.plot(newdata$Alcohol,newdata$Smoker, pred)

model.interaction<-glm(Leukoplakia~factor(Alcohol)*factor(Smoker), family=binomial(link="logit"), data=data)
summary(model.interaction)
newdata<-expand.grid(Alcohol=1:4, Smoker=0:1)
pred<-predict(model.interaction, newdata=newdata, type="response")
interaction.plot(newdata$Alcohol,newdata$Smoker, pred)


## b)

model.H0<-glm(Leukoplakia~factor(Alcohol), family=binomial(link="logit"))
interaction.plot(factor(Alcohol), factor(Smoker), fitted(model.H0,type="response"))
anova(model.H0, model, test="Chi")
anova(model.H0, model, test="Chi")$Deviance[2]


## c)

newdata<-data.frame(Alcohol=c(3,4), Smoker=1)
pred<-predict(model, newdata=newdata, type="response")
psi<-(pred[2]/(1-pred[2]))/(pred[1]/(1-pred[1]))
psi

(pred[1]/(1-pred[1]))/(pred[2]/(1-pred[2]))
1/psi

## d)

model.H1quasi<-glm(Leukoplakia~factor(Alcohol)*factor(Smoker), family=quasibinomial(link="logit"))
summary(model.H1quasi)
model.H0quasi<-glm(Leukoplakia~factor(Alcohol)+factor(Smoker), family=quasibinomial(link="logit"))
summary(model.H0quasi)

anova(model.H0quasi, model.H1quasi, test="F")
anova(model.H0quasi, model.H1quasi, test="F")$F[2]

model.H1<-glm(Leukoplakia~factor(Alcohol)*factor(Smoker), family=binomial(link="logit"))
model.H0<-glm(Leukoplakia~factor(Alcohol)+factor(Smoker), family=binomial(link="logit"))
anova(model.H0, model.H1, test="Chi")

## e)

model<-glm(Leukoplakia~factor(Alcohol)+factor(Smoker), family=binomial(link="logit"), data=data)
summary(model)
model.probit<-glm(Leukoplakia~factor(Alcohol)+factor(Smoker), family=binomial(link="probit"), data=data)
summary(model.probit)
model.cauchy<-glm(Leukoplakia~factor(Alcohol)+factor(Smoker), family=binomial(link="cauchit"), data=data)
summary(model.cauchy)
model.cloglog<-glm(Leukoplakia~factor(Alcohol)+factor(Smoker), family=binomial(link="cloglog"), data=data)
summary(model.cloglog)

AIC(model)
AIC(model.probit)
AIC(model.cauchy)
AIC(model.cloglog)

par(mfrow=c(2,2))
interaction.plot(factor(Alcohol), factor(Smoker), fitted(model,type="response"))
interaction.plot(factor(Alcohol), factor(Smoker), fitted(model.probit,type="response") )
interaction.plot(factor(Alcohol), factor(Smoker), fitted(model.cauchy,type="response") )
interaction.plot(factor(Alcohol), factor(Smoker), fitted(model.cloglog,type="response") )

mean(residuals(model,type="response")^2)
mean(residuals(model.probit,type="response")^2)
mean(residuals(model.cauchy,type="response")^2)
mean(residuals(model.cloglog,type="response")^2)


############################
### Question 2


data<-read.table("applejuiceCRA7152.txt", sep="\t", dec=".", header=TRUE)
attach(data)

## a) and b)

model<-glm(Growth~pH+Nisin+Temperature+Brix, data=data, family=binomial(link="logit"))
summary(model)


model.full<-glm(Growth~pH*Nisin*Temperature*Brix, data=data, family=binomial(link="logit"))
summary(model.full)


training<-data[-c(7,34,56,78),]
new<-data[c(7,34,56,78),]
plot(jitter(fitted(model.full, type="response")),jitter(Growth))
model.full<-glm(Growth~pH*Nisin*Temperature*Brix, data=training, family=binomial(link="logit"))
summary(model.full)
predict(model.full, newdata=new, type="response")


step(model)
AIC(model)

model.12<-glm(Growth~pH*Nisin+Temperature+Brix, data=data, family=binomial(link="logit"))
summary(model.12)

model.P<-glm(Growth~pH+Nisin+Temperature+Brix, data=data, family=binomial(link="probit"))
summary(model.P)
model.C<-glm(Growth~pH+Nisin+Temperature+Brix, data=data, family=binomial(link="cauchit"))
summary(model.C)
model.cll<-glm(Growth~pH+Nisin+Temperature+Brix, data=data, family=binomial(link="cloglog"))
summary(model.cll)

plot(Nisin, fitted(model, type="response"))
plot(Nisin, fitted(model.C, type="response"))

plot(fitted(model, type="response"), fitted(model.C, type="response"))

mean(residuals(model,type="response")^2)
mean(residuals(model.P,type="response")^2)
mean(residuals(model.C,type="response")^2)
mean(residuals(model.cll,type="response")^2)


### solution for b)

newdata<-data.frame(pH=4.5,Nisin=20,Temperature=30,Brix=17)
predict(model, newdata=newdata, type="response")
predict(model.C, newdata=newdata, type="response")

newdata<-data.frame(pH=4.5,Nisin=0:70,Temperature=30,Brix=17)
pred.L<-predict(model, newdata=newdata, type="response")
pred.C<-predict(model.C, newdata=newdata, type="response")

plot(jitter(Nisin, 0.4), jitter(Growth,0.4))
lines(newdata$Nisin, pred.L, lwd=3)
lines(newdata$Nisin, pred.C, lwd=3, col="red")


## c)

newdata<-data.frame(pH=4.5,Nisin=20,Temperature=30,Brix=17)
eta<-predict(model, newdata=newdata, type="link", se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

# logit
lowerbound<-exp(link.lowerbound)/(1+exp(link.lowerbound))
upperbound<-exp(link.upperbound)/(1+exp(link.upperbound))
lowerbound
upperbound

plogis(link.lowerbound)
plogis(link.upperbound)


# probit

newdata<-data.frame(pH=4.5,Nisin=20,Temperature=30,Brix=17)
eta<-predict(model.P, newdata=newdata, type="link", se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

pnorm(link.lowerbound)
pnorm(link.upperbound)


# cauchy

newdata<-data.frame(pH=4.5,Nisin=20,Temperature=30,Brix=17)
eta<-predict(model.C, newdata=newdata, type="link", se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

pcauchy(link.lowerbound)
pcauchy(link.upperbound)



newdata<-data.frame(pH=4.5,Nisin=0:70,Temperature=30,Brix=17)
pred.L<-predict(model, newdata=newdata, type="response")
pred.C<-predict(model.C, newdata=newdata, type="response")

plot(jitter(Nisin, 0.4), jitter(Growth,0.4))
lines(newdata$Nisin, pred.L, lwd=3)
lines(newdata$Nisin, pred.C, lwd=3, col="red")

eta<-predict(model.C, newdata=newdata, type="link", se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit

low<-pcauchy(link.lowerbound)
up<-pcauchy(link.upperbound)


eta<-predict(model, newdata=newdata, type="link", se.fit = TRUE) 
link.lowerbound<-eta$fit-qnorm(0.975)*eta$se.fit
link.upperbound<-eta$fit+qnorm(0.975)*eta$se.fit
low.L<-plogis(link.lowerbound)
up.L<-plogis(link.upperbound)

lines(newdata$Nisin, low, lwd=3, lty=3, col="red")
lines(newdata$Nisin, up, lwd=3, lty=3, col="red")

lines(newdata$Nisin, low.L, lwd=3, lty=3, col="black")
lines(newdata$Nisin, up.L, lwd=3, lty=3, col="black")


## d)

model<-glm(Growth~pH+Nisin+Temperature+Brix, data=data, family=binomial(link="logit"))
summary(model)
newdata<-data.frame(pH=4.5,Nisin=20,Temperature=30,Brix=17)

mu.f<-predict(model, newdata=newdata, type="response")
YS.pred<-100*mu.f

mu.hat<-predict(model, newdata=data, type="response")
n<-dim(data)[1]

e.b<-numeric()

for(b in 1:1000){
  
  yb<-numeric()
  for(i in 1:n){
    
    yb[i]<-sample(0:1,1,prob=c(1-mu.hat[i],mu.hat[i]))
    
  }
  
  model.b<-glm(yb~pH+Nisin+Temperature+Brix, family=binomial(link="logit"), data=data)
  newdata<-data.frame(pH=4.5,Nisin=20,Temperature=30,Brix=17)
  mu.fb<-predict(model.b, newdata=newdata, type="response")
  YS.predB<-100*mu.fb
  
  yf.b<-sample(0:1,100,prob=c(1-mu.f,mu.f), replace=TRUE)
  
  e.b[b]<-sum(yf.b)-YS.predB
  
}

var.error<-var(e.b)
var.error

z<-qnorm(c(0.1), lower.tail=FALSE)
lower.bound<-YS.pred-z*sqrt(var.error)
upper.bound<-YS.pred+z*sqrt(var.error)
lower.bound
upper.bound



