#Data
library(haven)#library(foreign)
#clhls=read_sav(file.choose()) #clhls2019
clhls18=read_sav(file.choose())
clhls18=data.frame(clhls18)
View(clhls18)
#View(clhls)
dim(clhls18)
attach(clhls18)
colnames(clhls18)
clhls18=clhls18[complete.cases(clhls18[,c("hukou","residenc","trueage")]),]
citybg=clhls18[clhls18$hukou==1&clhls18$residenc!=3,] #citizen
oldcitybg=citybg[citybg$trueage>=65,] #age selecting
dim(oldcitybg)
attach(oldcitybg)

#Data Cleaning
#Sex,age and marital status
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("a1","trueage","f41")]),]

#Psy
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("b31","b32","b333","b34","b35","b36","b37","b38","b39","b310a")]),]
oldcitybg=oldcitybg[which(oldcitybg$b31!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b32!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b33!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b34!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b35!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b36!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b37!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b38!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b39!=8),]
oldcitybg=oldcitybg[which(oldcitybg$b310a!=8),]
oldcitybg=oldcitybg[oldcitybg$b48==1,]
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("b41","b42","b43","b44","b45","b46","b47")]),]
dim(oldcitybg)

#Fertility Behavior
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("f10","f10a","f101","f102")]),]
sum(is.na(oldcitybg$f10))
dim(oldcitybg)

#Physical
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("e1","e2","e3","e4","e5","e6","e7","e8","e9","e10","e14")]),]
dim(oldcitybg)

#Social
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("d11b2","d11b3","d112c","d112d","d11h")]),]
dim(oldcitybg)

#Self-assessed
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("b12")]),]
oldcitybg=oldcitybg[which(oldcitybg$b12!=8),]
dim(oldcitybg)


#Grouping "a1","trueage","f41"
sum(oldcitybg$a1==1)
sum(oldcitybg$a1==2)
oldcitybg$trueage[which(oldcitybg$trueage>=65&oldcitybg$trueage<80)]=1
oldcitybg$trueage[which(oldcitybg$trueage>=80)]=2
sum(oldcitybg$trueage==1)
sum(oldcitybg$trueage==2)
sum(oldcitybg$f41==1|oldcitybg$f41==2)
sum(oldcitybg$f41!=1&oldcitybg$f41!=2)
oldcitybg$f41[which(oldcitybg$f41==1|oldcitybg$f41==2)]=1
oldcitybg$f41[which(oldcitybg$f41!=1&oldcitybg$f41!=2)]=2


#anxiety b41-b47
#logistic
anx=data.frame(oldcitybg$b41,oldcitybg$b42,oldcitybg$b43,oldcitybg$b44,oldcitybg$b45,oldcitybg$b46,oldcitybg$b47)
anx.t=apply(anx, 1, sum)
summary(anx.t)
anx.t[which(anx.t<=7)]=2
anx.t[which(anx.t<=14&anx.t>7)]=1
anx.t[which(anx.t>14)]=0
sum(anx.t==0)
sum(anx.t==1)
sum(anx.t==2)



#CDSE scale 
oldcitybg$b35=oldcitybg$b35*(-1)
oldcitybg$b37=oldcitybg$b37*(-1)
dep=data.frame(oldcitybg$b31,oldcitybg$b32,oldcitybg$b33,oldcitybg$b34,oldcitybg$b35,oldcitybg$b36,
               oldcitybg$b37,oldcitybg$b38,oldcitybg$b39)
dep.t=apply(dep, 1, sum)
summary(dep.t)
dep.t[which(dep.t<=18)]=0
dep.t[which(dep.t<=24&dep.t>18)]=1
dep.t[which(dep.t<=33&dep.t>24)]=2
sum(dep.t==0)
sum(dep.t==1)
sum(dep.t==2)

#psy total
apsy=dep.t+anx.t
apsy[which(apsy<2)]=0
apsy[which(apsy<4&apsy>=2)]=1
apsy[which(apsy==4)]=2
sum(apsy==0)
sum(apsy==1)
sum(apsy==2)

#descriptive
sum(apsy==0&oldcitybg$a1==1)
sum(apsy==1&oldcitybg$a1==1)
sum(apsy==2&oldcitybg$a1==1)

sum(apsy==0&oldcitybg$trueage==1)
sum(apsy==1&oldcitybg$trueage==1)
sum(apsy==2&oldcitybg$trueage==1)

sum(apsy==0&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(apsy==1&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(apsy==2&oldcitybg$f41!=1&oldcitybg$f41!=2)


#physical
#ADL E1-E6
adl=data.frame(oldcitybg$e1,oldcitybg$e2,oldcitybg$e3,oldcitybg$e4,oldcitybg$e5,oldcitybg$e6)
adl.t=apply(adl, 1, sum)
summary(adl.t)
adl.t[which(adl.t!=6)]=0
adl.t[which(adl.t==6)]=1
sum(adl.t==1)

sum(adl.t==0&oldcitybg$a1==1)
sum(adl.t==1&oldcitybg$a1==1)


sum(adl.t==0&oldcitybg$trueage==1)
sum(adl.t==1&oldcitybg$trueage==1)

sum(adl.t==0&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(adl.t==1&oldcitybg$f41!=1&oldcitybg$f41!=2)


#IADL e7-e10 e14负
iadl=data.frame(oldcitybg$e7,oldcitybg$e8,oldcitybg$e9,oldcitybg$e10,oldcitybg$e14)
iadl.t=apply(iadl, 1, sum)
summary(iadl.t)
iadl.t[which(iadl.t>5)]=0
iadl.t[which(iadl.t==5)]=1
sum(iadl.t==0)
sum(iadl.t==1)


sum(iadl.t==0&oldcitybg$a1==1)
sum(iadl.t==1&oldcitybg$a1==1)

sum(iadl.t==0&oldcitybg$trueage==1)
sum(iadl.t==1&oldcitybg$trueage==1)

sum(iadl.t==0&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(iadl.t==1&oldcitybg$f41!=1&oldcitybg$f41!=2)


#Social 
soc=data.frame(oldcitybg$d11b2,oldcitybg$d11b3,oldcitybg$d112c,oldcitybg$d112d,oldcitybg$d11h)
soc.t=apply(soc, 1, sum)
summary(soc.t)
soc.t[which(soc.t<=10)]=2
soc.t[which(soc.t>10&soc.t<=16)]=1
soc.t[which(soc.t>16)]=0
sum(soc.t==0)
sum(soc.t==1)
sum(soc.t==2)

sum(soc.t==0&oldcitybg$a1==1)
sum(soc.t==1&oldcitybg$a1==1)
sum(soc.t==2&oldcitybg$a1==1)

sum(soc.t==0&oldcitybg$trueage==1)
sum(soc.t==1&oldcitybg$trueage==1)
sum(soc.t==2&oldcitybg$trueage==1)

sum(soc.t==0&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(soc.t==1&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(soc.t==2&oldcitybg$f41!=1&oldcitybg$f41!=2)


#self-assessed
self=data.frame(oldcitybg$b12)
summary(self)
self.t=apply(self, 1, sum)
self.t[which(self.t<=2)]=2
self.t[which(self.t==3)]=1
self.t[which(self.t>3)]=0
sum(self.t==0)
sum(self.t==1)
sum(self.t==2)

sum(self.t==0&oldcitybg$a1==1)
sum(self.t==1&oldcitybg$a1==1)
sum(self.t==2&oldcitybg$a1==1)

sum(self.t==0&oldcitybg$trueage==1)
sum(self.t==1&oldcitybg$trueage==1)
sum(self.t==2&oldcitybg$trueage==1)

sum(self.t==0&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(self.t==1&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(self.t==2&oldcitybg$f41!=1&oldcitybg$f41!=2)


#Fertility behavior
no=oldcitybg$f10
no=as.numeric(no)
ra=oldcitybg$f10a/oldcitybg$f10
ra[which(ra<=0)]=1
ra=as.numeric(ra)
bound=oldcitybg$f102-oldcitybg$f101
bound[which(bound<0)]=0

#Controlling variables
sex=oldcitybg$a1
age=oldcitybg$trueage
ms=oldcitybg$f41
sex=as.factor(sex)
age=as.factor(age)
ms=as.factor(ms)


iadl2.m=glm(formula = iadl.t1~no+ra+bound+sex+age+ms, family = binomial, data = sdata)
summary(iadl2.m)
adl2.m=glm(formula = adl.t~no+ra+bound+sex+age+ms, family = binomial, data = sdata)
summary(adl1.m)

#DATA
sdata=data.frame(adl.t,iadl.t,apsy,soc.t,self.t,no,ra,bound,sex,age,ms)
ndata=data.frame(adl.t,iadl.t,apsy,soc.t,self.t,no,ra,bound)

sdata1=sdata[which(sex==1),]
sdata2=sdata[which(sex==2),]
sdata3=sdata[which(age==1),]
sdata4=sdata[which(age==2),]
sdata5=sdata[which(ms==1),]
sdata6=sdata[which(ms==2),]


#Binary logisticn regression
#iadl
library(car)
iadl.m=glm(formula = iadl.t~no, family = binomial, data = ndata)
summary(iadl.m)
iadl1.m=glm(formula = iadl.t~no+ra+bound, family = binomial, data = ndata)
summary(iadl1.m)
step1=step(iadl1.m,direction=c("both"))
summary(step1)
vif(iadl1.m)


#adl
adl1.m=glm(formula = adl.t~no+ra+bound, family = binomial, data = ndata)
summary(adl1.m)
step2=step(adl1.m,direction=c("both"))
summary(step2)
vif(adl1.m)


install.packages("mfx")
library(mfx)
logitmfx(formula = iadl.t~no+ra+bound,atmean=F,data = ndata)
logitmfx(formula = adl.t~no+ra+bound,atmean=F,data = ndata)

#Odds Ratio
logitor(formula = iadl.t~no+ra+bound,data = ndata)


#ordered logit
install.packages("questionr")
library(questionr)
install.packages("VGAM")
library(VGAM)
library(nnet)

#apsy
apsy.m=vglm(formula = apsy~no+ra+bound,family=multinomial, data=ndata)
summary(apsy.m)

step3=step(apsy.m,direction=c(""))
summary(step3)


#soc
soc.m=vglm(formula = soc.t~no+ra+bound,family=multinomial, data=ndata)
summary(soc.m)

#self-assessed
self.m=vglm(formula = self.t~no+ra+bound,family=multinomial, data=ndata)

self.m=vglm(formula = self.t~bound,family=multinomial, data=ndata)
summary(self.m)

#controlling variables
cont.m1=glm(formula = iadl.t~no+ra+bound+sex+age+ms,family=binomial,data=sdata)
summary(cont.m1)
logitmfx(formula = iadl.t~no+ra+bound+sex+age+ms,atmean=F,data = sdata)
logitor(formula = iadl.t~no+ra+bound+sex+age+ms,data = sdata)


cont.m2=glm(formula = adl.t~no+ra+bound+sex+age+ms,family=binomial,data=sdata)
summary(cont.m2)
logitmfx(formula = adl.t~no+ra+bound+sex+age+ms,atmean=F,data = sdata)
logitor(formula = adl.t~no+ra+bound+sex+age+ms,data = sdata)


cont.m3=vglm(formula = self.t~no+ra+bound+sex+age+ms,family=multinomial,data=sdata)
summary(cont.m3)
logitor(formula = self.t~no+ra+bound+sex+age+ms,data = sdata)

cont.m4=vglm(formula = soc.t~no+ra+bound+sex+age+ms,family=multinomial,data=sdata)
summary(cont.m4)

cont.m5=vglm(formula = apsy~no+ra+bound+sex+age+ms,family=multinomial,data=sdata)
summary(cont.m5)

#cross validation
N=length(sdata$iadl.t)
lm <- glm(iadl.t~.,data=ndata)
lm0 <- glm(iadl.t~1,data = ndata)
ave <- rep(0,1000)
for (i in 1:1000){
  ind=sample(2,N,replace=TRUE,prob=c(0.7,0.3))
  dia_train=ndata[ind==1,]
  dia_test=ndata[ind==2,]
  iadl1.m=glm(formula = iadl.t~no+ra+bound, family = binomial, data = dia_train)
  summary(iadl1.m) 
  ave[i] <- sum((dia_test$iadl-predict(adl1.m,newdata = dia_test[,-11]))^2/(length(dia_test)-1))}
MSE_bar = mean(ave)
ano <- anova(lm0,iadl1.m)
SST = ano$RSS[1]
SSE = ano$RSS[2]
SSR = ano$'Sum of Sq'[2]
F = ano$F
