#加载数据检查数据集
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
citybg=clhls18[clhls18$hukou==1&clhls18$residenc!=3,] #挑选城市人选（未包含城乡结合）
View(citybg)
oldcitybg=citybg[citybg$trueage>=65,] #挑选年龄65及以上人选
dim(oldcitybg)
attach(oldcitybg)

#数据筛选
#性别、年龄、婚姻状况
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("a1","trueage","f41")]),]

#能回答所有心理问题的
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

#生育行为
#f10生育个数,f10a男孩个数，f101初育年龄，f102末育年龄
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("f10","f10a","f101","f102")]),]
sum(is.na(oldcitybg$f10))
dim(oldcitybg)

#生理健康
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("e1","e2","e3","e4","e5","e6","e7","e8","e9","e10","e14")]),]
dim(oldcitybg)

#社会健康
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("d11b2","d11b3","d112c","d112d","d11h")]),]
dim(oldcitybg)

#自评健康
#能回答所有自评问题的
oldcitybg=oldcitybg[complete.cases(oldcitybg[,c("b12")]),]
oldcitybg=oldcitybg[which(oldcitybg$b12!=8),]
dim(oldcitybg)


#控制变量分组"a1","trueage","f41"
#分组
sum(oldcitybg$a1==1)
sum(oldcitybg$a1==2)
oldcitybg$trueage[which(oldcitybg$trueage>=65&oldcitybg$trueage<80)]=1
oldcitybg$trueage[which(oldcitybg$trueage>=80)]=2
sum(oldcitybg$trueage==1)
sum(oldcitybg$trueage==2)
sum(oldcitybg$f41==1|oldcitybg$f41==2)
sum(oldcitybg$f41!=1&oldcitybg$f41!=2)




#分值加总
#心理健康
#焦虑量表b41-b47负
#logistic处理
anx=data.frame(oldcitybg$b41,oldcitybg$b42,oldcitybg$b43,oldcitybg$b44,oldcitybg$b45,oldcitybg$b46,oldcitybg$b47)
anx.t=apply(anx, 1, sum)
summary(anx.t)
anx.t[which(anx.t<=7)]=2
anx.t[which(anx.t<=14&anx.t>7)]=1
anx.t[which(anx.t>14)]=0
sum(anx.t==0)
sum(anx.t==1)
sum(anx.t==2)



#抑郁量表b31-b36，b38-b39负,b37正
#logistic处理
oldcitybg$b35=oldcitybg$b35*(-1)
oldcitybg$b37=oldcitybg$b37*(-1)
dep=data.frame(oldcitybg$b31,oldcitybg$b32,oldcitybg$b33,oldcitybg$b34,oldcitybg$b35,oldcitybg$b36,oldcitybg$b37,oldcitybg$b38,oldcitybg$b39)
dep.t=apply(dep, 1, sum)
summary(dep.t)
dep.t[which(dep.t<=18)]=0
dep.t[which(dep.t<=24&dep.t>18)]=1
dep.t[which(dep.t<=33&dep.t>24)]=2
sum(dep.t==0)
sum(dep.t==1)
sum(dep.t==2)

#心理健康汇总
apsy=dep.t+anx.t
apsy[which(apsy<2)]=0
apsy[which(apsy<4&apsy>=2)]=1
apsy[which(apsy==4)]=2
sum(apsy==0)
sum(apsy==1)
sum(apsy==2)

#分类讨论
sum(apsy==0&oldcitybg$a1==1)
sum(apsy==1&oldcitybg$a1==1)
sum(apsy==2&oldcitybg$a1==1)

sum(apsy==0&oldcitybg$trueage==1)
sum(apsy==1&oldcitybg$trueage==1)
sum(apsy==2&oldcitybg$trueage==1)

sum(apsy==0&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(apsy==1&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(apsy==2&oldcitybg$f41!=1&oldcitybg$f41!=2)





#生理健康
#ADL E1-E6负
adl=data.frame(oldcitybg$e1,oldcitybg$e2,oldcitybg$e3,oldcitybg$e4,oldcitybg$e5,oldcitybg$e6)
adl.t=apply(adl, 1, sum)
summary(adl.t)
#logistic处理
adl.t[which(adl.t!=6)]=0
adl.t[which(adl.t==6)]=1
sum(adl.t==1)

sum(adl.t==0&oldcitybg$a1==1)
sum(adl.t==1&oldcitybg$a1==1)


sum(adl.t==0&oldcitybg$trueage==1)
sum(adl.t==1&oldcitybg$trueage==1)

sum(adl.t==0&oldcitybg$f41!=1&oldcitybg$f41!=2)
sum(adl.t==1&oldcitybg$f41!=1&oldcitybg$f41!=2)




#正向化处理
#oldcitybg$e1=oldcitybg$e1*(-1)
#IADL e7-e10 e14负
iadl=data.frame(oldcitybg$e7,oldcitybg$e8,oldcitybg$e9,oldcitybg$e10,oldcitybg$e14)
iadl.t=apply(iadl, 1, sum)
summary(iadl.t)
#logistic处理
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


#生理健康汇总
phy=adl.t+iadl.t
sum(phy==0)
sum(phy==1)
sum(phy==2)



#社会健康
soc=data.frame(oldcitybg$d11b2,oldcitybg$d11b3,oldcitybg$d112c,oldcitybg$d112d,oldcitybg$d11h)
soc.t=apply(soc, 1, sum)
summary(soc.t)
#logistic处理
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


#自评健康
self=data.frame(oldcitybg$b12)
summary(self)
#logistic处理
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


#生育行为
no=oldcitybg$f10
no=as.numeric(no)
ra=oldcitybg$f10a/oldcitybg$f10
ra[which(ra<=0)]=1
ra=as.numeric(ra)
bound=oldcitybg$f102-oldcitybg$f101
bound[which(bound<0)]=0


ndata=data.frame(adl.t,iadl.t,apsy,soc.t,no,ra,bound)

#二元logistic回归
#iadl
#生育个数单变量
library(car)
iadl.m=glm(formula = iadl.t~no, family = binomial, data = ndata)
summary(iadl.m)
iadl1.m=glm(formula = iadl.t~no+ra+bound, family = binomial, data = ndata)
summary(iadl1.m)
step1=step(iadl1.m,direction=c("both"))
summary(step1)
vif(iadl1.m)

coef(step1)#用以解释模型 
exp(step1$coefficients) #解释Odds比与x的关系 
exp(confint(step1)) #回归系数的置信区间 
xp1<-0/glm.safe1$coefficients[] 
xp1 #求使得pi为0.5的x 
ratio1<-step1$coefficients[]*0.25 
ratio1

#5.1计算决定系数 
R2cox<-1-exp((step1$deviance-step1$null.deviance)/length(iadl.t)) 
R2cox
#5.2计算Cox-Snell拟合优度 
cat("Cox-Snell R2=",R2cox,"\n") 
R2nag<-R2cox/(1-exp((-step1$null.deviance)/length(iadl.t))) 
cat("Nagelkerke R2=",R2nag,"\n") 
#5.4残差分析 
plot(residuals(step1)) 
#5.5异常值诊断 
library(car) 
influencePlot(step1) 
#边际效应
install.packages("mfx")
library(mfx)
logitmfx(formula = iadl.t~no+ra+bound,atmean=F,data = ndata)
#Odds Ratio
logitor(formula = iadl.t~no+ra+bound,data = ndata)


#adl
#生育个数单变量
adl.m=glm(formula = adl.t~no, family = binomial, data = ndata)
summary(adl.m)
adl1.m=glm(formula = adl.t~no+ra+bound, family = binomial, data = ndata)
summary(adl1.m)
#逐步回归
step2=step(adl1.m,direction=c("both"))
summary(step2)
vif(adl1.m)

coef(step1)#用以解释模型 
exp(step1$coefficients) #解释Odds比与x的关系 
exp(confint(step1)) #回归系数的置信区间 
xp1<-0/glm.safe1$coefficients[] 
xp1 #求使得pi为0.5的x 
ratio1<-step1$coefficients[]*0.25 
ratio1

#5.1计算决定系数 
R2cox1<-1-exp((step2$deviance-step1$null.deviance)/length(iadl.t)) 
R2cox1
#5.2计算Cox-Snell拟合优度 
cat("Cox-Snell R2=",R2cox,"\n") 
R2nag<-R2cox/(1-exp((-step1$null.deviance)/length(iadl.t))) 
cat("Nagelkerke R2=",R2nag,"\n") 
#5.4残差分析 
plot(residuals(step1)) 
#5.5异常值诊断 
library(car) 
influencePlot(step1) 


#广义有序logit回归
install.packages("questionr")
library(questionr)
install.packages("VGAM")
library(VGAM)
library(nnet)
#生理健康健康综合
phy.m=vglm(formula = phy~no,family=multinomial, data=ndata)
phy.m=vglm(formula = phy~ra,family=multinomial, data=ndata)
phy.m=vglm(formula = phy~bound,family=multinomial, data=ndata)
phy.m=vglm(formula = phy~no+ra+bound,family=multinomial, data=ndata)
summary(phy.m)


#心理健康
#焦虑
anx.m=vglm(formula = anx.t~1,family=multinomial, data=ndata)
anx.m=vglm(formula = anx.t~no,family=multinomial, data=ndata)
anx.m=vglm(formula = anx.t~no+ra+bound,family=multinomial, data=ndata)
summary(anx.m)

#抑郁
dep.m=vglm(formula = dep.t~no,family=multinomial, data=ndata)
dep.m=vglm(formula = dep.t~no+ra+bound,family=multinomial, data=ndata)
summary(dep.m)

#心理健康综合
apsy.m=vglm(formula = apsy~no,family=multinomial, data=ndata)
apsy.m=vglm(formula = apsy~ra,family=multinomial, data=ndata)
apsy.m=vglm(formula = apsy~bound,family=multinomial, data=ndata)
apsy.m=vglm(formula = apsy~no+ra+bound,family=multinomial, data=ndata)
summary(apsy.m)

step3=step(apsy.m,direction=c(""))
summary(step3)

coef(step3)#用以解释模型 
exp(step3$coefficients) #解释Odds比与x的关系 
exp(confint(step3)) #回归系数的置信区间 
xp3<-0/glm.safe1$coefficients[] 
xp3 #求使得pi为0.5的x 
ratio3<-step3$coefficients[]*0.25 
ratio3

#社会健康
soc.m=vglm(formula = soc.t~no,family=multinomial, data=ndata)
soc.m=vglm(formula = soc.t~ra,family=multinomial, data=ndata)
soc.m=vglm(formula = soc.t~bound,family=multinomial, data=ndata)
soc.m=vglm(formula = soc.t~no+ra+bound,family=multinomial, data=ndata)
summary(soc.m)

#自评健康
self.m=vglm(formula = self.t~no+ra+bound,family=multinomial, data=ndata)
self.m=vglm(formula = self.t~no,family=multinomial, data=sdata)
summary(self.m)
self.m=vglm(formula = self.t~ra,family=multinomial, data=sdata)
summary(self.m)
self.m=vglm(formula = self.t~bound,family=multinomial, data=sdata)
summary(self.m)


#选取变量
elderly=oldcitybg[,c('hukou','residenc','trueage','b12','b31','b32','b333','b34','b35','b36','b37',
                      'b38','b39','b310a','b41','b42','b43','b44','b45','b46','b47','e1','e2','e3',
                      'e4','e5','e6','e7','e8','e9','e10','e14','d11b2','d11b3','d112c','d112d','d11c',
                      'd11d','d11e','d11f','d11g','d11h','f10','f10a','f101','f102')]


#elderly=oldcitybg[,grepl("hukou|residenc|trueage|b12|b31|b32|b333|b34|b35|b36|b37|b38|b39|b310a|b41|b42|b43|b44|b45|b46|b47|e1|e2|e3|e4|e5|e6|e7|e8|e9|e10|e14|d11b2|d11b3|d112c|d112d|d11c|d11d|d11e|d11fd11g|d11h|f10|f10a|f101|f102",colnames(oldcitybg))]

#数据导出
write.table(elderly,"FYP Cleaned Data.csv",row.names=FALSE,col.names=TRUE,sep=",")


