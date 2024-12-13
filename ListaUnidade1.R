library("ExpDes")
material<-data.frame(y=c(110, 1, 880, 495,
  7, 194, 157, 2, 
  1256, 4, 5276, 7040,
  5, 5307, 29, 178, 
  18, 4355, 10050, 2), trat=rep(c(1,2,3,4,5),c(4,4,4,4,4)))
modelo<-crd(material$trat, material$y)

m.hat<-mean(material$y)

mean(material$y[material$trat==1])
mean(material$y[material$trat==2])
mean(material$y[material$trat==3])
mean(material$y[material$trat==4])
mean(material$y[material$trat==5])
library("ExpDes")
library("MASS")

material<-data.frame(y=c(110, 1, 880, 495,
  7, 194, 157, 2, 
  1256, 4, 5276, 7040,
  5, 5307, 29, 178, 
  18, 4355, 10050, 2), trat=rep(c(1,2,3,4,5),c(4,4,4,4,4)))
modelo<-crd(material$trat, material$y)
modelo$residuals
m.hat<-mean(material$y)
I<-length(unique(material$trat))
J<-4
t<-numeric(I)
for(i in 1:I)
{
  t[i]<-mean(material$y[material$trat==i]) - m.hat
}
t.i<-rep(c(t[1],t[2],t[3],t[4],t[5]),c(4,4,4,4,4))
residuo<-material$y-m.hat - t.i

s.2<-sum(residuo^2)/(I*(J-1)) # variância média
s<-sqrt(s.2)
residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado)
qqline(residuo.padronizado)

shapiro.test(residuo.padronizado)
hist(residuo.padronizado)

y.hat<-m.hat+t.i

plot(residuo.padronizado~y.hat)

bartlett.test(residuo.padronizado ~ material$trat)

# F máximo


# Padrão de funil
y<-material$y
boxcox(y~material$trat)
#------------------------log------------------------
y.tr<-log(material$y)
m.hat<-mean(y.tr)
t<-numeric(I)
for(i in 1:I)
{
  t[i]<-mean(y.tr[material$trat==i]) - m.hat
}
t.it<-rep(c(t[1],t[2],t[3],t[4],t[5]),c(4,4,4,4,4))
residuo<-y.tr-m.hat - t.it

s.2<-sum(residuo^2)/(I*(J-1)) # variância média
s<-sqrt(s.2)
residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado)
qqline(residuo.padronizado)

shapiro.test(residuo.padronizado)
hist(residuo.padronizado)

y.hat<-m.hat+t.i

plot(residuo.padronizado~y.hat)

bartlett.test(residuo.padronizado ~ material$trat)

#--------------ANOVA------------
J<-4
I<-5
correcao<- (sum(y.tr)^2)/(I*J)
tot.trat<-numeric(I)
for(i in 1:I)
{
  tot.trat[i]<-sum(y.tr[material$trat==i])
}
SQTrat<-sum(tot.trat^2)/J - correcao

SQTotal<-sum(y.tr^2) - correcao

SQRes<- SQTotal - SQTrat

tabela<-cbind(CV=c("Trat","Resi","Tot"),df=c(I-1,I*(J-1),(I*J)-1) ,SQ=c(SQTrat,SQTotal, SQRes), QM=c(SQTrat/(I-1),SQRes/I*(J-1),"-"))
estatis.F<-(SQTrat/(I-1))/(SQRes/I*(J-1))

pf(estatis.F,(I-1), I*(J-1), lower.tail = F)



Aleatoriz<-sample(rep(1:5,c(6,6,6,6,6)),30, replace = F)
