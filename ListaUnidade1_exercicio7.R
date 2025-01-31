rm(list=ls(all=T))
library("ExpDes")
library("dplyr")
library("MASS")
dados<-data.frame(y=c(58.2,57.2,58.4,55.8,54.9,
                      56.3,54.5,57.0,55.3,
                      50.1,54.2,55.4,
                      52.9,49.9,50.0,51.7), trat=rep(c(1,2,3,4),c(5,4,3,4)))
modelo<-crd(dados$trat, dados$y)
I<-4
m.i<-numeric(I)
for(i in 1:4)
{
  m.i[i]<-mean(dados$y[dados$trat==i])
}
m.i

m.i<-rep(m.i, c(5,4,3,4))

n.i<-c(5,4,3,4)
N<-sum(n.i)

residuo<-dados$y-m.i

s2<-sum(residuo^2)/(N-1)
s<-sqrt(s2)

residuo.pad<-residuo/s

qqnorm(residuo.pad)
qqline(residuo.pad)

shapiro.test(residuo.pad)

#Homocedasticidade

bartlett.test(x=dados$y,g=dados$trat)
plot(residuo.pad~m.i)

# Anova
correcao<-(sum(dados$y)^2)/N # É O QUADRADO DA SOMAAA

T.i<-numeric(I)

for (i in 1:I)
{
  T.i[i]<-sum(dados$y[dados$trat==i])
}

T.i.2.ni<- (T.i^2) / n.i

SQTrat<-sum(T.i.sqrt.ni)-correcao

SQTotal<- sum(dados$y^2)-correcao


SQRes<- SQTotal - SQTrat


QMTr<-SQTrat/(I-1)

QMR<- SQRes/12

est.F<-QMTr/QMR

pf(est.F, 3,12, lower.tail = F)

m.hat<-mean(dados$y)
cv<-s/m.hat*100
cv
