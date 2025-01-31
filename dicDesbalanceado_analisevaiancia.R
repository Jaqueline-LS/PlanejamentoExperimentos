rm(list = ls(all = TRUE))
library("ExpDes") #utiliza a restrição sum(ti)=0
library(readr)

golfe<-data.frame(resp=c(83,85,85,87,90,88,88,84,91,90,91,87,84,87,85,86,83,94,91,87,85,87,91,92,86),trat=c(rep("V",10),rep("E",7), rep("I",8)))
golfe

attach(golfe)
mod1=crd(trat,resp) #interamente causalizado
library("MASS")
erro<-residuals(mod1)
hist(erro)
qqnorm(erro)
qqline(erro)
# Normalidade OK!

#F máximo não pe indicado para desbalancedo


# Podemos usar o Barttlet para homegeneidade
bartlett.test(resp~trat)
# Não rejeitou a hipótese de homogeneidade


# verificando a independencia ( aqui n é os residuos padronizados então não conseguimos ver outliers)
ajustados=fitted(mod1)
plot(ajustados,erro, pch=19, cex=0.5)


#--------------------------------------------
# Residuos na unha

y.ij<-matrix(NA, byrow=T,ncol=10, nrow=3)
I<-dim(y.ij)[1]
n<-c(10,7,8)

y.ij[1,1:10]<-c(83,85,85,87,90,88,88,84,91,90,91,87,84,87,85,86,83,94,91,87,85,87,91,92,86)[1:10]
y.ij[2,1:7]<-c(91,87,84,87,85,86,83)
y.ij[3,1:8]<-c(94,91,87,85,87,91,92,86)

m.hat <- sum(y.ij, na.rm = TRUE)/(sum(n))

y.ij.squared<- y.ij^2

total.trat<-apply(y.ij,1,sum, na.rm=T)

total.trat.squared<-apply(y.ij.squared,1,sum, na.rm=T)


ti.hat<-apply(y.ij, 1,FUN=function(x)mean(x, na.rm=T)-m.hat)

residuo<-y.ij-m.hat-matrix(rep(ti.hat,10),byrow = F, ncol=10, nrow = I )
residuo<-as.vector(residuo)
indice<-is.na(residuo)
residuo<-residuo[!indice]


qqnorm(residuo)
qqline(residuo)
hist(residuo)



# Anova
N<-nrow(golfe)
I<-3
nome<-c("V","E","I")
correcao<-(sum(golfe$resp)^2)/N # É O QUADRADO DA SOMAAA

T.i<-numeric(I)
n.i<-c(10,7,8)

for (i in 1:I)
{
  T.i[i]<-sum(golfe$resp[golfe$trat==nome[i]])
}

T.i.2.ni<- (T.i^2) / n.i

SQTrat<-sum(T.i.2.ni)-correcao

SQTotal<- sum(golfe$resp^2)-correcao


SQRes<- SQTotal - SQTrat


QMTr<-SQTrat/(I-1)

QMR<- SQRes/12

est.F<-QMTr/QMR

pf(est.F, 3,12, lower.tail = F)

m.hat<-mean(dados$y)
cv<-s/m.hat*100
cv


pf(2.1221, 2,22, lower.tail = F)
pf(3.7944, 1,22, lower.tail = F)
pf(0.4493, 3,22, lower.tail = F)


