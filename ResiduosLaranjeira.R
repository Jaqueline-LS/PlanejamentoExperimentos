dados<-matrix(c(145, 155, 166,
         200,190,190,
         183,186,208,
         190,175,186,
         180,160,156,
         130,160,130,
         206,165,170,
         250,271,230,
         164,190,193), byrow=T, ncol=3,nrow=9)

ti<-c(-27.22,10.78,9.78,1.12,-17.22,-42.55,-2.22,67.78,-0.25)
m<-182.56
bj<-c(0.56,1,-1.56)

residuos<-sapply(1:3, FUN = function(x) dados[,x]-m-ti-bj[x])

hist(residuos, freq=F)

shapiro.test(residuos)

ajustado<-as.numeric(sapply(1:3, FUN = function(x) m+ti+bj[x]))
as.numeric(t(dados))-ajustado

plot(ajustado, residuos, pch=19)

trat=rep(1:9,3)
y=as.numeric(apply(dados, 2,FUN=function(x)x))

bartlett.test(y ~ trat)


I<-3
J<-9
Correcao<- (sum(dados)^2)/(I*J)
Ti<-apply(dados,1,FUN = function(x)sum(x))
SQT<- (sum(Ti^2)/(J)) - Correcao


library("dplyr")
library(readr)
laranjeira <- read_table("bloco.txt")

J<-3
I<-9

Correcao<-(sum(laranjeira$resp)^2)/(I*J)

Ti<-laranjeira |>
  group_by(trat) |>
  summarise(Ti=sum(resp))

SQTRAT<-sum(Ti$Ti^2)/J - Correcao

Bi<-laranjeira |>
  group_by(bloco) |>
  summarise(Bi=sum(resp))
SQBLOCO<-sum(Bi$Bi^2)/I - Correcao


SQTOTAL<- sum(laranjeira$resp^2) - Correcao

C<-matrix(c(1,1,1,1,-2,-2,1,-2,1,
                           0,0,0,0,1,1,0,-2,0,
                           0,0,0,0,1,-1,0,0,0,
                           1,-1,-1,1,0,0,1,0,-1,
                           1,0,0,1,0,0,-2,0,0,
                           1,0,0,-1,0,0,0,0,0,
                           0,1,1,0,0,0,0,0,-2,
                           0,1,-1,0,0,0,0,0,0), byrow = T, ncol=9, nrow = 8)

Y.hat<-C%*%Ti$Ti

SQY.i.hat<-sapply(c(1:8),FUN=function(x){(Y.hat[x]^2)/(sum(C[x,]^2)*J)})

SQRES<-SQTOTAL-SQBLOCO-SQTRAT

QMRES<-SQRES/16
QMTRAT<-SQTRAT/8
estatistica.f<-QMTRAT/(QMRES)
pf(estatistica.f,df1=8,df2=16,lower.tail = F)

pvalor<-function(x)
{
  estatistica.f<-x/(QMRES)
  p<-pf(estatistica.f,df1=1,df2=16,lower.tail = F)
  return(list(f=estatistica.f, p=p))
}
sapply(SQY.i.hat, FUN=pvalor)
