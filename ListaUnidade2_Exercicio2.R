dados<-data.frame(resp=c(4.65,5.18,5.52,
                    4.86,4.81,4.51,
                    4.54,4.39,5.20,
                    4.55,5.16,6,
                    4.73,5.51,5.09,
                    4.54,5.81,4.77,
                    3.89,4.74,5.43,
                    3.91,5.75,4.52,
                    5.61,5.40,5.10,
                    2.68,2.65,2.56,
                    2.90,2.71,2.93,
                    2.78,2.84,2.30,
                    3.48,2.75,3.06,
                    2.65,2.47,2.83,
                    2.50,2.60,2.66), trat=rep(1:15,rep(3,15)))
library("dplyr")

Ti<-dados |>
  group_by(trat) |>
  summarise(sum(resp)) 

Ti<-Ti$`sum(resp)`

C<-matrix(c(rep(1,12),rep(-4,3),
            rep(0,12),2,-1,-1,
            rep(0,13),1,-1,
            rep(1,9),rep(-3,3),rep(0,3),
            rep(0,9),-2,1,1,rep(0,3),
            rep(0,10),-1,1,rep(0,3),
            rep(1,6),rep(-2,3),rep(0,6),
            rep(0,6),-2,1,1,rep(0,6),
            rep(0,7),-1,1,rep(0,6),
            1,1,1,-1,-1,-1,rep(0,9),
            rep(0,3),-2,1,1,rep(0,9),
            rep(0,4),-1,1,rep(0,9),
            -2,1,1,rep(0,12),
            0,-1,1,rep(0,12)),ncol = 15, byrow = T)
apply(C,1,sum)

I<-15
J<-3

Y.hat<-C%*%Ti
total.geral<-sum(dados$resp)
correcao<-(total.geral^2) / (J*I)
SQTRAT<-sum(Ti^2)/J - correcao
SQY.i.hat<-round(sapply(c(1:14),FUN=function(x){(Y.hat[x]^2)/(sum(C[x,]^2)*J)}),2)
sum(SQY.i.hat)

SQTOTAL<-sum(dados$resp^2) - correcao

SQRES<-SQTOTAL-SQTRAT

estatisticas<-SQY.i.hat/(SQRES/30)
pf(estatisticas,df1 = 1,df2=30, lower.tail = F)


obj<-ExpDes::crd(dados$trat, dados$resp)
s<-sqrt(sum(obj$residuals^2)/30)
m.geral<-mean(dados$resp)

cv<-s/m.geral * 100
cv
