library("dplyr")
X<-39*matrix(c(1.230769,2/39,2/39,
            2/39,1.230769,-0.1025641,
            2/39,-0.1025641,1.230769), ncol=3, byrow = T)
y<-39*c(46.20256,32.52564,91.79231)


z<-solve(X)%*%y

dados<-data.frame(resp=c(34.45,31.85,42.45,
                         90.55,75.82,62.85,
                         88.45,74.20,70.90,
                         64.60,92.60,35.25,
                         60.20,48.90,40.15,
                         64.50,59.35,59.70,
                         33.07,19.15,25.85,
                         49.45,33.75,29.85,
                         75.75,80.55,74.90,
                         33.50,28.85,31.55,
                         40.15,31.37,24.35,
                         11.05,9.95,6.75,
                         46.75,6.85,12.30),
                  resp2=c(34.45,31.85,42.45,
                         90.55,NA,62.85,
                         88.45,74.20,70.90,
                         64.60,92.60,35.25,
                         60.20,48.90,40.15,
                         64.50,59.35,59.70,
                         NA,19.15,25.85,
                         49.45,33.75,29.85,
                         75.75,80.55,74.90,
                         33.50,28.85,31.55,
                         40.15,NA,24.35,
                         11.05,9.95,6.75,
                         46.75,6.85,12.30),
                  trat=c(rep(1:13,rep(3,13))),
                  bloco=c(rep(1:3,13)))
I<-13
J<-3
Correcao<-(sum(dados$resp)^2)/(I*J)
SQTOTAL<-sum(dados$resp^2) - Correcao

Ti<- dados |>
  group_by(trat) |>
  summarise(sum(resp))

mi<-Ti$`sum(resp)`/J
sort(mi,decreasing = T)

Bi<- dados |>
  group_by(bloco) |>
  summarise(sum(resp))


SQTRAT<- sum(Ti^2)/J - Correcao

SQBLOCO<- sum(Bi^2)/I - Correcao

SQRES<-SQTOTAL-SQTRAT-SQBLOCO

m<-mean(dados$resp)
ti<-rep(Ti$`sum(resp)`/J - m, rep(3,13))
bi<-rep(Bi$`sum(resp)`/I - m, 13)

ajustado<-m + ti +bi


RESIDUOS<-dados$resp - ajustado

library("ExpDes")
obj<-rbd(treat = dados$trat,block=dados$bloco, resp = dados$resp)
obj$residuals

s<-sqrt(sum(RESIDUOS^2)/22)

s/m *100

pf(14.65904,df1=12,df2=22, lower.tail = F)



library("ExpDes")
obj<-rbd(treat = dados$trat,block=dados$bloco, resp = dados$resp2)
obj$residuals


# modelo reduzido

Correcao<-(sum(dados$resp2, na.rm = T)^2)/(36)
SQTOTAL<-sum(dados$resp2^2, na.rm = T) - Correcao

Bi.2<- dados |>
  group_by(bloco) |>
  summarise(sum(resp2, na.rm = T))

SQBLOCO <- sum(Bi.2$`sum(resp2, na.rm = T)`^2 /c(12,11,13)) - Correcao
SQRES1<-SQTOTAL-SQBLOCO
SQTRAT.aj<-SQRES1 - SQRES

pf(12.85574,df1=12, df2=21, lower.tail = F)



