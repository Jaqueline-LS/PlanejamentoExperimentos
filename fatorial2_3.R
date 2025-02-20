trat<-c(rep(0,6*3), rep(c(0,0,1),6), rep(c(0,1,0),6),
       rep(c(0,1,1),6),rep(c(1,0,0),6 ),rep(c(1,0,1),6),
       rep(c(1,1,0),6), rep(c(1,1,1),6))

NPK<-matrix(trat,byrow=TRUE, ncol=3)

DadosNPK<-data.frame( Trat=c(rep(1:8,rep(6,8))) ,
                      Blocos=c(rep(1:6,8)) ,
                      Resp=c(31.8,40.5,25.7,25.7,37.2,45.3,
                             25.6,32.4,39.6,48.9,20.6,33.7,
                             36.2,37.8,40.9,44.8,32.4,38.4,
                             37.1,53,36.4,43,19.7,30.4,
                             35.3,39,36,33.5,28.2,42.4,
                             51.5,66.1,51.7,52,56.5,58.2,
                             43.8,32.7,43.3,41.8,31.9,37.7,
                             47,49.9,50.9,49.1,71.7,39.6) 
                      )
library("dplyr")
# Totais de Trat
Ti<- DadosNPK |>
  group_by(Trat)|>
  summarise(sum(Resp))
Ti<-Ti$`sum(Resp)`
# Totais de Bloco
Di<- DadosNPK |>
  group_by(Blocos)|>
  summarise(sum(Resp))
Di<-Di$`sum(Resp)`


correcao<- (sum(DadosNPK$Resp)^2) / (48)

SQTOTAL<- sum((DadosNPK$Resp)^2) - correcao

SQTRAT<- sum((Ti)^2)/6 - correcao

SQBLOCO<- sum((Di)^2)/8 - correcao

SQRES<-SQTOTAL-SQTRAT-SQBLOCO

TotaisN<-c(857.1,1089.8)
SQN<-sum((TotaisN)^2)/24 - correcao

TotaisK<-c(882.3,1064.6)
SQK<-sum((TotaisK)^2)/24 - correcao

TotaisP<-c(957.4,989.5)
SQP<-sum((TotaisP)^2)/24 - correcao


# Interações duplas<-

SQNP<-sum(c(407,450.1,550.4,539.4)^2)/12 - correcao - SQN -SQP
SQNK<-sum(c(436.7,420.4,445.6,644.2)^2)/12 - correcao - SQN -SQK
SQKP<-sum(c(420.6,461.7,536.8,527.8)^2)/12 - correcao - SQK -SQP

SQNKP<-SQTRAT - SQKP -SQNK -SQNP - SQP - SQN - SQK



# Nitrogenio dentro de potassio
NemK<-c(436.7,445.6)
SQNK0<- sum(NemK^2)/12 - (sum(NemK)^2)/24
NemK<-c(420.4,644.2)
SQNK1<- sum(NemK^2)/12 - (sum(NemK)^2)/24

