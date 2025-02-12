library(readr)
library("dplyr")
Fatorial <- read_table("~/Documentos/Planejamento/Fatorial.csv")

TI<-Fatorial |>
  group_by(operador, aparelho) |>
  summarise(sum(obs))

Totais.Trat<-TI$`sum(obs)`

Totais.Bloco<- Fatorial |>
  group_by(bloco) |>
  summarise(sum(obs))

Totais.Bloco<-Totais.Bloco$`sum(obs)`

Totais.op<-Fatorial |>
  group_by(operador) |>
  summarise(sum(obs))
Totais.op<-Totais.op$`sum(obs)`


Totais.ap<-Fatorial |>
  group_by(aparelho) |>
  summarise(sum(obs))
Totais.ap<-Totais.ap$`sum(obs)`


CORRECAO<-(sum(Fatorial$obs)^2) / 200 

SQTOTAL<-sum(Fatorial$obs^2) - CORRECAO

SQTRAT<- sum(Totais.Trat^2)/10  - CORRECAO

SQBLOCO<-sum(Totais.Bloco^2)/20  - CORRECAO

SQRES<-SQTOTAL-SQTRAT-SQBLOCO

SQOP<-sum(Totais.op^2)/50  - CORRECAO

SQAP<-sum(Totais.ap^2)/40  - CORRECAO

SQOPvsAP<-SQTRAT-SQOP-SQAP


QMAp<-SQAP/4

QMOPvsAP<-SQOPvsAP/12

QMRES<-SQRES/171


estatistica.f.Interacao<-QMOPvsAP/QMRES

pf(estatistica.f.Interacao,df1=12, df2=171, lower.tail = F) # Primeiro testa interação
# Como a interação foi significativa não interpreta as significãncias dos tratamentos separados
#

