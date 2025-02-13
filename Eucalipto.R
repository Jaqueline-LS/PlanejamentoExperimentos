library(readr)
library("dplyr")
Fatorial <- read_table("Fatorial.csv")

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

# Desdobramento de observador dentro de aparelho
tabela.aux<-matrix(Totais.Trat, ncol=5, nrow=4, byrow=T)
totais.ap<-apply(tabela.aux, 2,sum)  
totais.op<-apply(tabela.aux, 1,sum)  

SQOP_ap<-numeric(5)
for(i in 1:5)
{
  SQOP_ap[i]<- sum(tabela.aux[,i]^2)/10 - (totais.ap[i]^2)/40
}

QMOP_ap<-numeric(5)
for(i in 1:5)
{
  QMOP_ap[i]<-SQOP_ap[i]/3
}

esta.f.OP_ap<-numeric(5)

for(i in 1:5)
{
  esta.f.OP_ap[i]<-QMOP_ap[i]/QMRES
}

pf(esta.f.OP_ap, df1=3, df2=171, lower.tail = F)<0.05


SQAP_op<-numeric(4)
for(i in 1:4)
{
  SQAP_op[i]<- sum(tabela.aux[i,]^2)/10 - (totais.op[i]^2)/50
}

QMAP_op<-numeric(4)
for(i in 1:4)
{
  QMAP_op[i]<-SQAP_op[i]/4
}

esta.f.AP_op<-numeric(4)

for(i in 1:4)
{
  esta.f.AP_op[i]<-QMAP_op[i]/QMRES
}

pf(esta.f.AP_op, df1=4, df2=171, lower.tail = F)<0.05


