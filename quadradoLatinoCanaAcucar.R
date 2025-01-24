library("dplyr")
#Quadrados latinos
cana<-data.frame(resp=c(432,518,458,583,331,
                   724,478,524,550,400,
                   489,384,556,297,420,
                   494,500,313,486,501,
                   515,660,438,394,318),
            trat=c("d","a","b","c","e",
                   "c","e","a","b","d",
                   "e","b","c","d","a",
                   "b","d","e","a","c",
                   "a","c","d","e","b"),
            coluna=c(rep(paste0("c",1:5),5)),
            linha=c(rep(paste0("l",1:5),rep(5,5))),
            resp.p=c(432,518,458,583,331,
                   724,478,524,550,NA,
                   489,384,556,297,420,
                   494,500,313,486,501,
                   515,660,438,394,318))
write.table(cana, file="cana.txt", row.names = F)
k<-5
i<-5
j<-5

total.geral<-sum(cana$resp)
total.geral

m<-total.geral/25

# Totais tratamentos
T.k<-cana |>
  group_by(trat) |>
  summarise(sum(resp))

T.k<-T.k$`sum(resp)`
sum(T.k)

T.k.squa<-T.k^2
T.k.squa

medias.trat<-T.k/5

# Totais linhas

T.i<-cana |>
  group_by(linha) |>
  summarise(sum(resp))

T.i<-T.i$`sum(resp)`

T.i.squa<-T.i^2
T.i.squa


# Totais colunas

T.j<-cana |>
  group_by(coluna) |>
  summarise(sum(resp))

T.j<-T.j$`sum(resp)`

T.j.squa<-T.j^2
T.j.squa

correcao<-(total.geral^2)/25

SQTOTAL<-sum(cana$resp^2) - correcao

SQTRAT<-sum(T.k.squa)/5 - correcao
  
SQLINHA<-sum(T.i.squa)/5 - correcao

SQCOLUNA<-sum(T.j.squa)/5 - correcao

SQRES<-SQTOTAL-SQTRAT-SQLINHA-SQCOLUNA


# Quadrado medio
QMTRAT<-SQTRAT/4

QMLINHA<-SQLINHA/4

QMCOLUNA<-SQCOLUNA/4

QMRES<-SQRES/12

# So tem interesse em testar o tratamento

estatistica.f<-QMTRAT/QMRES
valor.p<-pf(estatistica.f,4,12,lower.tail = F)

# Existe diferença entre pelo menos duas medias de trat

# Se trat quantitativos ===> POlinomial
# Se qualitativo ====> todos os testes ver qual é o mais adequado

# Esse exemplo é quali, variedade de cana
# Não pediu nada especifico, Tukey

medias.trat


# No R
library("ExpDes")
latsd(cana$trat,cana$linha, cana$coluna, cana$resp)

# No R - fingindo que perdeu uma parcela
library("ExpDes")
latsd(cana$trat,cana$linha, cana$coluna, cana$resp.p)
