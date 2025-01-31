library("dplyr")
#Quadrados latinos
foguete<-data.frame(resp=c(24,20,19,24,24,
                        17,24,30,27,36,
                        18,38,26,27,21,
                        26,31,26,23,22,
                        22,30,20,29,31),
                 trat=c("a","b","c","d","e",
                        "b","c","d","e","a",
                        "c","d","e","a","b",
                        "d","e","a","b","c",
                        "e","a","b","c","d"),
                 coluna=c(rep(paste0("c",1:5),5)),
                 linha=c(rep(paste0("l",1:5),rep(5,5))))


              
write.table(foguete, file="foguete.txt", row.names = F)
k<-5
i<-5
j<-5

total.geral<-sum(foguete$resp)
total.geral

m<-total.geral/25

# Totais tratamentos
T.k<-foguete |>
  group_by(trat) |>
  summarise(sum(resp))

tk<-foguete |>
  left_join(T.k) |>
  select(`sum(resp)`)
  
T.k<-T.k$`sum(resp)`


medias.trat<-T.k/5

# Totais linhas

T.i<-foguete |>
  group_by(linha) |>
  summarise(sum(resp))

T.i<-T.i$`sum(resp)`



# Totais colunas

T.j<-foguete |>
  group_by(coluna) |>
  summarise(sum(resp))

T.j<-T.j$`sum(resp)`



correcao<-(total.geral^2)/25

SQTOTAL<-sum(foguete$resp^2) - correcao

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

tk<- as.numeric(tk$`sum(resp)`/5) - m
li<-T.i/5 - m
cj<-T.j/5 - m


ajustado<- m + rep(li,c(5,5,5,5,5)) + rep(cj,5)+ tk

residuos<-foguete$resp-ajustado

hist(residuos)

shapiro.test(residuos)

s<-sqrt(sum(residuos^2)/(12))

cv<-s/m*100

# No R
library("ExpDes")
obj<-latsd(foguete$trat,foguete$linha, foguete$coluna, foguete$resp)
sqrt(sum(obj$residuals^2)/(12)) / m
obj$means
foguete$resp-obj$fitted.values
