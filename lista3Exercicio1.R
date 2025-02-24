tempo_estoque<-c(3.45,3.36,
                 4.14,4.19,
                 5.8,5.23,
                 4.07,3.52,
                 4.38,4.26,
                 5.48,4.85,
                 4.2,3.68,
                 4.26,4.37,
                 5.67,5.58,
                 4.8,4.4,
                 5.22,4.7,
                 6.21,5.88,
                 4.52,4.44,
                 5.15,4.65,
                 6.25,6.2,
                 4.96,4.39,
                 5.17,4.75,
                 6.03,6.38,
                 4.08,3.65,
                 3.94,4.08,
                 5.14,4.49,
                 4.3,4.04,
                 4.53,4.08,
                 4.99,4.59,
                 4.17,3.88,
                 4.86,4.48,
                 4.85,4.9)

dados<-matrix(tempo_estoque, ncol=2, byrow = T)
T.i<-apply(dados, 1, sum)

correcao<-(sum(tempo_estoque)^2)/(length(tempo_estoque))
SQTOTAL<- sum(tempo_estoque^2) - correcao
SQTRAT<- sum(T.i^2)/2 - correcao # não tem confundimento
SQRES<-SQTOTAL-SQTRAT


library("dplyr")
library("tidyr")
dados2<-data.frame(tempo_estoque,
                   trabalhador=rep(1:3,rep(18,3)),
                   garrafa=rep(rep(c("p","v28","v38"),rep(6,3))),
                   armazenamento=rep(rep(c(1,2,3),rep(2,3)),9))

dados2 |>
  group_by(garrafa,trabalhador) |>
  summarise(sum(tempo_estoque)) |>
  pivot_wider(names_from = trabalhador, values_from = `sum(tempo_estoque)`)

GT<- dados2 |>
  group_by(garrafa,trabalhador) |>
  summarise(sum(tempo_estoque))
GT<-GT$`sum(tempo_estoque)`

dados2 |>
  group_by(armazenamento,trabalhador) |>
  summarise(sum(tempo_estoque)) |>
  pivot_wider(names_from = trabalhador, values_from = `sum(tempo_estoque)`)

AT<- dados2 |>
  group_by(armazenamento,trabalhador) |>
  summarise(sum(tempo_estoque))
AT<-AT$`sum(tempo_estoque)`

dados2 |>
  group_by(armazenamento,garrafa) |>
  summarise(sum(tempo_estoque)) |>
  pivot_wider(names_from = garrafa, values_from = `sum(tempo_estoque)`) 

AG<- dados2 |>
  group_by(armazenamento,garrafa) |>
  summarise(sum(tempo_estoque))
AG<-AG$`sum(tempo_estoque)`

TG<- dados2 |>
  group_by(garrafa)|>
  summarise(sum(tempo_estoque))

TG<-TG$`sum(tempo_estoque)`

TA<- dados2 |>
  group_by(armazenamento)|>
  summarise(sum(tempo_estoque))

TA<-TA$`sum(tempo_estoque)`

TT<-dados2 |>
  group_by(trabalhador)|>
  summarise(sum(tempo_estoque))

TT<-TT$`sum(tempo_estoque)`


SQG<-sum(TG^2)/18 - correcao
SQA<-sum(TA^2)/18 - correcao
SQT<-sum(TT^2)/18 - correcao

# Interações

SQGT<- sum(GT^2)/6 - SQG -SQT -correcao
SQAG<- sum(AG^2)/6 - SQG -SQA -correcao
SQAT<- sum(AT^2)/6 - SQT -SQA -correcao
SQGTA<- SQTRAT-SQGT-SQAG-SQAT-SQG-SQA-SQT

cores<-c("#BAF3DE","#C9E69E","#FFC29A","#FF9B95")

armaz.trab<-dados2 |>
  group_by(armazenamento,trabalhador) |>
  summarise(mean(tempo_estoque)) 

with(data=armaz.trab, plot(`mean(tempo_estoque)`~trabalhador,col=cores[armaz.trab$armazenamento], lwd=3,pch=19))
with(data=armaz.trab, lines(c(1:3),c(3.71,4.58,4.02), col=cores[1]))
with(data=armaz.trab, lines(c(1:3),c(4.27,4.94,4.33), col=cores[2]))
with(data=armaz.trab, lines(c(1:3),c(5.44,6.16,4.83), col=cores[3]))
legend("topright",legend=paste0("A",1:3),lwd=2,col=cores[1:3])

with(data=armaz.trab, plot(`mean(tempo_estoque)`~armazenamento,col=cores[armaz.trab$trabalhador], lwd=3,pch=19))
legend("topleft",legend=paste0("Trab",1:3),lwd=2,col=cores[1:3])


Ad.T1<-dados2 |>
  group_by(armazenamento,trabalhador) |>
  summarise(sum(tempo_estoque)) |>
  filter(trabalhador==1)
Ad.T1<-Ad.T1$`sum(tempo_estoque)`



