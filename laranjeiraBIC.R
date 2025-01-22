library("ExpDes")
data(ex2)
attach(ex2)
library("dplyr")


laranjeira<-matrix(c(145,155,166,
                     200,190,190,
                     183,186,208,
                     190,175,186,
                     180,160,156,
                     130,160,130,
                     206,165,170,
                     250,271,230,
                     164,190,193), ncol=3, nrow=9, byrow=T)
T.i<-apply(laranjeira,1,sum)

p<-c(145,155,166,
        200,190,190,
        183,186,208,
        190,175,186,
        180,160,156,
        130,160,130,
        206,165,170,
        250,271,230,
        164,190,193)
b<-rep(1:3,9)

t<-rep(1:9,rep(3,9))

dados<-data.frame(prod=p, bloco=b, trat=t)
sem.parcela.perdida<-rbd(treat=dados$prod, block=dados$bloco, resp=dados$prod)


# parcela perdida = 192.5625 calculo nas anotações


library(readr)
bloco <- read_table("bloco.txt") 
rbd(bloco$trat, bloco$bloco, bloco$resp)

rbd(bloco$trat, bloco$bloco, bloco$resp_1)# uma parcela perdida

rbd(bloco$trat, bloco$bloco, bloco$resp_2)# duas parcelas perdida
