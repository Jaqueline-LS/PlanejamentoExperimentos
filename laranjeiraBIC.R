library("ExpDes")
data(ex2)
attach(ex2)
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
b<-c(rep(1,9), rep(2,9), rep(3,9))
t<-rep(1:9,rep(3,9))

dados<-data.frame(prod=p, bloco=b, trat=t)
sem.parcela.perdida<-rbd(treat=dados$prod, block=dados$bloco, resp=dados$prod)
