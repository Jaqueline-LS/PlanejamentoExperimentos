dados<-data.frame(y=c(575,542,530,539,570,
                      565,593,590,579,610,
                      600,651,610,637,629,
                      725,700,715,685,710), 
                  x=rep(c(160,180,200,220),c(5,5,5,5)))

modeloL1<-lm(dados$y~dados$x)
modeloL2<-lm(dados$y ~ 1 + dados$x+ I(dados$x^2))

