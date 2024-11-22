library("ExpDes") #utiliza a restrição sum(ti)=0
library(readr)
estacas <- read_table("estacas.txt")
attach(estacas)
mod1=crd(Cultivar,Estacas) #interamente causalizado
library("MASS")
erro<-residuals(mod1)
hist(erro)
qqnorm(erro)
qqline(erro)
ajustados=fitted(mod1)
plot(ajustados,erro)

# Para transformações pode usar as funcoes/pacote
boxcox((Estacas+0.05)~Cultivar) # eliminar o zero
# Dados de contagem geralmente é raiz quadrada    
# Lição de casa fazer a transformação e anilisar os resíduos


y.ij<-matrix(Estacas, byrow=T,ncol=5, nrow=4)

m.hat<- sum(y_ij)/prod(dim(y.ij))

ti.hat<-apply(y.ij, 1,sum)


