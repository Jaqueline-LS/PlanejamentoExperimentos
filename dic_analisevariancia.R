rm(list = ls(all = TRUE))
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
I<-dim(y.ij)[1]
J<-dim(y.ij)[2]

m.hat<- sum(y.ij)/(I*J)

ti.hat<-apply(y.ij, 1,FUN=function(x)mean(x)-m.hat)
# Restrição soma de ti.hat é igual zero

residuo<-y.ij-m.hat-matrix(rep(ti.hat,J),byrow = F, ncol=J, nrow = I )

residuo.squared<-residuo^2

si<-apply(residuo.squared,1,FUN=function(x)sum(x)/I) #variância no trat i

sum(si)/I

s.2<-sum(residuo^2)/(I*(J-1)) # variância média

s<-sqrt(s.2)

residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado)
qqline(residuo.padronizado)


# Transformação

ajustados<-m.hat+matrix(rep(ti.hat,J),byrow = F, ncol=J, nrow = I)
plot(ajustados,residuo, pch=19, main="Gráfico dos resíduos pelos valores ajustados")

Estacas.residuo<-data.frame(residuo=as.vector(residuo),trat=rep(c("A","B","C","D"),each=5))
bartlett.test(residuo ~ trat, data = Estacas.residuo)


# Estabilizar a variância
# Para transformações pode usar as funcoes/pacote
boxcox((Estacas+0.05)~Cultivar) # eliminar o zero
# Dados de contagem geralmente é raiz quadrada    
# Lição de casa fazer a transformação e analisar os resíduos

Estacas.sqrt<-(Estacas+0.5)^(1/2)



y.ij<-matrix(Estacas.sqrt, byrow=T,ncol=5, nrow=4)
I<-dim(y.ij)[1]
J<-dim(y.ij)[2]

m.hat<- sum(y.ij)/(I*J)

ti.hat<-apply(y.ij, 1,FUN=function(x)mean(x)-m.hat)
# Restrição soma de ti.hat é igual zero

residuo<-y.ij-m.hat-matrix(rep(ti.hat,J),byrow = F, ncol=J, nrow = I )

residuo.squared<-residuo^2

si<-apply(residuo.squared,1,FUN=function(x)sum(x)/I) #variância no trat i

sum(si)/I

s.2<-sum(residuo^2)/(I*(J-1)) # variância média

s<-sqrt(s.2)

residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado, pch=19)
qqline(residuo.padronizado)

ajustados<-m.hat+matrix(rep(ti.hat,J),byrow = F, ncol=J, nrow = I )
plot(ajustados, residuo, pch=19)

hist(residuo.padronizado, freq=F, col="#dcedc1", xlim=c(-3,3))
curve(dnorm,-3,3, add=T)

shapiro.test(residuo)

# Teste do F máximo
si
# Pega a maxima divide pela minima e olha na tabelinha da F, nesse caso seria 4 variancias e 16 graus de liberdade(ou 4)

mod1=crd(Cultivar,Estacas)


