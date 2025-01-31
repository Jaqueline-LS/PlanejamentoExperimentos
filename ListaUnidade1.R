rm(list=ls(all=T))
library("ExpDes")
library("dplyr")
library("MASS")

material<-data.frame(y=c(110, 1, 880, 495, 7, 
                         157, 2, 1256, 7040, 5,
                         194, 4,5276, 5307, 29,
                         178, 18, 4355, 10050, 2), trat=rep(c(1,2,3,4,5),4))
# 
# modelo<-crd(material$trat, material$y, hvar = "bartlett")
# modelo$residuals




m.hat<-mean(material$y) # media geral

I<-length(unique(material$trat)) #numero de tratamentos
J<-4 # numero de repeticoes de cada trat, balanceado

t<-numeric(I) # vetor para guardar os efeitos de trat, (t_i)

for(i in 1:I)
{
  t[i]<-mean(material$y[material$trat==i]) - m.hat
}

t # efeito dos tratamentos


# Criando um vetor com repeticoes do t_i para facilitar as contas
t.i<-rep(c(t[1],t[2],t[3],t[4],t[5]),4) # o efeito do trat_i repetido 
residuo<- material$y - m.hat - t.i


# Calculo da variância dos erros - Duas formas de calcular

# Primeira forma:
## Se quiser usar o teste do F máximo precisa das variancias dos trat.
s.i<-numeric(I)
for(i in 1:I)
{
  s.i[i]<- sum(((residuo[material$trat==i])^2 ))/(J-1)
}

s.i # variância de cada tratamento

s.2<-mean(s.i)

# Segunda forma:
s.2<-sum(residuo^2)/(I*(J-1)) # variância média

#------desvio
s<-sqrt(s.2)
sd(residuo)
# Residuo padronizado
residuo.padronizado<-(residuo-0)/s


CV_bruto<-(s/m.hat)*100

#-------------Normalidade-----------
qqnorm(residuo.padronizado)
qqline(residuo.padronizado)

shapiro.test(residuo.padronizado)
hist(residuo.padronizado)


# ---------- Homocedasticidade ou homogeneidade de variâncias
# F maximo
H<-max(s.i)/min(s.i)

pf(H,J-1,J-1, lower.tail = F)

# Bartlett
bartlett.test(material$y ~ material$trat)

# Grafico do residuo ~ valor ajustado
# Verificar indícios de heterogeneidade das variâncias e não independência dos erros
y.hat<-m.hat+t.i
plot(residuo.padronizado~y.hat)
# Padrão de funil =========> Fazer uma transformação
y<-material$y
boxcox(y~material$trat)

#------------------------log------------------------
y.tr<-log(material$y)
m.hat<-mean(y.tr)
t<-numeric(I)
for(i in 1:I)
{
  t[i]<-mean(y.tr[material$trat==i]) - m.hat
}
t.it<-rep(c(t[1],t[2],t[3],t[4],t[5]),4)
residuo<-y.tr-m.hat - t.it

s.2<-sum(residuo^2)/(I*(J-1)) # variância média
s<-sqrt(s.2)

# Novos resíduos
residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado)
qqline(residuo.padronizado)

shapiro.test(residuo.padronizado)
hist(residuo.padronizado)

# ---------- Homocedasticidade ou homogeneidade de variâncias

y.hat<-m.hat+t.i

plot(residuo.padronizado~y.hat)

bartlett.test(y.tr ~ material$trat)

s.i<-numeric(I)
for(i in 1:I)
{
  s.i[i]<- sum(((residuo[material$trat==i])^2 ))/(J-1)
}

s.i # variância de cada tratamento

# F maximo
H<-max(s.i)/min(s.i)
pf(H,J-1,J-1, lower.tail = F)



#--------------ANOVA------------

J<-4
I<-5
correcao<- (sum(y.tr)^2)/(I*J)
tot.trat<-numeric(I)
for(i in 1:I)
{
  tot.trat[i]<-sum(y.tr[material$trat==i])
}
SQTrat<-sum(tot.trat^2)/J - correcao

SQTotal<-sum(y.tr^2) - correcao

SQRes<- SQTotal - SQTrat

tabela<-cbind(CV=c("Trat","Resi","Tot"),df=c(I-1,I*(J-1),(I*J)-1) ,SQ=c(SQTrat, SQRes,SQTotal), QM=c(SQTrat/(I-1),SQRes/I*(J-1),"-"))
tabela
estatis.F<-(SQTrat/(I-1))/(SQRes/(I*(J-1)))

pf(estatis.F,(I-1), I*(J-1), lower.tail = F)
estatis.F
qf(0.05,(I-1), I*(J-1), lower.tail = F)


# crd(material$trat, log(material$y)) # Apenas para comparar os resultados da ANOVA

#--------------------------------------------------------------
Aleatoriz<-sample(rep(c("Catuai","MN","Icatu","Obatâ","CR"),c(9,9,9,9,9)),45, replace = F)


# Exercicio 7

catalizadores<-data.frame(y=c(58.2,57.2,58.4,55.8,54.9,
                              56.3,54.5,57.0,55.3,
                              50.1,54.2,55.4,
                              52.9,49.9,50.0,51.7),  trat=rep(c(1,2,3,4),c(5,4,3,4)))library("ExpDes")
material<-data.frame(y=c(110, 1, 880, 495,
  7, 194, 157, 2, 
  1256, 4, 5276, 7040,
  5, 5307, 29, 178, 
  18, 4355, 10050, 2), trat=rep(c(1,2,3,4,5),c(4,4,4,4,4)))
modelo<-crd(material$trat, material$y)

m.hat<-mean(material$y)

mean(material$y[material$trat==1])
mean(material$y[material$trat==2])
mean(material$y[material$trat==3])
mean(material$y[material$trat==4])
mean(material$y[material$trat==5])
library("ExpDes")
library("MASS")

material<-data.frame(y=c(110, 1, 880, 495,
  7, 194, 157, 2, 
  1256, 4, 5276, 7040,
  5, 5307, 29, 178, 
  18, 4355, 10050, 2), trat=rep(c(1,2,3,4,5),c(4,4,4,4,4)))
modelo<-crd(material$trat, material$y)
modelo$residuals
m.hat<-mean(material$y)
I<-length(unique(material$trat))
J<-4
t<-numeric(I)
for(i in 1:I)
{
  t[i]<-mean(material$y[material$trat==i]) - m.hat
}
t.i<-rep(c(t[1],t[2],t[3],t[4],t[5]),c(4,4,4,4,4))
residuo<-material$y-m.hat - t.i

s.2<-sum(residuo^2)/(I*(J-1)) # variância média
s<-sqrt(s.2)
residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado)
qqline(residuo.padronizado)

shapiro.test(residuo.padronizado)
hist(residuo.padronizado)

y.hat<-m.hat+t.i

plot(residuo.padronizado~y.hat)

bartlett.test(residuo.padronizado ~ material$trat)

# F máximo


# Padrão de funil
y<-material$y
boxcox(y~material$trat)
#------------------------log------------------------
y.tr<-log(material$y)
m.hat<-mean(y.tr)
t<-numeric(I)
for(i in 1:I)
{
  t[i]<-mean(y.tr[material$trat==i]) - m.hat
}
t.it<-rep(c(t[1],t[2],t[3],t[4],t[5]),c(4,4,4,4,4))
residuo<-y.tr-m.hat - t.it
material

s.2<-sum(residuo^2)/(I*(J-1)) # variância média
s<-sqrt(s.2)
residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado)
qqline(residuo.padronizado)

shapiro.test(residuo.padronizado)
hist(residuo.padronizado)

y.hat<-m.hat+t.i

plot(residuo.padronizado~y.hat)

bartlett.test(residuo.padronizado ~ material$trat)

#--------------ANOVA------------
J<-4
I<-5
correcao<- (sum(y.tr)^2)/(I*J)
tot.trat<-numeric(I)
for(i in 1:I)
{
  tot.trat[i]<-sum(y.tr[material$trat==i])
}
SQTrat<-sum(tot.trat^2)/J - correcao

SQTotal<-sum(y.tr^2) - correcao

SQRes<- SQTotal - SQTrat

tabela<-cbind(CV=c("Trat","Resi","Tot"),df=c(I-1,I*(J-1),(I*J)-1) ,SQ=c(SQTrat,SQTotal, SQRes), QM=c(SQTrat/(I-1),SQRes/(I*(J-1)),"-"))
tabela
estatis.F<-(SQTrat/(I-1))/(SQRes/(I*(J-1)))
estatis.F
pf(estatis.F,(I-1), I*(J-1), lower.tail = F)



# Coeficiente de variação




Aleatoriz<-sample(rep(1:5,c(9,9,9,9,9)),45, replace = F)


# Exercicio 7

catalizadores<-data.frame(y=c(58.2,57.2,58.4,55.8,54.9,
                 56.3,54.5,57.0,55.3,
                 50.1,54.2,55.4,
                 52.9,49.9,50.0,51.7),  trat=rep(c(1,2,3,4),c(5,4,3,4)))

