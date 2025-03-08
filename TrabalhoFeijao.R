library("dplyr")
library("MASS")
cores<-c("#C9E69E","#FFC29A","#FF9B95")

# Criando o DataFrame
feijao <- data.frame(
  bloco = rep(c("Carlos", "Gabriela", "Jaqueline", "Maria Fernanda", "Natalia"), each = 9),
  trat = rep(c("Preto", "Vermelho", "Branco", "Carioquinha", "Manteiga", "Corda", "Fradinho", "Azuki", "Moyashi"), times = 5),
  y = c(
    0.60, 0.70, 2.00, 2.00, 10.50, 0.90, 1.10, 0.00, 9.00,  # Carlos
    9.70, 9.50, 13.20, 23.70, 24.00, 9.60, 18.30, 0.00, 18.50,  # Gabriela
    0.00, 0.00, 0.60, 0.00, 0.00, 0.00, 0.00, 0.20, 0.80,  # Jaqueline
    0.00, 0.00, 0.00, 0.00, 7.00, 5.50, 12.00, 0.00, 0.00,  # Maria Fernanda
    3.70, 1.40, 2.80, 0.00, 0.60, 5.60, 9.80, 0.00, 0.00   # Natalia
  )
)

# Obtenção dos resíduos
I<-9
J<-5

## Valor ajustado y_ij = m + bj +ti 
m<-mean(feijao$y)

bj<- feijao |>
  group_by(bloco) |>
  summarise(bj=mean(y))

ti<- feijao |>
  group_by(trat) |>
  summarise(ti=mean(y))

feijao <- feijao |>
  inner_join(ti) |>
  inner_join(bj) |>
  mutate(ajustado= m +ti+bj) |>
  mutate(resid = y-ajustado)

residuo<-feijao$resid


s.2<-sum(residuo^2)/(I*(J-1)) 
s<-sqrt(s.2)

residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado, pch=19)
qqline(residuo.padronizado)
plot(residuo, pch=19)

plot(residuo.padronizado~feijao$ajustado, pch=19)

bartlett.test(residuo ~ feijao$trat, data = feijao)

# O teste não rejeitou a homogeneidade, 
# porém pelo gráfico esta claro o padrão de funil
cv<-s/m * 100
attach(feijao)
boxcox((y+0.001)~trat)

# VAMOS FAZER O LOG
feijao.log <- feijao %>%
  mutate(log.y=log(y+0.001))  %>%
  dplyr::select(bloco,trat,y=log.y)

## Valor ajustado y_ij = m + bj +ti 
m<-mean(feijao.log$y)

bj<- feijao.log |>
  group_by(bloco) |>
  summarise(bj=mean(y))

ti<- feijao.log |>
  group_by(trat) |>
  summarise(ti=mean(y))

feijao.log <- feijao.log |>
  inner_join(ti) |>
  inner_join(bj) |>
  mutate(ajustado= m +ti+bj) |>
  mutate(resid = y-ajustado)

residuo<-feijao.log$resid


s.2<-sum(residuo^2)/(I*(J-1)) 
s<-sqrt(s.2)

residuo.padronizado<-(residuo-0)/s

qqnorm(residuo.padronizado, pch=19)
qqline(residuo.padronizado)
plot(residuo, pch=19)

plot(residuo.padronizado~feijao.log$ajustado, pch=19)

bartlett.test(residuo ~ feijao.log$trat, data = feijao.log)


