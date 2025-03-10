# Limpeza do ambiente e carregamento de pacotes
rm(list = ls(all = TRUE))
library("dplyr")
library("MASS")
cores <- c("#C9E69E", "#FFC29A", "#FF9B95")

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

feijao<-feijao |>
  filter(bloco!="Jaqueline")
# Obtenção dos resíduos
I <- 9
J <- 5

## Valor ajustado y_ij = m + bj + ti 
m <- mean(feijao$y)

bj <- feijao |>
  group_by(bloco) |>
  summarise(bj = mean(y) - m)

ti <- feijao |>
  group_by(trat) |>
  summarise(ti = mean(y) - m)

feijao <- feijao |>
  inner_join(ti) |>
  inner_join(bj) |>
  mutate(ajustado = m + ti + bj) |>
  mutate(resid = y - ajustado)

residuo <- feijao$resid

s.2 <- sum(residuo^2) / ((I - 1) * (J - 1)) 
s <- sqrt(s.2)

residuo.padronizado <- (residuo - 0) / s

# Gráfico Q-Q plot dos resíduos padronizados
png("qqplot_residuos_padronizados.png")
qqnorm(residuo.padronizado, pch = 19, main = "Q-Q Plot dos Resíduos Padronizados", xlab = "Quantis Teóricos", ylab = "Resíduos Padronizados")
qqline(residuo.padronizado)
dev.off()

# Gráfico de resíduos
png("plot_residuos.png")
plot(residuo, pch = 19, main = "Gráfico de Resíduos", xlab = "Índice", ylab = "Resíduos")
dev.off()

# Gráfico de resíduos padronizados vs valores ajustados
png("residuos_padronizados_vs_ajustados.png")
plot(residuo.padronizado ~ feijao$ajustado, pch = 19, main = "Resíduos Padronizados vs Valores Ajustados", xlab = "Valores Ajustados", ylab = "Resíduos Padronizados")
dev.off()

# Teste de Bartlett para homogeneidade de variâncias
bartlett.test(residuo ~ feijao$trat, data = feijao)

# Coeficiente de variação
cv <- s / m * 100

attach(feijao)
boxcox((feijao$y + 0.001) ~ feijao$trat)

# Transformação logarítmica
feijao.log <- feijao %>%
  mutate(log.y = log(y + 0.001))  %>%
  dplyr::select(bloco, trat, y = log.y)

## Valor ajustado y_ij = m + bj + ti 
m <- mean(feijao.log$y)

bj <- feijao.log |>
  group_by(bloco) |>
  summarise(bj = mean(y) - m)

ti <- feijao.log |>
  group_by(trat) |>
  summarise(ti = mean(y) - m)

feijao.log <- feijao.log |>
  inner_join(ti) |>
  inner_join(bj) |>
  mutate(ajustado = m + ti + bj) |>
  mutate(resid = y - ajustado)

residuo <- feijao.log$resid

s.2 <- sum(residuo^2) / (I * (J - 1)) 
s <- sqrt(s.2)

residuo.padronizado <- (residuo - 0) / s

# Gráfico Q-Q plot dos resíduos padronizados após transformação logarítmica
png("qqplot_residuos_padronizados_log.png")
qqnorm(residuo.padronizado, pch = 19, main = "Q-Q Plot dos Resíduos Padronizados (Log)", xlab = "Quantis Teóricos", ylab = "Resíduos Padronizados")
qqline(residuo.padronizado)
dev.off()

# Gráfico de resíduos padronizados após transformação logarítmica
png("plot_residuos_padronizados_log.png")
plot(residuo.padronizado, pch = 19, main = "Gráfico de Resíduos Padronizados (Log)", xlab = "Índice", ylab = "Resíduos Padronizados")
dev.off()

# Gráfico de resíduos padronizados vs valores ajustados após transformação logarítmica
png("residuos_padronizados_vs_ajustados_log.png")
plot(residuo.padronizado ~ feijao.log$ajustado, pch = 19, main = "Resíduos Padronizados vs Valores Ajustados (Log)", xlab = "Valores Ajustados", ylab = "Resíduos Padronizados")
dev.off()

# Teste de Bartlett para homogeneidade de variâncias após transformação logarítmica
bartlett.test(residuo ~ feijao.log$trat, data = feijao.log)



  

#----Anova-------

Bj <- feijao.log |>
  group_by(bloco) |>
  summarise(Bj = sum(y))

Ti <- feijao.log |>
  group_by(trat) |>
  summarise(Ti = sum(y))

attach(feijao.log)

CORRECAO <- (sum(y)^2)/(I*J)
SQTOTAL <- sum(y^2) - CORRECAO
SQTRAT <- sum(Ti$Ti^2)/(J) - CORRECAO
SQBLOCO <- sum(Bj$Bj^2)/(I) - CORRECAO
SQRES<- SQTOTAL-SQTRAT-SQBLOCO


ExpDes::rbd(feijao.log$trat, feijao.log$bloco, feijao.log$y)
