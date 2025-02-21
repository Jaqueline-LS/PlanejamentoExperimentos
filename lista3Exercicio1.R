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
