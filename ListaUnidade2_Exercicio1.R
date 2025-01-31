library("dplyr")
ratos<-data.frame(resp=c(83,86,103,116,132,
                         63,69,79,81,98,
                         55,61,79,79,91),
                  bloco=rep(1:3,c(5,5,5)),
                  trat=rep(c(30,34,38,42,46),3))

I<-5
J<-3

m.hat<-mean(ratos$resp)


# Total de tratamentos
Ti<-ratos |>
  group_by(trat) |>
  summarise(Ti=sum(resp))

# Total de blocos
Bi<- ratos |>
  group_by(bloco) |>
  summarise(Bi=sum(resp))
Ti<-Ti$Ti
Bi<-Bi$Bi
ti<- rep(Ti,3)/J - m.hat
bj<- rep(Bi,c(5,5,5))/I -m.hat
ajustado<-m.hat + ti + bj
residuos<- ratos$resp - ajustado
hist(residuos)

shapiro.test(residuos)

plot(ajustado, residuos, pch=19)
bartlett.test(resp ~ trat, data=ratos)


# Podemos fazer a ANOVA
correcao<- (sum(ratos$resp)^2)/(I*J)

SQTRAT<- sum(Ti^2)/J - correcao

SQBLOCO<- sum(Bi^2)/I - correcao

SQTOTAL<- sum(ratos$resp^2) - correcao

SQRES<-SQTOTAL-SQBLOCO-SQTRAT

# Variancia para o CV

s.2<-sum(residuos^2)/(I*(J-1)) # variância média
s<-sqrt(s.2)
s

CV<-s/m.hat * 100
CV


si<-apply(residuo.squared,1,FUN=function(x)sum(x)/I) #variância no trat i

sum(si)/I

s.2<-sum(residuo^2)/(I*(J-1)) # variância média


s<-sqrt(s.2)

Ti
c.1<-c(-2,-1,0,1,2)
c.2<-c(2,-1,-2,-1,2)
c.3<-c(-1,2,0,-2,1)
c.4<-c(1,-4,6,-4,1)

SQRL<-(t(Ti)%*%c.1)^2 / (J*sum(c.1^2)) 
SQ2<-(t(Ti)%*%c.2)^2 / (J*sum(c.2^2)) 
SQ3<-(t(Ti)%*%c.3)^2 / (J*sum(c.3^2)) 
SQ4<-(t(Ti)%*%c.4)^2 / (J*sum(c.4^2)) 

RL<-lm(resp~trat, data=ratos)



