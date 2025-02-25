library("dplyr")
library("tidyr")
# Criando o data.frame
dados <- data.frame(
  maquina = rep(c(1, 2, 3, 4), each = 24),  # 4 máquinas x 6 tratamentos x 4 repetições
  processo = rep(rep(c(1, 2), each = 4), times = 12),  # 2 processos x 6 horários x 4 repetições
  hora = rep(rep(c("08:00", "11:00", "15:00"), each = 8), times = 4),  # 3 horários x 4 repetições
  rep = rep(1:4, times = 24),  # 4 repetições por tratamento
  comprimento = c(
    # Máquina 1
    6, 5, 4, 3, 4, 6, 5, 3, 6, 3, 1, 0,  
    3, 1, 1, 0, 5, 4, 9, 6, 6, 3, 0, 7,  
    # Máquina 2
    7, 9, 5, 5, 6, 5, 4, 3, 8, 7, 4, 8,  # Processo 1
    6, 4, 1, 3, 10, 11, 6, 4, 8, 10, 7, 0,  # Processo 2
    # Máquina 3
    1, 0, 2, 4, 0, 0, 0, 1, 3, 1, 2, 0,  # Processo 1
    2, 0, 0, 1, 0, 2, 6, 1, 0, 4, 0, 0,  # Processo 2
    # Máquina 4
    5, 5, 6, 3, 3, 4, 4, 3, 7, 11, 9, 6,  # Processo 1
    9, 4, 6, 3, 8, 5, 4, 6, 4, 3, 5, 0   # Processo 2
  )
)

attach(dados)
correcao<-(sum(comprimento)^2)/96
SQTOTAL<- sum(comprimento^2) - correcao
# Totais de tratamento (24)
T.t<- dados |>
  group_by(maquina,processo, hora) |>
  summarise(tt=sum(comprimento))
T.t<-T.t$tt
SQTRAT<-sum(T.t^2)/4 - correcao

SQRES<-SQTOTAL-SQTRAT

TP<-dados |>
  group_by(processo) |>
  summarise(tp=sum(comprimento))
TP<-TP$tp

SQP<-sum(TP^2)/48 - correcao

TM<-dados |>
  group_by(maquina) |>
  summarise(tm=sum(comprimento))
TM<-TM$tm

SQM<-sum(TM^2)/24 - correcao

TH<-dados |>
  group_by(hora) |>
  summarise(th=sum(comprimento))
TH<-TH$th

SQH<-sum(TH^2)/32 - correcao

# Tabela processo e maquina
dados |>
  group_by(maquina, processo) |>
  summarise(tmp=sum(comprimento)) |>
  pivot_wider(names_from = maquina, values_from = tmp)

TMP<-dados |>
  group_by(maquina, processo) |>
  summarise(tmp=sum(comprimento)) 
SQMP<-sum(TMP$tmp^2)/12 - correcao - SQM-SQP



# Tabela hora e maquina
dados |>
  group_by(maquina, hora) |>
  summarise(tmh=sum(comprimento)) |>
  pivot_wider(names_from = maquina, values_from = tmh)

TMH<-dados |>
  group_by(maquina, hora) |>
  summarise(tmh=sum(comprimento)) 
SQMH<-sum(TMH$tmh^2)/8 - correcao - SQM-SQH




# Tabela hora e processo
dados |>
  group_by(processo, hora) |>
  summarise(tph=sum(comprimento)) |>
  pivot_wider(names_from = hora, values_from = tph)

TPH<-dados |>
  group_by(processo, hora) |>
  summarise(tph=sum(comprimento)) 
SQPH<-sum(TPH$tph^2)/16 - correcao - SQP-SQH

# Interação tripla
SQPMH<-SQTRAT-SQPH-SQMH-SQMP-SQM-SQH-SQP

qm<-c(68.34,99.09,5.7604,2.5382,44.03125,0.5104167,1.309033)
qmres<-4.489583
f<-qm/qmres
round(pf(f, df1=c(1,3,2,3,2,6,6), df2=72, lower.tail = F),3)



# Medias de maquinas

TM.medias<-dados |>
  group_by(maquina) |>
  summarise(tm=mean(comprimento))
medias.M<-TM.medias$tm


qtukey(0.05, nmeans = 4, df = 72, lower.tail = F)

f<-c(11.2812,32,28.125)/4.489583
pf(f, df1=1,df2=72, lower.tail = F)

# Tabela hora e processo
dados |>
  group_by(processo, hora) |>
  summarise(tph=mean(comprimento)) |>
  pivot_wider(names_from = hora, values_from = tph)
pf(c(1.0348,0.5893), df1=2, df2=72, lower.tail = F)

cores<-c("#a8e6cf",'#ff8b94',"#c2a5cf","#edf8b1")


mediasHP<-dados |>
  group_by(processo, hora) |>
  summarise(mph=mean(comprimento))
mediasHP$hora<-c(1:3,1:3)
plot(mediasHP$mph~mediasHP$hora, col=cores[mediasHP$processo], pch=19, xaxt = "n")
axis(1, 1:3,c("08:00", "11:00", "15:00"))
legend("topleft",legend=paste0("processo ",1:2), lwd=2, col=cores[1:2] ,bty="")


plot(mediasHP$mph~mediasHP$processo, col=cores[c(1:3,1:3)], pch=19)
legend("topright",legend=paste0("hora ",1:3), lwd=2, col=cores[1:3] ,bty="")


