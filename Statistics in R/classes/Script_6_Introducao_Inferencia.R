library(data.table)
setwd('C:/Users/A750086/OneDrive - Atos/Documents/Documents/Projects/Petala/Aulas/CIMATEC Estatística Aplicada com R - Turma 81758 - 2022/dados')
load("covidBAreduzido.RData")

# Selecionando uma amostra aleat?ria simples da vari?vel idade. 
aas = sample(df$idade_anos, size = 100, replace=T)
aas[1:10]
# Calculando a m?dia da vari?vel idade
mean(df$idade_anos,na.rm=T)

# Calculando a m?dia da aas
mean(aas,na.rm=T)

aas[1:10]


# Distribui??o amostral
mean(df$idade_anos,na.rm=T)
var(df$idade_anos,na.rm=T)
sd(df$idade_anos,na.rm=T)
medias = c(mean(sample(df[!is.na(idade_anos)]$idade_anos,2))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,4))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,6))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,8))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,20))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,50))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,100))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,200))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,300))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,400))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,500))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,600))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,700))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,800))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,900))
           ,mean(sample(df[!is.na(idade_anos)]$idade_anos,1000))
)
plot(medias)
abline(a=mean(df$idade_anos,na.rm=T),b=0)
hist(df$idade_anos)

#Inicializando as variaveis como vetores numericos
## Media para amostras de tam 4
xbar4<-numeric()
## Media para amostras de tam 8
xbar8<-numeric()
## Media para amostras de tam 11
xbar11<-numeric()
## Media para amostras de tam 15
xbar15<-numeric()
## Media para amostras de tam 30
xbar30<-numeric()
## Media para amostras de tam 100
xbar100<-numeric()

for ( i in 1:200){
  # Extraindo amostras de tamanho 4 e calculando a m?dia
  smp<-sample(df[!is.na(idade_anos)]$idade_anos,size = 4)
  xbar4[i]<-round(mean(smp),2)
  
  # Extraindo amostras de tamanho 8 e calculando a m?dia
  smp<-sample(df[!is.na(idade_anos)]$idade_anos,size = 8)
  xbar8[i]<-round(mean(smp),2)
  
  # Extraindo amostras de tamanho 11 e calculando a m?dia
  smp<-sample(df[!is.na(idade_anos)]$idade_anos,size = 11)
  xbar11[i]<-round(mean(smp),2)

  # Extraindo amostras de tamanho 15 e calculando a m?dia
  smp<-sample(df[!is.na(idade_anos)]$idade_anos,size = 15)
  xbar15[i]<-round(mean(smp),2)

  # Extraindo amostras de tamanho 30 e calculando a m?dia
  smp<-sample(df[!is.na(idade_anos)]$idade_anos,size = 30)
  xbar30[i]<-round(mean(smp),2)

  # Extraindo amostras de tamanho 100 e calculando a m?dia
  smp<-sample(df[!is.na(idade_anos)]$idade_anos,size = 100)
  xbar100[i]<-round(mean(smp),2)
}

par(mfrow=c(2,3))
hist(xbar4, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=4 ",round(mean(xbar15),2)))
hist(xbar8, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=8 ",round(mean(xbar30),2)))
hist(xbar11, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=11 ",round(mean(xbar100),2)))
hist(xbar15, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=15 ",round(mean(xbar15),2)))
hist(xbar30, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=30 ",round(mean(xbar30),2)))
hist(xbar100, col="lightblue4", border="white",freq = FALSE, breaks = 15, main=paste0("n=100 ",round(mean(xbar100),2)))




# Calculando probabilidades

q1 = (32.07-42.07)/(sd(df$idade_anos,na.rm=T)/sqrt(1000))
q2 = (52.07-42.07)/(sd(df$idade_anos,na.rm=T)/sqrt(1000))

pnorm(q1,lower.tail = F) - pnorm(q2,lower.tail = F)

sd(df[!is.na(idade_anos)]$idade_anos)


# Distribui??o amostral de uma propor??o
library(readxl)
obitos <- read_excel("banco_estadual_COVID19_15_07_2020_reduzido.xlsx",sheet='obitos')
obitos <- as.data.table(obitos)

# Calculando propor??o de ?bitos por sexo 
prop.table(table(obitos$SEXO))
p = 0.56
q = 0.44
n = 10
variancia = p*q/n  
q1 = (-0.01)/(sqrt(variancia))
q2 = (0.01)/(sqrt(variancia))

pnorm(q1,lower.tail = F) - pnorm(q2,lower.tail = F)

# Para n maior
n = 200
variancia = p*q/n  
q1 = (-0.05)/(sqrt(variancia))
q2 = (0.05)/(sqrt(variancia))

pnorm(q1,lower.tail = F) - pnorm(q2,lower.tail = F)

q1 = (-0.01)/(sqrt(variancia))
q2 = (0.01)/(sqrt(variancia))

pnorm(q1,lower.tail = F) - pnorm(q2,lower.tail = F)


# Para n ainda maior
n = 1000
variancia = p*q/n  
q1 = (-0.05)/(sqrt(variancia))
q2 = (0.05)/(sqrt(variancia))

pnorm(q1,lower.tail = F) - pnorm(q2,lower.tail = F)


pnorm(-1.96,lower.tail=F) - pnorm(1.96,lower.tail=F)

qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


# Calculando vari?ncia da idade entre ?bitos
var(obitos$IDADE)

# Calculando tamanho da amostra 
(var(obitos$IDADE)*((1.96)^2))/((2)^2)

mean(obitos$IDADE)

aas = sample(obitos$IDADE,replace=T, size = 268 )
mean(aas)

aas = sample(obitos$IDADE,replace=T, size = 27 )

mean(aas)
