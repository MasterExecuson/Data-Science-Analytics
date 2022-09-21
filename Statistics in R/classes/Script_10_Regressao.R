x <- c(2,3,4,5,5,6,7,7)
y <- c(17,20,21,23,25,24,27,26)

plot(x,y,xlab="Idade",ylab="Vocabulário")
abline(15,1.01) # y = 15 +1.01X
abline(14,2.5) # y = 14 + 2,5X
abline(14,2)
abline(13.9,1.7)
abline(lm(y~x))
# y = a 
modelo = lm(y ~ x)
modelo
summary(modelo)
hist(modelo$residuals)

anova(modelo)

plot(modelo)
plot(x,modelo$residuals)
abline(0,0)


#Tempos de reação a um estímulo(Y) e acuidade visual(Z) de 20 indivíduos, segundo o sexo(W) e a idade(X)
df<-data.frame(
  individuo=1:20,
  tempo_Y=c(96,92,106,100,98,104,110,101,116,106,109,100,112,105,118,108,113,112,127,117),
  sexo_W=factor(c("H","M","H","M","M","H","H","M","M","H","H","M","M","M","H","H","M","M","H","H")),
  idade_X=factor(c(20,20,20,20,25,25,25,25,30,30,30,30,35,35,35,35,40,40,40,40)),
  acuidade_Z=c(90,100,80,90,100,90,80,90,70,90,90,80,90,80,70,90,90,90,60,80))

df$n_idade_X<-as.numeric(as.character(df$idade_X))
plot(df$n_idade_X,df$tempo_Y, pch=20, xlab="Idade(x)", ylab="Estímulo(y)", col="darkblue")
abline(lm(df$tempo_Y~df$n_idade_X), lwd=2, col="red")


#Ajustando o modelo de regressão 
modelo <- lm(tempo_Y ~ n_idade_X,data = df)
summary(modelo)
modelo$residuals

# Predizendo valores utilizando o modelo de regressão
predito <- predict(modelo)

#Calculando os resíduos
residuos<-cbind(df[,1:3],resid=resid(modelo))

# Calculando a ANOVA do modelo 
anova(modelo)

# Intervalos de confiança para os coeficientes
confint(modelo)

# Intervalo de previsão
newdata=data.frame(n_idade_X=28) # `data.frame` contém os dados das variáveis explicativas utilizados para calcular as previsões.
predict(modelo, newdata, interval="predict") 

# Resíduos padronizados
rstandard(modelo)
# Resíduos não padronizados
resid(modelo)

cbind(df[3:4],zi=rstandard(modelo),resid = resid(modelo))


# Plotando os resíduos 
par(mfrow=c(1,2))
plot(df$n_idade_X,resid(modelo), cex=2, col="darkblue", pch=20, xlab="Idade", ylab="Resíduos",main="(a)")
abline(h=0)
plot(df$n_idade_X,rstandard(modelo), cex=2, col="darkblue", pch=20, xlab="Idade", ylab="Resíduos padronizados",main="(b)")
abline(h=0)

# Histograma dos resíduos 
hist(resid(modelo), xlab="Resíduos", ylab="", col="lightblue3", border="white", main="")
qqnorm(resid(modelo), cex=1.5, col="darkblue", xlab="Quantis da normal padrão", ylab="Quantis dos resíduos", pch=20)
qqline(resid(modelo), col="darkred")



par(mfrow=c(2,2))
plot(modelo)



### Exercício
# Os dados a seguir são referentes a Temperatura e quantidade de vapor de um processo industrial.
# Construa um modelo de regressão linear simples, verifique os pressupostos, 
# e faça predição para X = 50
df<-data.frame(n=1:25,
               xi=c(35.3,29.7,30.8,58.8,61.4,71.3,74.4,76.7,70.7,57.5,46.4,28.9,28.1,39.1,46.8,48.5,59.3,70,70,74.5,72.1,58.1,44.6,33.4,28.6),
               yi=c(10.98,11.13,12.51,8.4,9.27,8.73,6.36,8.5,7.82,9.14,8.24,12.19,11.88,9.57,10.94,9.58,10.09,8.11,6.83,8.88,7.68,8.47,8.86,10.36,11.08))

plot(df$xi,df$yi)
abline(lm(yi~xi, data=df))
modelo = lm(yi~xi,data=df)
# Modelo: yi = 13,62299 -0,07983(Xi)
summary(modelo)
anova(modelo)
par(mfrow=c(2,2))
plot(modelo)
hist(rstandard(modelo))
shapiro.test(rstandard(modelo))
plot(df$xi,rstandard(modelo))
plot(predict(modelo),rstandard(modelo))
confint(modelo)
predict(modelo, data.frame(xi = 60), interval="predict") 
predict(modelo)



###############################
## Regressão linear múltipla
###############################
library(readxl)
library(data.table)
bebes <- as.data.table(read_excel("C:/Users/A750086/OneDrive - Atos/Documents/Documents/Projects/Petala/Aulas/CIMATEC Estatística Aplicada com R/Scripts/babies.xlsx"))
bebes[1:10,]
# bwt = peso ao nascer

# Verificando correlação entre as variáveis
cor(bebes)
pairs(bebes[,.(bwt,gestation,age,height,weight)])

# Ajustando modelo de regressão
modelo <- lm(bwt ~ age + smoke + gestation,data=bebes)
summary(modelo)
x1x2x3 = cbind(bebes$age, bebes$smok,bebes$gestation)
anova(lm(bebes$bwt ~ x1x2x3))

par(mfrow = c(2,2), mar=c(4,4,2,2), cex.lab=1.2,
    cex.axis=1.2, las=1,  bty="n")
plot(modelo)

anova(lm(stack.loss~cbind(Air.Flow,Water.Temp,Acid.Conc.),data = dados2))


# Comparando dois modelos:

modelocompleto = lm(bwt ~ age+smoke+gestation,data=bebes)
modeloreduzido = lm(bwt ~ smoke+gestation,data=bebes)
summary(modelocompleto)
summary(modeloreduzido)

anova(modelocompleto,modeloreduzido)


### Avaliação gráfica
df = data.frame(x1 = c(-2,-1,-1,-1,-1,1,1,1,1,0,0,0,0,2),
                x2 = c(2,-1,0,0,1,-1,0,0,1,0,0,0,0,-2),
                x3 = c(-2,0,0,0,0,0,0,0,0,-1,0,0,1,2))

df$Y1 = 10+ 3*df$x1 - 2*df$x2 + rnorm(14,0,1)

modelo1=lm(Y1~ x1 + x2 + x3, data=df)
summary(modelo1)

### gráfico da variável adicionada

modx2x3 = lm(Y1 ~ x2 + x3,data=df)
modx1 = lm(x1 ~ x2 + x3, data=df)
e = resid(modx2x3)
u = resid(modx1)
plot(e,u)
abline(lm(u~e))

modx1x2=lm(Y1~x1+x2, data=df)
modx3=lm(x3~x1+x2, data=df)
plot(resid(modx1x2),resid(modx3))
abline(h=0)

modx1x3=lm(Y1~x1+x3, data=df)
modx2=lm(x2~x1+x3,data=df)
plot(resid(modx1x3),resid(modx2))

####### Gráfico da regressão parcial (variável adicionada)
library(car)
avPlots(lm(Y1~ x1 +x2 + x3 ,data = df)) 
summary(modelo1)


### Gráfico de resíduos parciais
library(faraway)

par(mfrow=c(2,2))
prplot(modelo1,1)
prplot(modelo1,2)
prplot(modelo1,3)



#####################################################
# Fazendo avaliação gráfica para o modelo dos bebês #
#####################################################
# Faça a avaliação e interprete






#####################################
# Verificando observações influentes#
#####################################

dados2=read.table("stackloss.ascii",head=T)
dados2

### Ajuste do modelo linear múltiplo

modelo3=lm(stack.loss ~ Air.Flow+Water.Temp+Acid.Conc., data = dados2)
summary(modelo3)
pairs(dados2)
anova(lm(stack.loss~cbind(Air.Flow,Water.Temp,Acid.Conc.),data = dados2))

### Gráfico dos resíduos

# Resíduos versus X (espera-se padrão aleatório em torno do zero)
plot(dados2$Acid.Conc.,resid(modelo3))
abline(h=0)
plot(dados2$Water.Temp,resid(modelo3))
abline(h=0)
plot(dados2$Air.Flow,resid(modelo3))
abline(h=0)

# Avaliando normalidade dos resíduos
par(mfrow=c(1,3))
qqnorm(rstudent(modelo3),ylab="Residuos estudentizados",main=" ")
qqline(rstudent(modelo3))
hist(rstudent(modelo3),probability="TRUE", main=" ")
boxplot(rstudent(modelo3))

# Avaliando dimensão dos resíduos
plot(rstudent(modelo3),ylab="Residuos jacknife",main="")
abline(h=0,lty=4)
abline(h=-2, lty=3)
abline(h=2, lty=3)
identify(1:nrow(dados2),rstudent(modelo3))


### Medidas de influência das observações no ajuste do modelo

## Leverage
# VALORES MAIORES QUE 2P/n DEVEM SER VERIFICADOS

x=model.matrix(modelo3)
lev=hat(x)
plot(lev,ylab="Leverages",main="Index plot of Leverages")
abline(h=2*4/nrow(x))
identify(1:nrow(x),lev)
# 17

## Distância de Cook
# AVALIAR OBSERVAÇÕES COM D>1

cook=cooks.distance(modelo3)
plot(cook,ylab="Cooks distances")
identify(1:nrow(x),cook)


## Medidas de influência (dfbeta,deffit e covratio)
#Verificação conjunta
IM1=influence.measures(modelo3)
View(IM1$infmat)
View(IM1$is.inf)
which(apply(IM1$is.inf,1,any))
summary(IM1)
# Obs 21 tem resíduo alto

# # Verificação separadamente (gráficos)
# #DFBETA: influência da i-ésima observação na estimação do j-ésimo beta
# |DFBETA|>1 ->OBS. INFLUENTE
# #DFFIT: influência da i-ésima observação o ajuste global
# |DFFIT|>3raiz[p/(n-p)]
# #COVRATIO: 
# Valores menores do que 1 indicam que a presen»ca da observação
# diminui a precisão das estimativas dos parâmetros.

dfbet=dfbetas(modelo3)
dff=dffits(modelo3)
covr=covratio(modelo3)

par(mfrow=c(1,3))
plot(dfbet[,2],ylab="DFBetas para Air.Flow")
abline(h=0)
plot(dfbet[,3],ylab="DFBetas para Water.Temp")
abline(h=0)
plot(dfbet[,4],ylab="DFBetas para Acid.Conc")
abline(h=0)


plot(dff,ylab="DFFITs")
abline(h=0)
abline(h=(3*sqrt(4/(nrow(dados2)-4))))
abline(h=-(3*sqrt(4/(nrow(dados2)-4))))
identify(1:nrow(x),dff)


plot(covr,ylab="COVRATIO")
abline(h=1)
identify(1:nrow(x),covr)


### Gráfico de regressão parcial
avPlots(modelo3)
### Gráfico de resíduos parciais
par(mfrow=c(2,2))
prplot(modelo3,1)
prplot(modelo3,2)
prplot(modelo3,3)


#2,3,4,17,21
dados2 = data.table(dados2)
dados2[,id:=seq(1,nrow(dados2))]
dados2 = dados2[!id %in% c(2,3,4,17,21)]

summary(lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = dados2))

# Seleção automática de modelo 

step(lm(bwt ~ gestation + age + height + weight + smoke, data = bebes))
step(lm(bwt ~ gestation + age + height + weight + smoke, data = bebes),direction = "backward")
step(lm(bwt ~ gestation + age + height + weight + smoke, data = bebes),direction = "forward")
