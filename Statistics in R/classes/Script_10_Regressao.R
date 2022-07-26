x <- c(2,3,4,5,5,6,7,7)
y <- c(17,20,21,23,25,24,27,26)

plot(x,y,xlab="Idade",ylab="Vocabul�rio")
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


#Tempos de rea��o a um est�mulo(Y) e acuidade visual(Z) de 20 indiv�duos, segundo o sexo(W) e a idade(X)
df<-data.frame(
  individuo=1:20,
  tempo_Y=c(96,92,106,100,98,104,110,101,116,106,109,100,112,105,118,108,113,112,127,117),
  sexo_W=factor(c("H","M","H","M","M","H","H","M","M","H","H","M","M","M","H","H","M","M","H","H")),
  idade_X=factor(c(20,20,20,20,25,25,25,25,30,30,30,30,35,35,35,35,40,40,40,40)),
  acuidade_Z=c(90,100,80,90,100,90,80,90,70,90,90,80,90,80,70,90,90,90,60,80))

df$n_idade_X<-as.numeric(as.character(df$idade_X))
plot(df$n_idade_X,df$tempo_Y, pch=20, xlab="Idade(x)", ylab="Est�mulo(y)", col="darkblue")
abline(lm(df$tempo_Y~df$n_idade_X), lwd=2, col="red")


#Ajustando o modelo de regress�o 
modelo <- lm(tempo_Y ~ n_idade_X,data = df)
summary(modelo)
modelo$residuals

# Predizendo valores utilizando o modelo de regress�o
predito <- predict(modelo)

#Calculando os res�duos
residuos<-cbind(df[,1:3],resid=resid(modelo))

# Calculando a ANOVA do modelo 
anova(modelo)

# Intervalos de confian�a para os coeficientes
confint(modelo)

# Intervalo de previs�o
newdata=data.frame(n_idade_X=28) # `data.frame` cont�m os dados das vari�veis explicativas utilizados para calcular as previs�es.
predict(modelo, newdata, interval="predict") 

# Res�duos padronizados
rstandard(modelo)
# Res�duos n�o padronizados
resid(modelo)

cbind(df[3:4],zi=rstandard(modelo),resid = resid(modelo))


# Plotando os res�duos 
par(mfrow=c(1,2))
plot(df$n_idade_X,resid(modelo), cex=2, col="darkblue", pch=20, xlab="Idade", ylab="Res�duos",main="(a)")
abline(h=0)
plot(df$n_idade_X,rstandard(modelo), cex=2, col="darkblue", pch=20, xlab="Idade", ylab="Res�duos padronizados",main="(b)")
abline(h=0)

# Histograma dos res�duos 
hist(resid(modelo), xlab="Res�duos", ylab="", col="lightblue3", border="white", main="")
qqnorm(resid(modelo), cex=1.5, col="darkblue", xlab="Quantis da normal padr�o", ylab="Quantis dos res�duos", pch=20)
qqline(resid(modelo), col="darkred")



par(mfrow=c(2,2))
plot(modelo)



### Exerc�cio
# Os dados a seguir s�o referentes a Temperatura e quantidade de vapor de um processo industrial.
# Construa um modelo de regress�o linear simples, verifique os pressupostos, 
# e fa�a predi��o para X = 50
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
## Regress�o linear m�ltipla
###############################
library(readxl)
library(data.table)
bebes <- as.data.table(read_excel("C:/Users/A750086/OneDrive - Atos/Documents/Documents/Projects/Petala/Aulas/CIMATEC Estat�stica Aplicada com R/Scripts/babies.xlsx"))
bebes[1:10,]
# bwt = peso ao nascer

# Verificando correla��o entre as vari�veis
cor(bebes)
pairs(bebes[,.(bwt,gestation,age,height,weight)])

# Ajustando modelo de regress�o
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


### Avalia��o gr�fica
df = data.frame(x1 = c(-2,-1,-1,-1,-1,1,1,1,1,0,0,0,0,2),
                x2 = c(2,-1,0,0,1,-1,0,0,1,0,0,0,0,-2),
                x3 = c(-2,0,0,0,0,0,0,0,0,-1,0,0,1,2))

df$Y1 = 10+ 3*df$x1 - 2*df$x2 + rnorm(14,0,1)

modelo1=lm(Y1~ x1 + x2 + x3, data=df)
summary(modelo1)

### gr�fico da vari�vel adicionada

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

####### Gr�fico da regress�o parcial (vari�vel adicionada)
library(car)
avPlots(lm(Y1~ x1 +x2 + x3 ,data = df)) 
summary(modelo1)


### Gr�fico de res�duos parciais
library(faraway)

par(mfrow=c(2,2))
prplot(modelo1,1)
prplot(modelo1,2)
prplot(modelo1,3)



#####################################################
# Fazendo avalia��o gr�fica para o modelo dos beb�s #
#####################################################
# Fa�a a avalia��o e interprete






#####################################
# Verificando observa��es influentes#
#####################################

dados2=read.table("stackloss.ascii",head=T)
dados2

### Ajuste do modelo linear m�ltiplo

modelo3=lm(stack.loss ~ Air.Flow+Water.Temp+Acid.Conc., data = dados2)
summary(modelo3)
pairs(dados2)
anova(lm(stack.loss~cbind(Air.Flow,Water.Temp,Acid.Conc.),data = dados2))

### Gr�fico dos res�duos

# Res�duos versus X (espera-se padr�o aleat�rio em torno do zero)
plot(dados2$Acid.Conc.,resid(modelo3))
abline(h=0)
plot(dados2$Water.Temp,resid(modelo3))
abline(h=0)
plot(dados2$Air.Flow,resid(modelo3))
abline(h=0)

# Avaliando normalidade dos res�duos
par(mfrow=c(1,3))
qqnorm(rstudent(modelo3),ylab="Residuos estudentizados",main=" ")
qqline(rstudent(modelo3))
hist(rstudent(modelo3),probability="TRUE", main=" ")
boxplot(rstudent(modelo3))

# Avaliando dimens�o dos res�duos
plot(rstudent(modelo3),ylab="Residuos jacknife",main="")
abline(h=0,lty=4)
abline(h=-2, lty=3)
abline(h=2, lty=3)
identify(1:nrow(dados2),rstudent(modelo3))


### Medidas de influ�ncia das observa��es no ajuste do modelo

## Leverage
# VALORES MAIORES QUE 2P/n DEVEM SER VERIFICADOS

x=model.matrix(modelo3)
lev=hat(x)
plot(lev,ylab="Leverages",main="Index plot of Leverages")
abline(h=2*4/nrow(x))
identify(1:nrow(x),lev)
# 17

## Dist�ncia de Cook
# AVALIAR OBSERVA��ES COM D>1

cook=cooks.distance(modelo3)
plot(cook,ylab="Cooks distances")
identify(1:nrow(x),cook)


## Medidas de influ�ncia (dfbeta,deffit e covratio)
#Verifica��o conjunta
IM1=influence.measures(modelo3)
View(IM1$infmat)
View(IM1$is.inf)
which(apply(IM1$is.inf,1,any))
summary(IM1)
# Obs 21 tem res�duo alto

# # Verifica��o separadamente (gr�ficos)
# #DFBETA: influ�ncia da i-�sima observa��o na estima��o do j-�simo beta
# |DFBETA|>1 ->OBS. INFLUENTE
# #DFFIT: influ�ncia da i-�sima observa��o o ajuste global
# |DFFIT|>3raiz[p/(n-p)]
# #COVRATIO: 
# Valores menores do que 1 indicam que a presen�ca da observa��o
# diminui a precis�o das estimativas dos par�metros.

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


### Gr�fico de regress�o parcial
avPlots(modelo3)
### Gr�fico de res�duos parciais
par(mfrow=c(2,2))
prplot(modelo3,1)
prplot(modelo3,2)
prplot(modelo3,3)


#2,3,4,17,21
dados2 = data.table(dados2)
dados2[,id:=seq(1,nrow(dados2))]
dados2 = dados2[!id %in% c(2,3,4,17,21)]

summary(lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = dados2))

# Sele��o autom�tica de modelo 

step(lm(bwt ~ gestation + age + height + weight + smoke, data = bebes))
step(lm(bwt ~ gestation + age + height + weight + smoke, data = bebes),direction = "backward")
step(lm(bwt ~ gestation + age + height + weight + smoke, data = bebes),direction = "forward")
