library(readxl)
library(data.table)
setwd("C:/Users/A750086/OneDrive - Atos/Documents/Documents/Projects/Petala/Aulas/CIMATEC Estatística Aplicada com R/dados")
# Lendo uma porção reduzida dos dados
df <- read_excel("banco_estadual_COVID19_15_07_2020_reduzido.xlsx")
head(df)

# Lendo uma aba específica
obitos <- read_excel("banco_estadual_COVID19_15_07_2020_reduzido.xlsx",sheet='obitos')
head(obitos)
obitos <- as.data.table(obitos)

#Entendendo o conjunto de dados
df <- as.data.table(df)
head(df)
View(df[1:100,])
str(df)
dim(df)

# Distribuições de Frequências
# Variável qualitativa
library(knitr)
freq = table(df$`CLASSIFICACAO FINAL`)

prop = round(prop.table(freq)*100,2)

kable(cbind(freq,prop),col.names = c("Freq","Prop"))

# Distribuição de frequência para Variável quantitativa
table(df$`IDADE EM ANOS`)
setnames(df,"IDADE EM ANOS","idade_em_anos")

# Transformando a variável em numérica
df[ , idade_em_anos := as.numeric(idade_em_anos)]

# Criando nova variável com grupos de valores
df[ idade_em_anos %between% c(0,18), idade_grupos := "0 - 18"]
df[ idade_em_anos %between% c(19,24), idade_grupos := "19 - 24"]
df[ idade_em_anos %between% c(25,34), idade_grupos := "25 - 34"]
df[ idade_em_anos %between% c(35,44), idade_grupos := "35 - 44"]
df[ idade_em_anos %between% c(45,54), idade_grupos := "45 - 54"]
df[ idade_em_anos >= 55, idade_grupos := "55 +"]

#Construindo tabela de frequência
freq = table(df$idade_grupos)
prop = round(prop.table(table(df$idade_grupos))*100,2)
kable(cbind(freq,prop),col.names = c("Freq","Prop"))

#######################
# Desafio de estudo ###
#######################

#Entenda a função de agrupamento

agrupar <- function(x, grupos = NULL, sep = "-", text = ""){
  # Input: Vetor com valores numéricos.
  # Output: Fator com nomes dos grupos da variável numérica.
  if(length(grupos) < 2) {
    stop("Número de grupos deve ser maior que 2.")		
  }
  x <- as.numeric(x)
  brackets <- paste(grupos, grupos[2:length(grupos)]-1, sep = sep)
  brackets[length(grupos)] <- paste0(grupos[length(grupos)], " +")
  x <- factor(findInterval(x, grupos), levels = c(1:length(brackets)))
  levels(x) <- paste0(brackets, text)
  x
}

df$idade_grupos <- agrupar(df$idade_em_anos, grupos = c( 18, 25, 35, 45, 55))
table(df$idade_grupos)


# Gráficos para Variáveis Qualitativas

#Gráfico em barras
setnames(df,'CLASSIFICACAO FINAL','classificacao_final')
barplot(table(df$classificacao_final))

# Gráfico de barras mais elegante com ggplot2
library(ggplot2)

# Base do gráfico, painel vazio
ggplot(data = df)

# Precisamos definir quais variáveis serão mapeadas nos aspectos visuais do gráfico
ggplot(data = df,aes(x=classificacao_final))

# Adicionando camadas ao gráfico

ggplot(df, aes(x=classificacao_final)) + 
  geom_bar() 

# Melhorando a aparência
ggplot(df, aes(x=classificacao_final)) + 
  geom_bar() + 
  labs(x="Classificação Final", y="Frequência", title="COVID-19") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.3))

# Transformando a variável em fator e renomando os níveis do fator
table(df$classificacao_final)                                  
levels(df$classificacao_final)
names(table(df$classificacao_final))                                  

df$classificacao_final <- factor(df$classificacao_final)
levels(df$classificacao_final) <- c("Confirmado","Confirmado","Confirmado","Confirmado","Descartado","Descartado","Suspeito")
df$classificacao_final <- factor(df$classificacao_final,levels=c("Descartado","Confirmado","Suspeito"))

table(df$classificacao_final)                                  
str(df$classificacao_final)
levels(df$classificacao_final)

# Construindo o gráfico de barras com a nova variável classificacao_final
ggplot(df, aes(x=classificacao_final)) + 
  geom_bar() + 
  labs(x="Classificação Final", y="Frequência", title="COVID-2019")


# Gráfico em setores

# Preparando o conjunto de dados
freq = table(df$classificacao_final)
prop = round(prop.table(table(df$classificacao_final))*100,2)
df_pie = data.table(cbind(freq,prop))
df_pie[,classificacao_final := levels(df$classificacao_final)]  

# Construindo o gráfico
ggplot(df_pie, aes(x='',y=prop,fill = classificacao_final)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0)


# Gráficos para variáveis quantitativas

# Histograma
ggplot(df[!is.na(idade_em_anos)], aes(x=idade_em_anos)) + 
  geom_histogram(bins = 30) + 
  labs(x="Idade", y="Frequência", title="COVID-19")


# Diagrama de dispersão
plot(df$idade_em_anos)


# Medidas Resumo
# Moda

z <- table(df$idade_em_anos) ; names(z)[z == max(z)]
moda = names(z)[z == max(z)]
moda

# Mediana
x <- c(3, 4, 7, 8 , 8)
median(x)

# Media
x<- c(3, 4, 7, 8, 8)	
mean(x)

#############
## Desafio ##
#############

#Qual a idade mediana dos individuos testados para o covid-19?
#E qual a idade média?
#Quais as idades média e mediana entre os testados positivos e negativos para o covid-19?
  

# Medidas de dispersão

# Desvio padrão
grupoA <- c(3,4,5,6,7)
sd(grupoA) ; sd(grupoA)^2
var(grupoA)

grupoB <- c(5,5,5,5,5,5) ; mean(grupoB); sd(grupoB)
#############
## Desafio ##
#############

# Crie uma função que calcula o desvio padrão. Não vale utilizar a função pronta do R sd.
# Confirme se a função sd calcula o desvio padrão considerando n ou n-1


#Quantis Empíricos
grupoA <- c(3,4,5,6,7)
mean(grupoA)
sd(grupoA)
var(grupoA)
median(grupoA)

newgrupoA <- c(3,4,5,6,7,100)
mean(newgrupoA)
sd(newgrupoA)
var(newgrupoA)
median(newgrupoA)

summary(newgrupoA)
summary(grupoA)

# Boxplot
ggplot( df[!is.na(idade_em_anos)] , aes(y=as.numeric(idade_em_anos))) + 
  geom_boxplot() + 
  labs(x="", y="Idade", title="COVID-19")

ggplot( obitos , aes(y=IDADE)) + 
  geom_boxplot() + 
  labs(x="", y="Idade", title="COVID-19")


######## Gabarito ########

mean(df[ `RESULTADO DO TESTE` %in% c('POSITIVO') ]$idade_em_anos ,
     na.rm=T)

mean(df[`RESULTADO DO TESTE` %in% 'NEGATIVO']$idade_em_anos,na.rm=T)

median(df[`RESULTADO DO TESTE` %in% 'NEGATIVO']$idade_em_anos,na.rm=T)
median(df[`RESULTADO DO TESTE` %in% 'POSITIVO']$idade_em_anos,na.rm=T)

df[!is.na(idade_em_anos),
   list(mean=mean(idade_em_anos),
        median = median(idade_em_anos),
        sd=sd(idade_em_anos),
        max = max(idade_em_anos),
        min = min(idade_em_anos))
   ,
   by=`RESULTADO DO TESTE`]



# Atividades em sala

ggplot(obitos, aes(x=IDADE)) + 
  geom_histogram(bins = 30) + 
  labs(x="Idade", y="Frequência", title="COVID-19")

z <- table(obitos$IDADE) ; names(z)[z == max(z)]

sum(is.na(df$idade_em_anos))

median(df[!is.na(idade_em_anos)]$idade_em_anos)

median(obitos$IDADE)

mean(obitos$IDADE)
mean(df$idade_em_anos,na.rm=T)

round( mean(obitos$IDADE),0 )


media <- function(x){
  media = sum(x)/length(x)
  media
}
x
media(x)
mean(x)

desvio <- function(x){
  
  media = media(x)
  a = (x - media)^2
  b = (length(x)-1)
  
  desvio = sqrt(sum(a/b))
  desvio
}

desvio(x)
sd(x)

max(df$idade_em_anos,na.rm=T) - min(df$idade_em_anos,na.rm=T)
