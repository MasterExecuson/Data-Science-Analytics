# Obter o diret�rio de trabalho
getwd()

#Definir o diret�rio de trabalho
setwd('C:/Users/A750086/OneDrive - Atos/Documents/Documents/Projects/Petala/Aulas/CIMATEC Estat�stica Aplicada com R/dados')

# Instalando e carregando pacotes
#install.packages('ggplot2')
library('ggplot2')

# R com ocalculadora 
10+2
10-2
10*2
10/2
10^2
10**2
9%%2
9%/%2

10<2
10<=2
10>2
10==2
10!=2

# If, else e else if 
condicao_1 = TRUE
condicao_2 = FALSE

if(condicao_1){
  
  print('Condi��o 1 � verdadeira')
  
} else if (condicao_2){
  
  print('Condi��o 2 � verdadeira')
  
} else {
  
  print('Nenhuma condi��o � verdadeira')
  
}


# For
for (i in 1:10){
  print(paste0('i = ',i))
}

# Ifelse
x <- c(6:-4)
x
sqrt(x)  #- gives warning
sqrt(ifelse(x >= 0, x, NA))  # no warning

# While (N�O EXECUTAR)

# while(T){
#   print('Loop infinito')
# }



# Vetoriza��o
numeros = 1:5
numeros

log10(numeros)
2^numeros


# Reciclagem
x <- c(1,5)
x
y <- c(1,10,100)
y
x + y

# Fun��es
# fun��o que ecoa uma palavra
ecoar <- function(palavra, n_ecos = 3) {
  paste(c(rep(palavra, n_ecos), "!"), collapse = " ")
}

ecoar("eco")
ecoar("eco",5)


# Criar fun��o que recebe dois argumentos x e y e realiza a soma dos argumentos.

somar <- function(x,y){
  z <- x + y
  print(z)
  return(z)
}

resultado = somar(1,1)
resultado

#Estruturas de dados no R

#Vetores
x <- c(1, 2, 5, 7,10) ;x ;typeof(x)
x <- c(1, 2, 5, 7, 10.5) ;x ;typeof(x)
x <- c(T, F, TRUE, FALSE) ;x ;typeof(x)
x <- c("string 1", "string 2") ;x ;typeof(x)
x <- c("string 1", T,10) ;x ;typeof(x)
  


#Fatores
c5 <- c("M", "F", "F", "F", "M", "M"); c5 ; typeof(c5)
f5 <- as.factor(c5); f5 ; typeof(f5)
f5 <- factor(c5, levels = c("M","F","teste")); f5 ; typeof(f5)
f5[7] <- "M"
f5[7] <- "teste"

f <- factor(c("15", "3", "1", "10"))
typeof(f)

as.numeric(f)

as.numeric(as.character(f))

#Matrizes
matrix(1:12, ncol=3,byrow = T)

#Listas
pessoa <- list(idade=21, nome='Fred', score=c(65,78,55)) 
pessoa
pessoa$idade
pessoa$nome
pessoa$score

# Data frames
df <- data.frame(x = 1:4, 
                 y = c("um", "dois", "tres", "quatro"), 
                 z = T)

df1 <- data.frame(x = 3:4, y = c("s", "s"), z = T)
df2 <- data.frame(x = 1:2, y = c("n", "n"), z = F)
rbind(df1, df2)
cbind(df1, df2)

# Subsetting
x <- c(13, 8, 5, 3, 2, 1, 1) 
x[c(1,2,3)]
order(x,decreasing)
x[order(x)]
x[-c(2, 5,6)]

# Lendo um df em formato excel (dataset reduzido)
install.packages('readxl')
install.packages('data.table')

library(readxl)
library(data.table)
setwd("C:/Users/A750086/OneDrive - Atos/Documents/Documents/Projects/Petala/Aulas/CIMATEC Estat�stica Aplicada com R/dados")
#df <- read_excel("banco_estadual_COVID19_15_07_2020.xlsx")
df <- read_excel("banco_estadual_COVID19_15_07_2020_reduzido.xlsx")
df <- as.data.table(df)
head(df)
str(df)

# Escrevendo arquivo csv
write.csv(head(df), file = 'Covid.csv')
  
# Lendo arquivo csv
read.csv('Covid.csv')

View(head(df,50))

# Dimens�es e Comprimentos
dim(c(1,2,3))
nrow(c(1,2,3))
ncol(c(1,2,3))
length(c(1,2,3))

dim(df)
nrow(df)
ncol(df)
length(df)



# Explorando o conjunto de dados

names(df)
tolower(names(df))

# Redefinindo nomes das colunas (removendo espa�os e caracteres especias)
colnames(df) <- c("data_notificacao","dor_garganta","dispneia","febre","tosse","outros",
                  "profissional_saude","descricao_sintoma","data_inicio_sintomas",
                  "doencas_respiratorias_cronicas_descompensadas","doencas_cardiacas_cronicas",
                  "diabetes","doencas_renais_cronicas","imunossupressao","gestante_alto_risco",
                  "doenca_cromossomica","classificacao_final","investigacao_concluida","estado_teste",
                  "cbo","data_coleta_teste","idade_anos","tipo_teste","resultado_teste","sexo",
                  "estado_residencia","municipio_residencia","raca","epidemia")

str(df)
df <- as.data.table(df)

# Configurando datas
df[ ,data_notificacao := as.Date(data_notificacao,format = "%d/%m/%Y")]
df[ ,.(data_notificacao,data_notificacao)]
str(df)

#Removendo colunas
df[ , data_notificacao_teste := NULL]

df[,data_notificacao := as.Date(data_notificacao,format = "%d/%m/%Y")]
df[,data_inicio_sintomas := as.Date(data_inicio_sintomas,format = "%d/%m/%Y")]
df[,data_coleta_teste := as.Date(data_coleta_teste,format = "%d/%m/%Y")]

# Transformando vari�veis em num�ricas
df[ , idade_anos := as.numeric(idade_anos)]

# Subsetting dataset
df[ , 1:3]
df[1:3, ]

df <- df[estado_residencia == 'BAHIA']

