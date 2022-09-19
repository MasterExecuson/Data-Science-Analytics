library(readxl)
library(data.table)
setwd("C:/Users/A750086/OneDrive - Atos/Documents/Documents/Projects/Petala/Aulas/CIMATEC Estatística Aplicada com R/dados")
df <- as.data.table(read_excel("banco_estadual_COVID19_15_07_2020_reduzido.xlsx"))

# Fazendo procesamento inicial dos dados e salvando dataset trabalhado
colnames(df) <- c("data_notificacao","dor_garganta","dispneia","febre","tosse","outros",
                  "profissional_saude","descricao_sintoma","data_inicio_sintomas",
                  "doencas_respiratorias_cronicas_descompensadas","doencas_cardiacas_cronicas",
                  "diabetes","doencas_renais_cronicas","imunossupressao","gestante_alto_risco",
                  "doenca_cromossomica","classificacao_final","investigacao_concluida","estado_teste",
                  "cbo","data_coleta_teste","idade_anos","tipo_teste","resultado_teste","sexo",
                  "estado_residencia","municipio_residencia","raca","epidemia")

# Configurando datas
df[,data_notificacao := as.Date(data_notificacao,format = "%d/%m/%Y")]
df[,data_inicio_sintomas := as.Date(data_inicio_sintomas,format = "%d/%m/%Y")]
df[,data_coleta_teste := as.Date(data_coleta_teste,format = "%d/%m/%Y")]
df[ , idade_anos := as.numeric(idade_anos)]
df <- df[estado_residencia == 'BAHIA']

df$classificacao_final <- factor(df$classificacao_final)
levels(df$classificacao_final) <- c("Confirmado","Confirmado","Confirmado","Confirmado","Descartado","Descartado","Suspeito")

save(df,file = "covidBAreduzido.RData")
load("covidBAreduzido.RData")

dfpop = data.table(municipio = c("São Paulo","Rio de Janeiro","Salvador","Belo Horizonte","Fortaleza",
                                 "Brasília","Curitiba","Recife","Porto Alegre","Manaus","Belém","Goiânia",
                                 "Guarulhos","Campinas","São Gonçalo","Nova Iguaçu","São Luíz","Maceió",
                                 "Duque de Caxias","São Bernado do Campo","Natal","Teresina","Osasco","Santo André",
                                 "Campo Grande","João Pessoa","Jaboatão","Contagem","São José dos Campos", "Ribeirão Preto"),
           populacao = c(988.8,556.9,224.6,210.9,201.5,187,151.6,135.8,129.8,119.4,116.0,
                         102.6,101.8,92.4,84.7,83.9,80.2,74.7,72.7,68.4,66.8,66.8,63.7,62.8,
                         61.9,56.2,54.1,50.3,49.7,46.3))

head(dfpop)

boxplot(dfpop$populacao)
hist(dfpop$populacao)

# Aplicando transformações
dfpoptransf <- dfpop[ , list(log = log(populacao),
              p25 = populacao^(1/4),
              p50 = populacao^(1/2),
              p33 = populacao^(1/3))]
View(cbind(dfpop[,.(populacao)],dfpoptransf))
par(mfrow=c(2,2))
hist(dfpoptransf$log, main="log", ylab="", xlab="", col="darkgrey", border="white")
hist(dfpoptransf$p25, main="p=1/4", ylab="", xlab="", col="darkgrey", border="white")
hist(dfpoptransf$p50, main="p=1/2", ylab="", xlab="", col="darkgrey", border="white")
hist(dfpoptransf$p33, main="p=1/3", ylab="", xlab="", col="darkgrey", border="white")

#Avaliando simetria dos dados
boxplot(df$idade_anos)
hist(df$idade_anos)

names(df)
table(df$febre)
table(df$febre,df$classificacao_final)

#Removendo valor ignorado da variável FEBRE
df <- df[!febre %in% "IGNORADA"]

# Calculando tabela de frequência bidimensional
table(df$febre)
(tabela <- table(df$febre,df$classificacao_final))

# Calculando totais marginais
(Total_linha<-margin.table(tabela,2))  # O argumento 2 define a marginal da coluna
(Total_coluna<-margin.table(tabela,1)) # O argumento 1 define a marginal da linha
tabFinal<-rbind(cbind(tabela,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tabFinal)[[1]][3]<-"Total_linha" 
tabFinal

# calculando percentuais na tabela de frequência bidimensional
(tabela <- round(prop.table(table(df$febre,df$classificacao_final)),4)*100)

# Calculando totais marginais
(Total_linha<-margin.table(tabela,2))  # O argumento 2 define a marginal da linha
(Total_coluna<-margin.table(tabela,1)) # O argumento 1 define a marginal da coluna
tabFinal<-rbind(cbind(tabela,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tabFinal)[[1]][3]<-"Total_linha" 
tabFinal

# Calculando totais na tabela de frequência bidimensional por margens
(tabela <- round(prop.table(table(df$febre,df$classificacao_final),2),4)*100) # Por linha
(tabela <- round(prop.table(table(df$febre,df$classificacao_final),1),4)*100) # Por coluna


# Análise gráfica bidimensional para duas variáveis qualitativas
library(ggplot2)
library(scales)
library(reshape2)
# dataset de entrada do gráfico deve conter três colunas: categoria1, categoria2 e frequência

df1 <- df[!is.na(febre),.N, by = list(febre,classificacao_final)]

ggplot(df1 , aes(x = febre, y = N, fill = classificacao_final))+
  geom_bar(stat="identity", position = "stack")
  
ggplot(df1 , aes(x = febre, y = N, fill = classificacao_final))+
  geom_bar(stat="identity",position = "dodge")

ggplot(df1 , aes(x = febre, y = N, fill = classificacao_final))+
  geom_bar(stat="identity",position = "fill")

ggplot(df1 , aes(x = classificacao_final, y = N, fill = febre))+
  geom_bar(stat="identity",position = "fill")


# Análise bivariada para duas variáveis quantitativas
temp<-data.table(agente=c("A","B","C","D","E","F","G","H","I","J"),
                        anos_servico=c(2,3,4,5,4,6,7,8,8,10),
                        n_clientes=c(48,50,56,52,43,60,62,58,64,72))

ggplot(temp, aes(x=anos_servico, y=n_clientes)) +
  geom_point()


# 
obitos <- read_excel("banco_estadual_COVID19_15_07_2020_reduzido.xlsx",sheet='obitos')
head(obitos)
obitos <- as.data.table(obitos)

obitos[ , dias_notificacao := difftime(`DATA NOTIFICACAO OBITO`, `DATA OBITO`, units = "days")]

ggplot(obitos, aes(x=IDADE, y=dias_notificacao)) +
  geom_point(alpha = 0.3)

# Melhorando a visualização
ggplot(obitos, aes(x=IDADE, y=dias_notificacao)) +
  geom_point()

cor(obitos$IDADE,as.numeric(obitos$dias_notificacao))


# Análise bivariada entre uma variável qualitativa e uma variável quantitativa

sum(is.na(df$diabetes))
sum(is.na(df$idade_anos))

library(knitr)
kable(df[!is.na(diabetes) &
           !is.na(idade_anos) &
           !diabetes %in% "IGNORADA",
   list(mean=mean(idade_anos,na.rm=T),
        median = median(idade_anos,na.rm=T),
        sd=sd(idade_anos,na.rm=T),
        min = min(idade_anos,na.rm=T),
        max = max(idade_anos,na.rm=T),
        q1 = quantile(idade_anos,0.25,na.rm=T),
        q2 = quantile(idade_anos,0.50,na.rm=T),
        q3 = quantile(idade_anos,0.75,na.rm=T),
        n = .N),
   by=c(diabetes)])

# Boxplot
ggplot(df[!is.na(diabetes) & !is.na(idade_anos) & !diabetes %in% "IGNORADA"] , 
       aes(x = idade_anos, y = diabetes)) +
  geom_boxplot()


ggplot(df[!is.na(raca) & !is.na(idade_anos) & !raca %in% "IGNORADA"] , 
       aes(x = idade_anos, y = raca)) +
  geom_boxplot()


# Comparando variâncias 

(varglobal <- var(df$idade_anos,na.rm=T))
(varsim <- var(df[diabetes %in% "SIM"]$idade_anos,na.rm=T))
(varnao <- var(df[diabetes %in% "NAO"]$idade_anos,na.rm=T))
mean(c(varsim,varnao))

(varponderada <- (varsim*nrow(df[diabetes %in% "SIM"]) + 
    varnao*nrow(df[diabetes %in% "NAO"]))/(nrow(df[!is.na(diabetes)])))

(r2 <- (varglobal-varponderada)/varglobal)

#rm(list=ls())
#load(url("http://ime.usp.br/~pam/dados.RData"))
