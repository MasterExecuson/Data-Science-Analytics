library(data.table)
# Step function
step = data.table(x = c(0,1,2,3,4,5),
                  y = c(0,0.6561,0.9477,0.9963,0.9999,1))
library(ggplot2)
ggplot(step, aes(x,y)) + 
  geom_step()




# Distribui??o Binomial 

n <- 10 #experimentos realizados
p <- 0.1 #probabilidade de um experimento resultar em sucesso
x <- seq(0,n,1) # valores de x =0,1,2,3,..., 10
p_x <- dbinom(x,n,p) #distribui??o dos resultados
p_x



#Qual a probabilidade de que uma pe?a seja defeituosa?
(p_1 <- dbinom(1,n,p))


#Qual a probabilidade de que nenhuma pe?a seja defeituosa?
(p_0 <- dbinom(0,n,p))


# Qual a probabilidade de obtermos no m?nimo duas pe?as defeituosas?
sum(p_x[3:11]) #= P(x=2)+P(X=3)+....+P(X=10)

# Qual a probabilidade de obtermos No m?ximo duas pe?as defeituosas?
p_2 = dbinom(2,n,p)
p_012 = p_0+p_1+p_2

sum(p_x[1:3])

#Conferindo se as probabilidades complementares somam 1
sum(p_x[4:11])+sum(p_x[1:3])
sum(p_x)

# Plotando a distribui??o binomial
library(ggplot2)
a <- data.frame(x,p_x) #cria um dataframe para usar ggplot

ggplot(a, aes(x=x, y=p_x)) + geom_bar(stat="identity",fill="steelblue")+
  ggtitle('Distribui??o Binomial ')+
  geom_text(aes(label=p_x), vjust=1.6, color="white", size=3.5)



# Se as probabilidades fossem mais sim?tricas?
n <- 10 #experimentos realizados
p <- 0.5 #probabilidade de um experimento resultar em sucesso
x <- seq(0,n,1) # valores de x =0,1,2,3,..., 10
p_x <- dbinom(x,n,p) #distribui??o dos resultados
p_x
a <- data.frame(x,p_x) #cria um dataframe para usar ggplot

ggplot(a, aes(x=x, y=p_x)) + geom_bar(stat="identity",fill="steelblue")+
  ggtitle('Distribui??o Binomial')+
  geom_text(aes(label=p_x), vjust=1.6, color="white", size=3.5)






# Se selecionarmos aleatoriamente 10 pessoas que j? tiveram o v?rus, 
#qual a probabilidade do v?rus n?o ter sido transmitido para nenhuma 
#outra pessoa a partir dessas 10 pessoas?

# k =0 n = 10 P = 0,3439 Q = 1 - P = 0,6561

dbinom(0,10,0.3439)
dbinom(10,10,0.3439)
dbinom(2,10,0.3439)
dbinom(3,10,0.3439)





# DISTRIBUI??O NORMAL

library(data.table)
load("covidBAreduzido.RData")

hist(df$idade_anos)

summary(df$idade_anos)


# Para as idades entre os testados pelo covid, qual a probabilidade
# do individuo ter idade acima de 40 anos
P(X>40)

media = mean(df[!is.na(idade_anos)]$idade_anos)
desvio = sd(df[!is.na(idade_anos)]$idade_anos)
#bnorm(k,n,p)

# P(X < 40) 
pnorm(40, mean = media, sd = desvio)

# P(X > 40) 
pnorm(40, mean = media, sd = desvio, lower.tail = F)

1 - pnorm(40, mean = media, sd = desvio)


pnorm(40, mean=media,
      sd=desvio,
      lower.tail=T) 

# Qual a probabilidade do indiv?duo ter entre 30 e 50 anos?

pnorm(30, mean=media,
      sd=desvio,
      lower.tail=FALSE) - 
  pnorm(50, mean=media,
        sd=desvio,
        lower.tail=FALSE)



# Qual a probabiliade de P(30 < X < 50) ou P( 18 < X < 25)

#P(30 < X < 50) + P( 18 < X < 25)

pnorm(30, mean=media,
      sd=desvio,
      lower.tail=FALSE) - 
  pnorm(50, mean=media,
        sd=desvio,
        lower.tail=FALSE) +
  
pnorm(18, mean = media, sd=desvio,lower.tail=F)-
pnorm(25,mean = media, sd=desvio,lower.tail=F)
