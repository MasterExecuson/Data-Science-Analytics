#Simulação jogar a moeda

# toss a coin
(moeda <- c('cara', 'coroa'))

sample(moeda, size = 2,replace=F)

sample(moeda, size = 3, replace = T)

prop.table(table(sample(moeda, size = 10,replace = T)))


# Fazer experimento replicável
set.seed(133)
sample(moeda, size =6, replace = TRUE)


num_flips <- 1000

# flips simulation
(flips <- sample(moeda, size = num_flips, replace = TRUE))

freqs <- table(flips)
freqs


flips <- sample(moeda, size = num_flips, replace = TRUE)
freqs <- table(flips)
freqs


cara_freq <- cumsum(flips == 'cara') / 1:num_flips
head(cara_freq,20)
head(flips,20)

plot(cara_freq,      # vector
     type = 'l',      # line type
     lwd = 2,         # width of line
     col = 'tomato',  # color of line
     las = 1,         # orientation of tick-mark labels
     ylim = c(0, 1),  # range of y-axis
     ylab = "Frequência Relativa")  # y-axis label
abline(h = 0.1, col = 'gray50')
abline(h = 0.5, col = 'gray50')



table(df$tosse)
sum(is.na(df$resultado_teste))

prop.table(table(df$tosse))

tosse <- sample(df[!is.na(tosse)]$tosse, size = 1000, replace=T)
prop.table(table(tosse))

tosse_freq <- cumsum(tosse == 'NAO') / 1:1000



plot(tosse_freq,      # vector
     type = 'l',      # line type
     lwd = 2,         # width of line
     col = 'tomato',  # color of line
     las = 1,         # orientation of tick-mark labels
     ylim = c(0, 1),  # range of y-axis
     ylab = "Frequência Relativa")  # y-axis label
abline(h = 0.6317, col = 'gray50')
abline(h = 0.5, col = 'gray50')

