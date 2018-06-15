#Universidade federal de minas gerais
#William Giani Duani Martins
#_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_

# importing libraries and a dataset
require(dplyr)
require(tidyverse)
dados <- read.csv2("https://pastebin.com/raw/Pxi5p3Ap", header=T)

# cleaning some values
dados <- dados[1:4,1:2]

# doing the required combinations
combinations <- combn(c(1:4),2)
combinations

# creating some auxiliary vectors
int <- c()
inc <- c()
pesos <- c()

# calculating all possible regressions
for(i in 1:6){
  y <- rbind(dados[combinations[,i][1],][1],dados[combinations[,i][2],][1]) %>% as.matrix()
  x <- cbind(c(1,1), rbind(dados[combinations[,i][1],][2],dados[combinations[,i][2],][2])) %>% as.matrix()
  pesos <- rbind(pesos, abs(dados[combinations[,i][1],][2]-dados[combinations[,i][2],][2]))
  int <- rbind(int, solve(x,y)[1])
  inc <- rbind(inc, solve(x,y)[2])
}

# creating an auxiliary table
tabelaAuxiliar <- c()
tabelaAuxiliar <- cbind(int,inc,pesos)
tabelaAuxiliar

# converting the distances in weights
tabelaAuxiliarConvertida <- tabelaAuxiliar %>% arrange(inc) 
tabelaAuxiliarConvertida <- cbind(tabelaAuxiliarConvertida, cumsum(pesos/sum(pesos)))
colnames(tabelaAuxiliarConvertida) <- c("int","inc","distancia","pesoacum")

# extracting the median
filter(tabelaAuxiliarConvertida, pesoacum>0.5)[1,]
beta1 <- filter(tabelaAuxiliarConvertida, pesoacum>0.5)[1,2]

# calculating all betas0
betas0 <- c()
for(i in 1:4){
  betas0 <- rbind(betas0, dados[i,][1] - beta1 * dados[i,][2])
}

# extracting betas0 median
betas0 <- betas0 %>% arrange(y)
beta0 <- round(median(betas0$y), 2)

# final results
beta0
beta1
