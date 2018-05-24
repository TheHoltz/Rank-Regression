#Universidade federal de minas gerais
#William Giani Duani Martins

# importei
require(dplyr)
require(tidyverse)
dados <- read.csv2("https://pastebin.com/raw/Pxi5p3Ap", header=T)

# limpei
dados <- dados[1:4,1:2]

combinations <- combn(c(1:4),2)
combinations

int <- c()
inc <- c()
pesos <- c()

# Calculando todas as possíveis regressões
for(i in 1:6){
  y <- rbind(dados[combinations[,i][1],][1],dados[combinations[,i][2],][1]) %>% as.matrix()
  x <- cbind(c(1,1), rbind(dados[combinations[,i][1],][2],dados[combinations[,i][2],][2])) %>% as.matrix()
  pesos <- rbind(pesos, abs(dados[combinations[,i][1],][2]-dados[combinations[,i][2],][2]))
  int <- rbind(int, solve(x,y)[1])
  inc <- rbind(inc, solve(x,y)[2])
}

# montando tabela auxiliar
tabelaAuxiliar <- c()
tabelaAuxiliar <- cbind(int,inc,pesos)
tabelaAuxiliar

# convertendo distancias em pesos
tabelaAuxiliarConvertida <- tabelaAuxiliar %>% arrange(inc) 
tabelaAuxiliarConvertida <- cbind(tabelaAuxiliarConvertida, cumsum(pesos/sum(pesos)))
colnames(tabelaAuxiliarConvertida) <- c("int","inc","distancia","pesoacum")

#pegando a mediana
filter(tabelaAuxiliarConvertida, pesoacum>0.5)[1,]
beta1 <- filter(tabelaAuxiliarConvertida, pesoacum>0.5)[1,2]


# calculando betas0
betas0 <- c()
for(i in 1:4){
  betas0 <- rbind(betas0, dados[i,][1] - beta1 * dados[i,][2])
}

# mediana dos betas0
betas0 <- betas0 %>% arrange(y)
beta0 <- round(median(betas0$y), 2)

# resultados
beta0
beta1
