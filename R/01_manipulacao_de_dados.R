# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#-----------------------------------------------------------#
# primeiro instalamos todos os pacotes necessarios para o processo
# ajeitamos no terminal usando script --> BASH.R para dentro da pasta cestes e começamos (Na real não precisa pq o caminho esta na criação do objeto)

library(tidyr)

# criamos objeto listando os arquivos que tem na pasta

files.path <- list.files(path = "data/cestes", pattern = ".csv", full.names = TRUE)

files.path

#Agora temos os endereços de cada arquivo csv e podemos criar os objetos mais tranquilamente. Há também Há maneiras mais automatizadas de fazer a mesma tarefa cinco vezes! No arquivo .pdf da aula tem dois exemplos usando lapply e for para ler todas as planilhas uma única vez.

comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])

#Entendendo cada um dos objetos/planilhas vamos usar as funções head, dim e summary para inspecionar cada arquivo
head(comm)
dim(comm)
summary(comm)

head(coord)
dim(coord)
summary(coord)

head(envir)
dim(envir)
summary(envir)

head(splist)
dim(splist)
summary(splist)

head(traits)
dim(traits)
summary(traits)

### Fazendo sumário dos dados das planilhas
# Quantas espécies temos?

nrow(splist)

#Quantas áreas foram amostradas?

nrow(comm)

#Quantas variáveis ambientais? pede o nome das colunas menos a primeira coluna que é Sites

names(envir[-1])
length(names(envir[-1]))

#Qual a riqueza de cada área? Primeiro, precisamos transformar a nossa matriz que possui dados de abundância em uma matriz de presença e ausência.

#Queremos criar uma matriz com todas as linhas, e todas as colunas menos a primeira e perguntamos a ele quem é maior que 0 para ele marcar como True
comm.pa <- comm[,-1] > 0
row.names(comm.pa) <- envir$Sites

#ver a riqueza da primeira linha, quantos True tem na primeira linha, na
summary(comm.pa[1,])

#aplique (função apply) às linhas da tabela (MARGIN=1) a função soma (FUN=SUM) --> fazemos assim o passo anterior para todos os sites
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
summary(rich)

#saber se tem algum site com riqueza igual a 1
sum(rich == 1)

#para saber qual site que tem riqueza igual a 1
which(rich == 1)
