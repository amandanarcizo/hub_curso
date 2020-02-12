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

## Juntando diferentes tabelas por meio de identificadores comuns
envir$Sites
summary(envir$Sites)

#modificando tipos de variaveis --> sites ainda esta sendo entendida como uma variavel numerica e precisa ser transformada em variável categorica porque é o nome das áreas. Vamos confirmar como ele entende essa variável:
class(envir$Sites)

#interger significa que é um numero inteiro e não decimal, ou seja, variavel numérica e temos que transformar em categorica

as.factor(envir$Sites)

#se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
envir$Sites <- as.factor(envir$Sites)
coord$Sites <- as.factor(coord$Sites)

#Juntando envir e coord
envir.cord <- merge(x = envir, y = coord, by = "Sites")
dim(envir)
dim(coord)
dim(envir.cord)

## Transformando uma matrix espécie vs. área em uma tabela de dados
#Agora, queremos transformar a nossa matriz de espécie vs. área em uma planilha que contenha cada observação em uma linha e cada variável em uma coluna. Cada observação é a abundância de uma espécie em uma determinada área. Para fazer essa transformação iremos usar a função gather() do pacote tidyr. Como temos 97 sites e 56 espécies, terminaremos com um objeto com 5432 linhas

Sites <- envir$Sites
length(Sites)

n.sp <- nrow(splist)
n.sp

#criando tabela com cada especie em cada area especies em linhas -> comm é uma matrix de especies em colunas e sites em linhas e a função gather do pacote tidyr faz essa transposição retirando a primeira coluna que é o nome dos sites
comm.df <- tidyr::gather(comm[, -1])
dim(comm.df)
head(comm.df)

#Queremos alterar o nome das colunas de comm.df. Para isso, usaremos a função colnames().

colnames(comm.df)
colnames(comm.df) <-  c("TaxCode", "Abundance")
colnames(comm.df)

#Queremos agora adicionar a coluna Sites ao novo objeto. Vamos usar a função rep(). Esta função cria sequências. Vamos criar uma sequência de localidades, em que cada uma das 97 localidades se repete 56 vezes. A sequência deve ter também 5432 elementos

seq.site <- rep(Sites, times = n.sp)
seq.site
length(seq.site)

#agora sinalizamos que a coluna sites de comm.df é igual a seq.site
comm.df$Sites <- seq.site
head(comm.df)

## Juntando todas as variáveis com comm.df
#Como vimos na aula, as relações entre duas tabelas são sempre feitas par a par. Então, vamos juntar par a par as tabelas usando a função merge().

comm.sp <- merge(x =comm.df, y =splist, by= "TaxCode")
head(comm.sp)

#juntando com traits --> adicionamos os dados de atributos das espécies à tabela de comunidade. Na tabela traits, a coluna que identifica as espécies é chamada Sp. Antes de fazer a junção, precisamos mudar o nome para bater com o nome da coluna em comm.sp que é TaxCode.

names(traits)
colnames(traits) [1] <- "TaxCode"

comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)

## Finalmente, juntamos as variáveis ambientais (que aqui já contém as coordenadas) à tabela geral da comunidade por meio da coluna Sites.

comm.total <- merge(comm.traits, envir.cord, by = "Sites")
head(comm.total)

## Por último, finalizamos nossa rotina de manipulação de dados exportando a planilha final modificada. Para isso, usamos a função write.csv().

write.csv(x = comm.total,
         file = "data/01_data_format_combined.csv",
         row.names = FALSE)

####----------------------------------- FIM DESSA BAGACEIRA --------------


