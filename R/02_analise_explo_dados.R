### Aula dia 13.02 ###
### ----- Análise exploratória de dados ----- ###

#abrir o data set disponivel dentro do R
data("anscombe")

#explorar as dimensões do conjunto de dados
dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto
str(anscombe) # estrutura do objeto

#selecionando colunas de dados --> fazendo média das colunas com X no nome
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

#agora fazemos a mesma coisa sendo que usando apenas uma linha de comando com a função apply. Tanto para as colunas com x quanto as com y (onde 1 é linha e 2 é coluna)
apply(anscombe[,1:4], 2, mean)
apply(anscombe[,5:8], 2, mean)

#agora fazemos o mesmo para analisar a variancia dos dados dispostos na coluna da tabela
apply(anscombe, 2, var)

#entendendo a correlação e coeficiente de regressão dos conjuntos x e y.
##a correlação entre as variaveis
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

#coeficiente de regressão
##primeiro criamos objetos com as regressoes dos quatro conjunto
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)

## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)

##agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

#os dados são diferentes mas quando olhamos os dados em si vemos que ele é diferente
anscombe
## vamos ver o quão diferentes:
### funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2, 2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las= 1, # deixa as legendas dos eixos na vertical
    bty= "l") # tipo do box do grafico em L

### plotando os gráficos
plot(anscombe$y1 ~ anscombe$x1)
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow=c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

#########   Uma rotina (entre muitas possíveis) de análise exploratória #######

?iris
head(iris)
summary(iris)

#há quantas informações por espécie?
table(iris$Species)

#Qual a média das variáveis por espécie? Vamos usar as funções agreggate e tapply. As duas funções são semelhantes, o que muda são os argumentos e o formato de saída de cada uma delas.
##media do comprimento de sepala por especie
?tapply
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)

##a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)

##ainda a mesma tarefa, com a mesma função mas em uma notação diferente --> o mais facil namoral
aggregate(Sepal.Length ~ Species, data = iris, mean)

##fazendo o mesmo para as outras variáveis
aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)
aggregate(Petal.Width ~ Species, data = iris, mean)

##E agora vamos calcular o desvio padrão das variáveis.
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

##Sempre temos que pensar que tem uma forma de automatizar a situacao. Para isso, vamos usar o comando for e executar todas as tarefas em um mesmo ciclo.
###criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica --> NA é uma matrix vazia que vamos definir depois
medias <- matrix(NA, ncol = 3, nrow = 4)
?matrix

###definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]

###fazendo tudo de uma vez
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}
medias

###### Estatisticas descritivas ######
#Medidas de tendencia central

##media
dim(iris)
head(iris)
vars <- iris[, -5]
apply(vars, 2, mean)

##mediana
apply(vars, 2, median)

##moda
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

#Medidas de dispersão
##variancia

apply(vars, 2, var)

#desvio padrão (raiz quadrada da variancia)
apply(vars, 2, sd)

#Coeficiente de variacao: medida relativa de desvio padrao --> nao tem uma funcao entao criamos
cv <- function(x){
  sd(x)/mean(x)*100
}
apply(vars, 2, cv)

#Quantis e percentis --> e o valor que corta a enésima porcentagem de valores dos dados quando ordenados de forma ascendente. Por padrão, a função quantile retorna o mínimo, o 25º percentil, a mediana, o 50º percentil, o 75º percentil e o máximo, também conhecidos pelo sumário de cinco números proposto por Tuckey (que também é o retorno da função summary de um vetor numérico).

#sumario de cinco numeros
apply(vars, 2, quantile)

#5, 50, 95%
apply(vars, 2, quantile, probs=c(0.05,0.5,0.95))

#Intervalo (range) --> O intervalo é a diferença entre o maior e o menor valor de determinada variável.
#a funcao range os retorna os valores minimo e maximo
apply(vars,2 , range)

#aplicando diff nesse resultado temos o valor que queremos --> fiz caminho diferente do tutorial
my_range <- (apply(vars, 2, range))
diff(my_range)

#diferença intraquartil --> procurar aplicaçoe --  IIQ é a diferença entre o quartil superior (75%) e o quartil inferior (25%).
apply(vars, 2, IQR)

#Correlacao -->Uma matriz de correlação é uma tabela com os valores de correlação entre cada uma das variáveis par a par.
cor(vars)

######## METODOS GRAFICOS ########
##Grafico de barras --> Um grafico de barras mostra a frequência de de observações em uma dada classe.
barplot(table(iris$Species))

##histograma --> O histograma é o equivalente do gráfico de barras para variáveis contínuas. Cada barra representa um intervalo de valores
par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)

par(mfrow=c(2,2))

##vamos ver o efeito do número de intervalos no histograma com o argumento breaks.
par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)

par(mfrow=c(1, 1))

##curva de densidade probabilistica (??)
par(mfrow=c(1, 2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE

par(mfrow=c(1, 1))

#boxplot
par(mfrow=c(2,2))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

## agora vamos olhar os valores por espécie
boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

#identificando outliers
par(mfrow= c(1,1))
boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
#o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
#vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

#No caso anterior consideramos outliers em relação à distribuição da variável para todas as espécies juntas. É razoável assumir que cada espécie tenha um padrão morfométrico distinto de modo que poderíamos identificar outliers de maneira espécie específica.
boxplot(Sepal.Width ~ Species, data = iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2

#o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out

#neste caso, queremos apenas os outliers da especie setosa
#vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species == "setosa",
     c("Sepal.Width", "Species")]

#Entendendo a distribuição dos dados --> Vamos olhar para os dados morfométricos das espécies de Iris e comparar com uma distribuição normal. No R, isto pode ser feito de forma visual com as funções qqnorm e qqline.
par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"],
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])

par(mfrow=c(1,1))

######OBS: O pacote de R GGally fornece uma saída muito interessante para avaliar as relações entre variáveis pois já mostra o valor de correlação entre variáveis e a curva de densidade probabilística de cada uma das variáveis.#######

