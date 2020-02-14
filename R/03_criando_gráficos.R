########### Aula 13.02 --- Criando e salvando gráficos no R ##############
###Explorando o dataset
##Carregue o arquivo contendo os dados ambientais de cada localidade da aula de ontem e os dados de comunidade (envir e comm).

##Vamos extrair a riqueza de espécies por Site e plotar a riqueza de espécies em função do teor de argila (clay), silte (silt) e areia (sand) do solo.

comm <- read.csv("data/cestes/comm.csv")
envir <- read.csv("data/cestes/envir.csv")

head(comm)
summary(comm)

head(envir)
summary(envir)

#Para extrair a riqueza por Site, lembremos da aula de ontem:
comm.pa <- comm[, -1] > 0
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
head(rich)

summary(rich)
boxplot(rich)
#altera o eixo y
boxplot(rich, las=1)

#Criando uma nova tabela com a coluna de riqueza
localidades <- cbind(envir, rich)
head(localidades)

#Grafico de dispersao
# criando modelos lineares
riqsilt <- lm(rich ~ Silt, data = localidades)
riqclay <- lm(rich ~ Clay, data = localidades)
riqsand <- lm(rich ~ Sand, data = localidades)

# extraindo os coeficientes do modelo
coef_s <- coef(riqsilt)
coef_c <- coef(riqclay)
coef_d <- coef(riqsand)

# definindo os limites dos eixos
limy <- c(min(localidades$rich),
          max(localidades$rich))
limx <- c(min(localidades[,c("Clay", "Sand", "Silt")]),
          max(localidades[,c("Clay", "Sand", "Silt")]))

## definindo o nome do eixo y
laby <- "Riqueza de espécies"

# aqui estamos usando las e bty dentro do par para fixar para todas as janelas
par(mfrow = c(1, 3),
    las = 1,
    bty = "l")

# plot da riqueza em função do teor de Silte
plot(rich ~ Silt, data = localidades,
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby,
     xlab = "Teor de Silte (%)")
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_s[1], b = coef_s[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot da riqueza em função do teor de Argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)

## plot da riqueza em função do teor de Areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)

####Exportando o gráfico com as funções png() e dev.off()####
# a funcao png cria o arquivo, daqui pra frente você não vai mais ver o gráfico
# temos sempre que usar png - tudo que fizemos pra fazer o grafico - devoff()
png("figs/figura01.png", res = 300, width = 2400, height = 1200)
# define parametros graficos

par(mfrow = c(1, 3),
    las = 1,
    bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot da riqueza em função do teor de Silte
plot(rich ~ Silt, data = localidades,
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby,
     xlab = "Teor de Silte (%)")
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_s[1], b = coef_s[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot da riqueza em função do teor de Argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)

## plot da riqueza em função do teor de Areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)
# para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
dev.off()

#BOXPLOT
##usando cores para criar um boxplot de cada cor --> box-plots da riqueza para cada especie e variavel com cada especie de uma cor

##OBS: colocar o nome das especies em italico no eixo -> Isto e possivel com o argumento font da função axis. As funções text e mtext tambem tem a opção font

#criando vetor de cores
cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")

#criando vetor com o nome das especies
?paste
sp <- paste("I.", unique(iris$Species), sep = " ")

#mfrow -> quantidade de quadrantes no plots/ mar -> n de linhas da margem/ bty=l -> tipo de margem do gráfico em L / las=1 -> vira a legenda do eixo y
par(mfrow = c(2, 2),
    mar = c(4, 1, 1, 1),
    bty = 'l',
    las = 1)

#Fazer um boxplot de sepal.length (y) em funcao de species (x), data=iris -> da tabela iris, xlab -> deixando o eixo x sem label, col=cores -> o default é sem cor entao indica que a cor é da paleta de cores do objeto cores, xatx = remover os numeros do eixo x
boxplot(Sepal.Length ~ Species,
        data = iris,
        xlab = "",
        col = cores,
        xaxt = "n")

#alterando o label de um eixo, 1-> x, at= de quanto a quantos, labels = objeto sp que é o nome das espécies, font=3 -> tamanho da fonte
axis(1, at = 1:3, labels = sp, font = 3)

boxplot(Sepal.Width ~ Species,
        data = iris,
        xlab = "",
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Petal.Length ~ Species, data = iris,  col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)
boxplot(Petal.Width ~ Species,
        data = iris,
        col = cores,
        xaxt = "n")
axis(1, at = 1:3, labels = sp, font = 3)

par(mfrow = c(1,1))

#Grafico de media com desvio padrao com arrows
#Vamos criar um data.frame com a média e desvio padrão de cinco variáveis. Atenção: estamos usando a função set.seed para que os valores gerados com a função sample sejam iguais para todxs os computadores.

## fixando uma semente de numeros aleatorios para manter o mesmo resultado no sample
set.seed(42)

# criando um data frame/matriz/tabela com valores medios e desvio padrao de uma variavel, ele nomeia e coloca os numeros por coluna
d2 <- data.frame(name = letters[1:5],
                 value = sample(seq(4, 15), 5),
                 sd = c(1, 0.2, 3, 2, 4))

#Fazer o plot dos pontos/ Adicionar a configuracao do eixo x na mao com a funcao axis
par(mfrow = c(1,1))
plot(x = 1:5, d2$value, las = 1, bty = 'l', pch = 19, xaxt = 'n',
     xlab = "names", ylab = "value")
axis(1, at = 1:5, labels = d2$name)

#Adicionar os valores de desvio padrão em torno da média com a função arrows
#y0 = de onde partir o desenho, y1= até onde ele vai, angle=90 -> angulo da linha saindo do ponto, code=3 -> faz a barrinha no final da linha de dp
arrows(x0 = 1:5,
       y0 = d2$value + d2$sd,
       y1 = d2$value - d2$sd, angle = 90, length = 0.05, code = 3)


