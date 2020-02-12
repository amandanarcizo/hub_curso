#script para ler a tabela limpa
#os dados originais estão em um xlsx

#Nesse caso o csv vindo do excel em portugues é bom ou usar read.csv2 ou especificar que o separador (sep) é ; e que os decimais tão separados por , dai ele coloca tudo pra ponto
read.csv("./data/ex04_clean.csv", sep = ";", dec = ",")

#Agora vamos criar um objeto com a tabela
ex04 <- read.csv("./data/ex04_clean.csv", sep = ";", dec = ",")

#Colocamos git add R(pasta onde ta o arquivo)/"Nome do arquivo de script" lá no terminal e assim o git começa a monitorar esse arquivo
#Fizemos commit do arquivo de script e planilha limpa o git commit -m "entre parenteses colocamos uma mensagem"
