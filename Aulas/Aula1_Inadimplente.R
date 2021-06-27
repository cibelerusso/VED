# Análise inicial dados_banco.csv
# por Cibele Russo ICMC USP

dados <- read.csv ("https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/dados_banco.csv", header=TRUE)

ggplot(data = dados) +
  geom_boxplot(
    mapping = aes(y = Salario, x=Sexo, fill=Sexo)) 


ggplot(data = dados) +
  geom_boxplot(
    mapping = aes(y = Salario, x=Sexo, fill=Sexo)) 


boxplot(dados$Salario)

attach(dados)

estado_civil

View(dados)
