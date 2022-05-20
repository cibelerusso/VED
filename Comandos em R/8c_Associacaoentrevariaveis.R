# Associação entre variáveis quantitativas e qualitativas
# Por Cibele Russo
# Dados Companhia MB de Bussab e Morettin

dados <- read.csv ("/home/cibele/Insync/cibele@icmc.usp.br/Google Drive/Disciplinas/!2022 Visualização e Exploração de Dados/Bancos de dados/amostra_banco.csv", header=TRUE)
attach(dados)
dados<-data.frame(dados)


# O salário está associado ao sexo? E ao tipo de empresa?
# Estatísticas por grupos

medias = tapply(dados$Salario, dados$Sexo, mean)
round(medias,2)

desvios = tapply(dados$Salario, dados$Sexo, sd)
round(desvios,2)

variancias = tapply(dados$Salario, dados$Sexo, var)
round(variancias,2)


tapply(Salario, Sexo, summary)


# Ganho na variância
fprov = table(Sexo)
s2barra = weighted.mean(variancias, fprov)
s2 = var(Salario)

R2 = 1 - s2barra / s2

R2

## Representação gráfica

plot(Salario ~ Sexo)


boxplot(Salario~Sexo, col=c('coral','lightgreen'), ylab='Salário', xlab='Sexo')


# Gráfico de médias com desvio padrão
limy = c(0, 1.1 * max(medias + desvios))

gbarras = barplot(medias, xlab = "Sexo", ylab = "Salário", col = "orange", ylim=limy, density=20)
arrows(gbarras, medias, gbarras, medias + desvios, angle = 90) 
arrows(gbarras, medias, gbarras, medias - desvios, angle = 90) 


#install.packages('ggplot2')
library(ggplot2)

# Gráfico de médias, mínimo e máximo

ggplot(data=dados)+
  stat_summary(
    mapping = aes(x = Sexo, y = Salario),
    fun.min = min,
    fun.max = max,
    fun = mean
  )


ggplot(data=dados)+
  stat_summary(
    mapping = aes(x = Sexo, y = Salario),
    fun.min = min,
    fun.max = max,
    fun = median
  )


# Usando o fill no gráfico de violino
ggplot(dados, aes(x=Sexo, y=Salario, fill=Sexo)) + 
  geom_violin(trim=FALSE)

# Usando o fill no gráfico de violino
ggplot(dados, aes(x=Empresa, y=Salario, fill=Empresa)) + 
  geom_violin(trim=FALSE)

# Usando o hue
ggplot(dados, aes(x=Empresa, y=Salario, hue=Sexo, fill=Sexo)) + 
  geom_boxplot()


# Usando o hue
ggplot(dados, aes(x=Sexo, y=Salario, hue=Empresa, fill=Empresa)) + 
  geom_boxplot()


# Associação entre Sexo e Inadimplente 

tab <- table(Sexo, Inadimplente)


n <- sum(tab)
k <- nrow(tab)
m <- ncol(tab)

plot(tab, main='Gráfico de mosaico para os dados observados', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)

chisq.test(tab)

Q2<- chisq.test(tab)$statistic

# Não rejeitamos a independência entre Sexo e Inadimplente pois 0.328 < 3.84 
# Concluimos que não existe associação entre Sexo e Inadimplente.

# Coeficiente de Contingência
C = sqrt(Q2/(Q2+n))
C

# Coeficiente de Tchuprow
T = sqrt(Q2/(n * sqrt((k-1)*(m-1))))
T



# Associação entre Empresa e Inadimplente 

tab <- table(Empresa, Inadimplente)
tab

n <- sum(tab)
k <- nrow(tab)
m <- ncol(tab)

plot(tab, main='Gráfico de mosaico para os dados observados', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)

chisq.test(tab)

qchisq(0.95, 2)


Q2<- chisq.test(tab)$statistic


curve(dchisq(x, 2), 0, 25)

# Como 20.96 >> 5.99, rejeitamos a independência entre Empresa e Inadimplente
# Concluímos que existe associação entre Empresa e Inadimplente

# Coeficiente de Contingência
C = sqrt(Q2/(Q2+n))
C

# Coeficiente de Tchuprow
T = sqrt(Q2/(n * sqrt((k-1)*(m-1))))
T
