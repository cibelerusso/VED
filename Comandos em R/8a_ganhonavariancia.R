# Associação entre variáveis qualitativas
# Por Cibele Russo
# Dados Companhia MB de Bussab e Morettin

dados <- read.csv ("https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/CompanhiaMB.csv", header=TRUE)
attach(dados)
dados<-data.frame(dados)


# O salário está associado ao grau de instrução?
# Estatísticas por grupos

medias = tapply(dados$salario, dados$instrucao, mean)
round(medias,2)

desvios = tapply(dados$salario, dados$instrucao, sd)
round(desvios,2)

variancias = tapply(dados$salario, dados$instrucao, var)
round(variancias,2)


tapply(salario, instrucao, summary)


# Ganho na variância
fprov = table(instrucao)
s2barra = weighted.mean(variancias, fprov)
s2 = var(salario)

R2 = 1 - s2barra / s2

R2

## Representação gráfica

plot(salario ~ instrucao)

levels(instrucao) = c('ensino fundamental', 'ensino médio', 'ensino superior')
plot(salario~instrucao)


plot(salario~instrucao, ylab='salário', xlab='grau de instrução')


boxplot(salario~instrucao, col=c('coral','lightblue','lightgreen'), ylab='salário', xlab='grau de instrução')



# Gráfico de médias com desvio padrão
limy = c(0, 1.1 * max(medias + desvios))

gbarras = barplot(medias, xlab = "grau de instrução", ylab = "salário", col = "orange", ylim=limy, density=20)
gbarras

arrows(gbarras, medias, gbarras, medias + desvios, angle = 90) 

arrows(gbarras, medias, gbarras, medias - desvios, angle = 90) 





# Gráfico de médias com desvio padrão
limy = c(0, 1.1 * max(medias + desvios))

plot(medias, xlab = "grau de instrução", ylab = "salário",  ylim=limy, pch=16, type='b', xaxt='n')

arrows(c(1,2,3), medias, c(1,2,3), medias + desvios, angle = 90, col='blue') 
arrows(c(1,2,3), medias, c(1,2,3), medias - desvios, angle = 90, col='blue') 

text(seq(1, 3, by=1), labels = levels(instrucao),  pos = 1,  par("usr")[3] - 0.2, xpd = TRUE)


#install.packages('ggplot2')
library(ggplot2)

# Gráfico de médias, mínimo e máximo

ggplot(data=dados)+
  stat_summary(
    mapping = aes(x = instrucao, y = salario),
    fun.min = min,
    fun.max = max,
    fun = mean
  )



ggplot(data=dados)+
  stat_summary(
    mapping = aes(x = instrucao, y = salario),
    fun.min = min,
    fun.max = max,
    fun = median
  )




# Basic violin plot
p <- ggplot(dados, aes(x=instrucao, y=salario)) + 
  geom_violin(trim=FALSE)
p

# Rotate the violin plot
p + coord_flip()

# Boxplots
ggplot(dados, aes(x=instrucao, y=salario)) + 
  geom_boxplot( fill=c('coral'))

# Usando o fill
ggplot(dados, aes(x=instrucao, y=salario, fill=instrucao)) + 
  geom_violin(trim=FALSE)

# Usando o hue
ggplot(dados, aes(x=regiao, y=salario, hue=instrucao, fill=instrucao)) + 
  geom_boxplot()



# Leitura complementar

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# https://rcompanion.org/handbook/C_04.html

# http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization

