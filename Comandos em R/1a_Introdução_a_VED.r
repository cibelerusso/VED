# Visualização e Exploração de Dados
# Cibele Russo - ICMC USP


# Leitura dos dados da Companhia MB
# Fonte: Bussab, W.O.; Morettin, P.A. Estatística Básica. 6ª ed., São Paulo: Saraiva, 2009. 


dados <- read.csv("https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/CompanhiaMB.csv", header=TRUE)

View(dados)

df <- data.frame(dados)

# Representação de variáveis qualitativas em R

# Representação tabular
tab = table(df$estado_civil)
tab

tab.rel = table(df$estado_civil)/sum(tab)
tab.rel
round(tab.rel,2)


tab = table(df$instrucao)
tab


tab.rel = table(df$instrucao)/sum(tab)
tab.rel
round(tab.rel,2)


# Gráficos padrões do R

barplot(tab)

barplot(tab, col = c('red','blue','green'))

barplot(tab, col = c('coral','lightblue','lightgreen'),
        main='Gráfico de barras de grau de instrução')

barplot(tab.rel, col = c('coral','lightblue','lightgreen'))


# Gráfico de Pareto

# install('qicharts2')
library(qicharts2)

paretochart(df$instrucao,
            title = 'Gráfico de Pareto de grau de instrução')


# Gráfico de setores (gráfico de pizza)

pie(tab, main='Gráfico de setores para grau de instrução')



# Pacote tidyverse

install.packages('tidyverse')

library(tidyverse)

# Veja: Cheat Sheet de Data Visualizationwith ggplot2
# https://www.rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf


ggplot(data = df) +
       geom_bar(
         mapping = aes(x = estado_civil))


ggplot(data = df) +
  geom_bar(
    mapping = aes(x = estado_civil, fill=estado_civil)) 
    

ggplot(data = df) +
  geom_bar(
    mapping = aes(x = instrucao, fill=instrucao)) 

  
ggplot(data = df) +
  geom_bar(
    mapping = aes( x = regiao, fill=regiao))+
  labs(title='Gráfico de barras de região')

# Fonte: https://www.r-graph-gallery.com/piechart-ggplot2.html

# Basic piechart
ggplot(data = df, aes(x="", y='estado civil', fill=estado_civil)) + 
  geom_bar(stat="identity") +
  coord_polar("y", start=0)+
  labs(title='Gráfico de setores de estado civil')+
    theme_void() # remove background, grid, numeric labels

# Basic piechart
ggplot(data = df, aes(x="", y='instrucao', fill=instrucao, )) + 
  geom_bar(stat="identity") +
  coord_polar("y", start=0)+
  labs(title='Gráfico de setores de grau de instrução')+
  theme_void() # remove background, grid, numeric labels

