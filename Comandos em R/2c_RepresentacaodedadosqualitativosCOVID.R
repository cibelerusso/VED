# Visualização e Exploração de Dados
# Cibele Russo - ICMC USP


### Exemplo: Dados de COVID-19 em cidades brasileiras
# Os dados em covid_cidades.csv referem-se aos registros de casos e mortes em cidades
# brasileiras no dia 29-12-2020. Desenvolva a visualização de dados de mortes registradas 
# por estado, utilizando gráficos de setores, de barras e de Pareto. 
# Visualização e Exploração de Dados
# Cibele Russo - ICMC USP

# Analise a correlação entre casos e mortes registradas.
# Fonte: Brasil.IO: boletins epidemiológicos da COVID-19 por município por dia, disponível em: (https://brasil.io/datasets/). 

#install.packages('tidyverse')
library(tidyverse)

dados_cidades <- read.csv('https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/covid_cidades.csv', header=TRUE)

names(dados_cidades)


View(dados_cidades)


# Agrupando os dados por estado

# group_by

dados <- dados_cidades %>% 
  group_by(state) %>% 
  summarise(confirmed = sum(confirmed),
            deaths = sum(deaths))


# Representação tabular

# Por que nesse caso não é necessário usar table?

tab <- dados$deaths
names(tab) <- levels(dados$state)
tab

tab.rel <- dados$deaths / sum(tab)
names(tab.rel) <- levels(dados$state)
tab.rel

# Gráficos padrões do R

barplot(tab, col='lightblue')

barplot(tab.rel, col='lightblue')


# Gráfico de Pareto

# install('qicharts2')
library(qicharts2)

x <- rep(dados$state, dados$deaths)

paretochart(x,
            title = 'Gráfico de Pareto para mortes')



# Usando o pacote plotly

#install.packages('plotly')
library(plotly)

# Para variáveis qualitativas
# Por que nesse caso não precisamos obter a tabela de frequências?

fig <- plot_ly(dados, labels = ~state, values = ~deaths, type = 'pie')
fig <- fig %>% layout(title = 'Gráfico de setores para mortes acumuladas nos estados até 29/12/2020')

fig

# Gráfico de barras de mortes

fig <- plot_ly(dados,
               x = ~state,
               y = ~deaths,
               name = 'mortes',
               type = 'bar')
fig


# Gráfico de barras de confirmados

fig <- plot_ly(dados,
               x = ~state,
               y = ~confirmed,
               name = 'confirmados',
               type = 'bar')
fig



# Refaça os gráficos com estados onde o número de mortes é maior que 9 mil

# filter

dados_mais9milmortes <- dados %>%
  filter(
    deaths > 9000
  )

dados_mais9milmortes



fig <- plot_ly(dados_mais9milmortes, labels = ~state, values = ~deaths, type = 'pie')
fig <- fig %>% layout(title = 'Gráfico de setores para mortes em estados com mais de 9 mil mortes em 29/12/2020')

fig


# Exercício: Criar as regiões usando o tibble

dados = dados %>% 
  mutate(regiao = case_when(
    state %in% c("RS","PR","SC") ~ 'sul',
    state %in% c("RJ","SP","MG","ES") ~ 'sudeste',
    state %in% c("GO","MT","MS","DF") ~ 'centro-oeste',
    state %in% c("CE","PI","BA","MA","RN","PB","PE","SE","AL") ~ 'nordeste',
    state %in% c("AM", "AC", "PA", "RO", "RR", "AP", "TO") ~ 'norte'))


# Outra forma de criar as regiões

dados$regiao = "norte"
dados$regiao[dados$state %in% c("RS","PR","SC")] = 'sul'
dados$regiao[dados$state %in% c("RJ","SP","MG","ES")] = "sudeste"
dados$regiao[dados$state %in% c("CE","PI","BA","MA","RN","PB","PE","SE","AL")] = 'nordeste'
dados$regiao[dados$state %in% c("GO","MT","MS","DF")] = "centro-oeste"

order(dados$regiao)

dados = dados[order(dados$regiao),]

dados[1,]


dados[10,]


# Filtrando dados apenas da região sudeste

dados_sudeste <- dados %>%
  filter(
    regiao == 'sudeste'
  )


fig <- plot_ly(dados_sudeste, labels = ~state, values = ~deaths, type = 'pie')
fig <- fig %>% layout(title = 'Gráfico de setores para mortes na região sudeste')

fig



# Leitura adicional:

# http://sillasgonzaga.com/material/curso_visualizacao/ggplot2-parte-ii.html#graficos-de-barras-ou-colunas

