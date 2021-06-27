## Visualização e Exploração de dados

# Análise dados_banco com plotly
# Por Cibele Russo - ICMC USP

dados <- read.csv ("https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/dados_banco.csv", header=TRUE)

dados <- dados[sample(10000),]


library(plotly)

# Para variáveis qualitativas
# Tabela de frequência, gráfico de barras e gráfico de setores

tab = table(dados$Empresa)

fig <- plot_ly( labels = ~names(tab), values = ~tab, type = 'pie')
fig <- fig %>% layout(title = 'Tipo de empresa')
                      
fig



# Gráfico de barras

library(plotly)

fig <- plot_ly(
  x = names(tab),
  y = tab,
  type = "bar"
)

fig



# Gráfico de barras para tipo de empresa agrupados por sexo

tab <- table(dados$Sexo, dados$Empresa)

Empresa <- colnames(tab)
F <- tab[1,]
M <- tab[2,]
data <- data.frame(Empresa, F, M)

fig <- plot_ly(data,
  x = Empresa,
  y = F,
  name = 'F',
  type = 'bar'
)

fig = fig %>% add_trace(y = ~M, name='M')

fig


# Variáveis quantitativas

library(plotly)
fig <- plot_ly(data=dados, y = ~Salario, type = "box")

fig


# Variáveis quantitativas

library(plotly)
fig <- plot_ly(x = ~Sexo, y = ~Salario, type = "box", data=dados)

fig



# Associação entre variáveis quantitativas e qualitativas

library(plotly)
fig <- plot_ly(x = ~Sexo, y = ~Salario, type = "box", data=dados)

fig



# Associação entre variáveis quantitativas

# Gráfico de dispersão

fig <- plot_ly(data = dados, y = ~Salario, x = ~Idade)

fig




