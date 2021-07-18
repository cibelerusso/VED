#### Objetivo: Pirâmide etária e impacto da covid no estado de SP
# Códigos originais por Flaviane Louzeiro da Silva - Grupo Predict
# Visualização e Exploração de Dados
# Cibele Russo ICMC USP

#### carregando os pacotes
library(scales)
library(sidrar)
library(ggplot2)
library(plyr)
library(tidyverse)


# Leitura dos dados de frequências consolidados previamente

dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/SP_pop_cov.csv')


### Piramide etária - ainda com erro nos níveis do GrupoDeIdade

ggplot(dados, aes(y = Valor, x = GrupoDeIdade))+ 
  geom_bar(data = subset(dados, Sexo == "Mulheres" & info == "IBGE"), stat = "identity", fill = '#FF00FF')+ 
  geom_bar(data = subset(dados, Sexo == "Homens" & info == "IBGE"), stat = "identity", fill = '#0000FF')+
  coord_flip()+labs(x='Grupo de Idade', y='Total')+ 
  ggtitle(paste0('População Total - SP \nInicio:', '2020-02-04', '\nFim:', '2021-07-15'),  '\n Fonte: SEADE' ) 

# Redefinindo a ordem dos níveis de GrupoDeIdade
ordem <- c("0 a 4 anos", "5 a 9 anos","10 a 14 anos","15 a 19 anos",    
"20 a 24 anos","25 a 29 anos","30 a 34 anos","35 a 39 anos",    
"40 a 44 anos","45 a 49 anos","50 a 54 anos",    
"55 a 59 anos","60 a 64 anos","65 a 69 anos","70 a 74 anos",    
"75 a 79 anos","80 a 84 anos","85 a 89 anos","90 a 94 anos",    
"95 a 99 anos", "100 anos ou mais" )


dados$GrupoDeIdade <- factor(dados$GrupoDeIdade, levels=ordem)


# Gráfico de pirâmide corrigido
ggplot(dados, aes(y = Valor, x = GrupoDeIdade))+ 
  geom_bar(data = subset(dados, Sexo == "Mulheres" & info == "IBGE"), stat = "identity", fill = '#FF00FF')+ 
  geom_bar(data = subset(dados, Sexo == "Homens" & info == "IBGE"), stat = "identity", fill = '#0000FF')+
  coord_flip()+labs(x='Grupo de Idade', y='Total')+ 
  ggtitle(paste0('População Total - SP \nInicio:', '2020-02-04', '\nFim:', '2021-07-15'),  '\n Fonte: SEADE' ) 



### Piramide de óbitos
ggplot(dados, aes(y = Valor, x = GrupoDeIdade))+ 
  geom_bar(data = subset(dados, Sexo == "Mulheres" & info == "Covid"), stat = "identity", fill = '#800080')+ 
  geom_bar(data = subset(dados, Sexo == "Homens" & info == "Covid"), stat = "identity", fill = '#191970')+
  coord_flip() + labs(x='Grupo de Idade', y='Total')+ 
  ggtitle(paste0('Nº de óbitos - SP \nInicio:', '2020-02-04', '\nFim:', '2021-07-15'),  '\n Fonte: SEADE' ) 



### Piramide populacional e de óbitos por COVID-19
ggplot(dados, aes(y = Valor, x = GrupoDeIdade))+ 
  geom_bar(data = subset(dados, Sexo == "Mulheres" & info == "IBGE"), stat = "identity", fill = '#FF00FF')+ 
  geom_bar(data = subset(dados, Sexo == "Mulheres" & info == "Covid"), stat = "identity", fill = '#800080')+ 
  geom_bar(data = subset(dados, Sexo == "Homens" & info == "IBGE"), stat = "identity", fill = '#0000FF')+
  geom_bar(data = subset(dados, Sexo == "Homens" & info == "Covid"), stat = "identity", fill = '#191970')+
  scale_y_continuous(labels = paste0(as.character(c(seq(2, 0, -1), seq(1, 2, 1))), "m")) + 
  coord_flip() + labs(x='Grupo de Idade', y='Total')+ 
  ggtitle(paste0('População Total versus Nº de óbitos - SP \nInicio:', '2020-02-04', '\nFim:', '2021-07-15'),  '\n Fonte: SEADE' ) 


