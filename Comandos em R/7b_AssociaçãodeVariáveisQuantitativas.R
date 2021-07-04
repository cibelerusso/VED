# Visualização e Exploração de Dados
# Cibele Russo - ICMC USP

# Associação entre variáveis qualitativas


# Exemplo diabetes x óbitos por COVID-19

tab <- matrix(c(497,248,256,181), byrow=T, ncol=2)
tab

colnames(tab)<-c('óbito=0','óbito=1')
rownames(tab)<-c('diabetes=0','diabetes=1')

mosaicplot(tab,col=c('aquamarine', 'lightblue'), cex=1.1)
mosaicplot(t(tab),col=c('aquamarine', 'lightblue'), cex=1.1)


Q2 <- chisq.test(tab)
Q2
Q2<- Q2$statistic

k <- nrow(tab)
m <- ncol(tab)
n = sum(tab)


Q2 = (497-474.6)^2/474.6 + (256-278.4)^2/278.4  + (248-270.4)^2/270.4  + (181-158.6)^2/158.6
Q2


# Quantil qui-quadrado
qchisq(0.95, (k-1)*(m-1))

curve(dchisq(x,(k-1)*(m-1)), 0, 10)

curve(dchisq(x, 1), 0, 10)



# Coeficiente de Contingência
C = sqrt(Q2/(Q2+n))
C

# Coeficiente de Tchuprow
T = sqrt(Q2/(n * sqrt((k-1)*(m-1))))
T





dados <- read.csv ("https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/CompanhiaMB.csv", header=TRUE)
attach(dados)
dados<-data.frame(dados)


tab <- table(instrucao, estado_civil)


plot(tab, main='Gráfico de mosaico para os dados observados', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)

plot(t(tab), main='Gráfico de mosaico para os dados observados', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)


barplot(tab, col = c('aquamarine', 'lightblue', 'coral'), legend.text = c('ensino fundamental','ensino médio','ensino superior'), main='Gráfico de barras para estado civil x instrução', beside = T)

# Adicionando as somas por linhas e por colunas
tabmarg = addmargins(tab, 1:2)

k = nrow(tabmarg) - 1
m = ncol(tabmarg) - 1
n = sum(tab)

# Tabela sob independência
tabind = tabmarg[1:k, m + 1] %*% t(tabmarg[k + 1, 1:m]) / n
rownames(tabind) = rownames(tab)
colnames(tabind) = colnames(tab)

tab

tabmarg

round(tabind,1)


# Gráfico de mosaico sob independência
mosaicplot(t(tabind), main='Gráfico de mosaico sob independência', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)

# Teste Qui-quadrado
chisq.test(tab)

# Teste exato de Fisher
fisher.test(tab)

Q2 <- chisq.test(tab)$statistic
Q2

k <- nrow(tab)
m <- ncol(tab)

# Quantil qui-quadrado
qchisq(0.95, (k-1)*(m-1))

curve(dchisq(x,(k-1)*(m-1)), 0, 10)


n <-nrow(dados)
