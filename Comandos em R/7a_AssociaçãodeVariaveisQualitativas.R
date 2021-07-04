# Visualização e Exploração de Dados
# Cibele Russo - ICMC USP

dados <- read.csv ("https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/CompanhiaMB.csv", header=TRUE)

attach(dados)

dados<-data.frame(dados)

tab <- table(regiao, instrucao)

plot(tab, main='Gráfico de mosaico para os dados observados', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)

barplot(tab, col = c('aquamarine', 'lightblue', 'coral'), legend.text = c('capital','interio','outro'), main='Gráfico de barras para instrução x região')

plot(idade_anos, salario, pch=16, col=2, main='Gráfico de dispersão para idade x salário')
abline(lm(salario~idade_anos), col=4, lwd=2)
text(25,22,'Correlação = 0.36', cex=1.5)
cor(idade_anos,salario)





tab <- table(instrucao, estado_civil)


plot(tab, main='Gráfico de mosaico para os dados observados', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)


barplot(tab, col = c('aquamarine', 'lightblue', 'coral'), legend.text = c('ensino fundamental','ensino médio','ensino superior'), main='Gráfico de barras para estado civil x instrução', beside = T)



tabmarg = addmargins(tab, 1:2)
k = nrow(tabmarg) - 1
m = ncol(tabmarg) - 1
n = sum(tab)
tabind = tabmarg[1:k, m + 1] %*% t(tabmarg[k + 1, 1:m]) / n
rownames(tabind) = rownames(tab)
colnames(tabind) = colnames(tab)

tab

tabmarg
tabind



mosaicplot(tabind, main='Gráfico de mosaico sob independência', col=c('aquamarine', 'lightblue', 'coral'), cex=1.1)


chisq.test(tab)


fisher.test(tab)

Q2 <- chisq.test(tab)$statistic

k <- nrow(tab)
m <- ncol(tab)


qchisq(0.95, (k-1)*(m-1))


n <-nrow(dados)

C = sqrt(Q2/(Q2+n))
C


T = sqrt(Q2/(n * sqrt((k-1)*(m-1))))
T

