# Visualização e Exploração de Dados
# Cibele Russo - ICMC USP

# Medidas de Concentração e Desigualdade
# Por Cibele Russo

# Remove todos os elementos ativos
rm(list = ls(all=TRUE))

# install.packages("ineq")

library(ineq)

# Exemplos vistos na aula teórica
# Curva de Lorenz

clorenz <- Lc(c(1, 1, 2, 6, 30)) 
plot(clorenz, main='Curva de Lorenz', col=4)

clorenz <- Lc(c(1, 1, 2, 6, 300))
plot(clorenz, main='Curva de Lorenz', col=4)

clorenz <- Lc(c(1, 1, 2, 2, 2))
plot(clorenz, main='Curva de Lorenz', col=4)

# Índice de Gini
x <- c(0, 3.75)
clorenz <- Lc(x)
plot(clorenz, main='Curva de Lorenz', col=4)
G = Gini(x)
text(0.2, 0.9, 'G = 0.5', cex=1.5)

x = c(1/12, 1/12, 1/12, 9/12)
clorenz = Lc(x)
plot(clorenz, main='Curva de Lorenz', col=4)
G = Gini(x)
text(0.2, 0.9, 'G = 0.5', cex=1.5)


# Discrepância máxima
jmax = which.max(clorenz$p - clorenz$L)
Lmax = clorenz$p[jmax] - clorenz$L[jmax]

plot(clorenz, main='Curva de Lorenz', col=4)

segments(clorenz$p[jmax], clorenz$L[jmax],
         clorenz$p[jmax], clorenz$p[jmax],
         lty = 2, col = "blue")

text(clorenz$p[jmax] + 0.07, 
     clorenz$L[jmax] + 0.3, label = "Lmax")


# Exemplo bilionários
#install.packages('gglorenz')

?gglorenz

library(gglorenz)

?billionaires

View(billionaires)

ggplot(billionaires, aes(TNW)) +
  stat_lorenz() +
  annotate_ineq(billionaires$TNW) +
  geom_abline(linetype = "dashed") 

ggplot(billionaires, aes(TNW)) +
  stat_lorenz(desc = TRUE) +
  geom_abline(linetype = "dashed") +
  theme_bw() 
