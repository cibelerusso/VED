# Representação de Dados multidimensionais
# Por Cibele Russo - ICMC USP
# SME0803 Visualização e Exploração de Dados

library(lattice)

splom(USArrests)
splom(USArrests, type = c("p", "smooth"))
splom(USArrests, type = c("g", "p", "smooth"), col = "black", xlab = "", 
      varnames = c("Homicídio", "Assalto", "População \n urbana (%)", "Estupro"))

attach(USArrests)
coplot(Murder ~ Assault | UrbanPop)
co.intervals(UrbanPop, number = 6, overlap = 0.5)
coplot(Murder ~ Assault | UrbanPop * Rape, number = c(2, 3), pch = 20, 
       cex = 1.5, panel = panel.smooth)

xyplot(Murder ~ Assault | cut(UrbanPop, 3))
xyplot(Murder ~ Assault | cut(UrbanPop, 3), type = c("p", "smooth"), pch = 20)

xyplot(Murder + Rape ~ UrbanPop | cut(Assault, 3), type = c("p","smooth"),
       pch = 20, strip = strip.custom(strip.names = TRUE, var.name = "Assault"))


xyplot(Murder ~ UrbanPop | cut(Assault, 3) + cut(Rape, 2),pch = 20, 
       strip = strip.custom(strip.names = TRUE, var.name = c("Assault", "Rape")))

parallelplot(USArrests)

parallelplot(USArrests,
         varnames = c("Homicídio", "Assalto", "População \n urbana (%)", "Estupro"))

parallelplot(USArrests,
         varnames = c("Homicídio", "Assalto", "População \n urbana (%)", "Estupro"))



library(ineq)
data(Ilocos)
dados = Ilocos
attach(dados)
names(dados)


(tab3 = ftable(urbanity, province, sex))
tab3rel = prop.table(tab3, margin = 1)
(tab3relp = tab3rel *   100)


rownames(tab3relp) = paste(rep(levels(urbanity), each =length(levels(province))), 
                           levels(province), sep = "\n")
barplot(t(tab3relp), beside = TRUE, legend = levels(sex), density
          = 15, ylab = "Percentagem")
box()


(tab3var = xtabs(~ urbanity + province + sex))

as.data.frame(tab3var)

barchart(prop.table( tab3var, margin = c(1, 2)) * 100, xlab ="Percentagem", 
         ylab = "Localização")


barchart(prop.table( tab3var, margin = c(1, 2)) * 100,
         xlab = "Percentagem", ylab= "Localização", layout =c(1, 4))

names(dados)

stripplot(log(income / 1000, 10) ~ sex | province, xlab = "Sexo do responsável",
          ylab = "Log renda domiciliar(1000 pesos)")



stripplot(log(income / 1000, 10) ~ sex | province + urbanity, 
xlab = "Sexo do responsável", ylab = "Log renda domiciliar (1000 pesos)",
          jitter.data = TRUE)

bwplot(log(income / 1000, 10) ~ sex, xlab = "Sexo do responsável", 
       ylab = "Log renda domiciliar (1000 pesos)")

bwplot(log(income / 1000, 10)  ~ sex | province, xlab = "Sexo do responsável",
       ylab = "Log
renda domiciliar (1000 pesos)",
       layout = c(4, 1))

bwplot(log(income / 1000, 10)
       ~ sex | province + urbanity,
       xlab = "Sexo do responsável",
       ylab = "Log renda domiciliar(1000 pesos)")


histogram(~ log(income / 1000, 10) | province, type =
                  "percent", ylab =  "Percentagem", xlab = "Log
renda domiciliar (1000 pesos)",  col = "white")

histogram(~ log(income /  1000, 10) | province, type =  "density", 
          layout = c(1,  length(levels(province))), ylab = "Densidade", 
          xlab = "Log renda domiciliar (1000 pesos)",
          col = "white")



densityplot(~ log(income / 1000, 10) | province, ylab = "Densidade", xlab = "Log renda
domiciliar (1000 pesos)")

densityplot(~ log(income / 1000, 10), groups = sex, ylab =  "Densidade",  
            xlab = "Log renda domiciliar (1000 pesos)",
            plot.points = FALSE, auto.key = TRUE)


histogram(~ log(income  / 1000, 10) | province, type = "density",
          ylab = "Densidade",  xlab = "Log renda domiciliar (1000
pesos)", col = "white",  panel =  function(x, ...)
                  { panel.histogram(x,  ...)  
        panel.mathdensity(dmath  = dnorm, 
                          col = "blue", args = list(mean = mean(x),sd = sd(x))) })

xyplot(log(income / 1000, 10) ~ family.size | province, xlab = "Tamanho da família",
       ylab ="Log renda domiciliar (1000 pesos)")


xyplot(log(income / 1000, 10) ~ family.size | province, xlab ="Tamanho da família", ylab = "Log
renda domiciliar (1000 pesos)", pch = 20, type = c("p", "r"))

xyplot(log(income / 1000, 10) ~ family.size | province, xlab ="Tamanho da família", ylab = "Log
renda domiciliar (1000 pesos)", pch = 20, type = c("p", "smooth"))


xyplot(log(income / 1000, 10) ~family.size | province, group = sex, auto.key = TRUE, xlab =
               "Tamanho da família", ylab = "Log renda domiciliar (1000 pesos)", pch
       = 20, type = c("p", "r"))


xyplot(log(income / 1000, 10) ~ family.size | province + urbanity,
       group = sex, auto.key = TRUE, xlab = "Tamanho da família", ylab =   "Log renda domiciliar (1000
pesos)", pch = 20, type = c("p",  "r"))

parallelplot(~ cbind(family.size, income) | province, varnames = c("Tamanho da \nfamília", "Renda\
n domiciliar"))


parallelplot(~ cbind(family.size, income) | province + urbanity,  varnames = c("Tamanho da \
nfamília", "Renda domiciliar"))



# Dados amostra_banco

dados <- read.csv('https://raw.githubusercontent.com/cibelerusso/VED/main/Dados/amostra_banco.csv', header=T)

names(dados)

attach(dados)


xyplot(Salario ~ Idade | Sexo + Empresa, type=c('p','r'))

xyplot(Salario ~ Idade |  Empresa + Sexo, type=c('p','r'))


xyplot(Salario ~ Idade | Empresa + Sexo, type=c('p','smooth'))



parallelplot(~ cbind(Salario, Saldo_cc, Saldo_investimento, Saldo_poupança) | Empresa + Sexo)


parallelplot(~ cbind(Salario, Idade) | Empresa + Sexo)
 


# Selecionando somente os dados com Saldo_investimento e Saldo_poupança > 0


dados1 <- dados[(Saldo_investimento>0) & (Saldo_poupança > 0),]


parallelplot(~ cbind(Salario, Saldo_cc, Saldo_investimento, Saldo_poupança) | Empresa + Sexo, data = dados1)


parallelplot(~ cbind(Salario, Idade) | Empresa + Sexo, data=dados1)



