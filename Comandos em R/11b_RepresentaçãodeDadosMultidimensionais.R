# Visualização e Exploração de Dados
# Cibele Russo - ICMC USP
# Representação de dados multidimensionais - Parte 2


# Gráficos de pontos 3D
# Fonte: http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization
# Fonte iris: https://rpubs.com/mbatista/545937

#install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

data(iris)
head(iris)

# Basic 3d graphics
scatterplot3d(iris[,1:3])

# Change the angle of point view
scatterplot3d(iris[,1:3], angle = 80)

scatterplot3d(iris[,1:3],
              main="3D Scatter Plot",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)",
              zlab = "Petal Length (cm)")

scatterplot3d(iris[,1:3], pch = 16, color="steelblue")

colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
scatterplot3d(iris[,1:3], pch = 16, color=colors)


s3d <- scatterplot3d(iris[,1:3], pch = 16, color=colors)
legend(s3d$xyz.convert(7.5, 3, 4.5), legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)


# Gráficos de bolhas 3D 

#install.packages('plotly')
library(plotly)

plot_ly(data=iris, x=~Sepal.Length, y=~Petal.Length, z=~Sepal.Width,  
          color=~Species, type='scatter3d')

plot_ly(data=iris, x=~Sepal.Length, y=~Petal.Length, z=~Sepal.Width,  
        size=~Petal.Width, color=~Species)

p <- plot_ly(data=iris, x=~Sepal.Length, y=~Petal.Length, z=~Sepal.Width  )
add_markers(p, size=~Petal.Width, color=~Species, mode="markers" )




# Gráfico de radar ou estrelas
# Fonte: https://www.r-graph-gallery.com/142-basic-radar-chart.html
# install.packages('fmsb')
library(fmsb)

# Create data: note in High school for Jonathan:
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(20,10) , rep(0,10) , data)

# Check your data, it has to look like this!
# head(data)

# The default radar chart 
radarchart(data)

# Ver mais em: https://www.datanovia.com/en/blog/beautiful-radar-chart-in-r-using-fmsb-and-ggplot-packages/






# Mapa de árvore - Tree map
# Fonte: https://www.r-bloggers.com/2015/11/treemap-world-population-visualisation/

# install.packages('treemap')
library(treemap)
data(GNI2014)
treemap(GNI2014,
        index=c("continent", "iso3"),
        vSize="population",
        vColor="GNI",
        type="value")



# Mapa de calor - heat map
# Fonte: https://www.r-graph-gallery.com/215-the-heatmap-function.html
# Ver também: https://returnonnow.com/2017/09/heatmaps-increase-conversion-rate/
        
data <- as.matrix(mtcars)
heatmap(data, scale="column")

