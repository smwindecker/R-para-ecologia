## Distribución normal

# cargamos los paquetes de funciones que usaremos
library(viridisLite)
library(ggplot2)

# para replicar los mismos resultados en cada participante
# usamos set.seed para que cada generacion aleatoria
# nos entregue los exactos mismos resultados
set.seed(111)

# ajustamos una paleta de colores para los graficos
col_palette <- inferno(256)

# cargamos los datos
repuesta <- read.csv("data/bird_community.csv", stringsAsFactors = FALSE)[, -1]
predictores <- read.csv("data/predictor_variables.csv", stringsAsFactors = FALSE)

# datos de la variable respuesta continua: 
# precipitación, temperatura mínima y temperatura máxima
precipitacion <- predictores$precipitation

# elevación en metros
elevacion <- predictores$elevation_m

### generamos un gráfico personalizado
# para visualizar los datos
plot(precipitacion ~ elevacion, # formula a graficar
     pch = 16, # tamaño del punto de observación
     cex = 0.9, # grado de transparencia (90%)?
     col = alpha(col_palette[25], 0.8),
     xlab = "Elevación (m)", ylab = "Precipitación (mm)") # rótulos de los ejes
abline(lm(precipitacion ~ elevacion), # abline es una función para agregar una linea en general 
       col = col_palette[200], lwd = 3) # si es usando "abline(lm(formula))" sera de un modelo lineal 



### Ajustando una regresión lineal en R
  
# lm ajusta un modelo lineal usando una formula 
mod <- lm(precipitacion ~ elevacion) # precipitación es var. respuesta
# y elevación su var. predictora

# chequeo visual de los resultados
plot(mod)

# summarise es una función para reportar un resumen del modelo
summary(mod)
  

### Diagnostico y evaluación de los modelos
  
## evaluacion visual de normalidad de los residuales
hist(residuals(mod)) # hist genera un histograma
# residuals(modelo) entrega los valores de los residuales


# fit model with log-transformed repuesta
mod <- lm(log(precipitacion) ~ elevacion)

# is it any better?
plot(mod)

### Linear regression: interpreting a fitted model
  
summary(mod)

mod <- lm(precipitacion ~ elevacion)
summary(mod)


