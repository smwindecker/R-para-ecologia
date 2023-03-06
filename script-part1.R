### Curso Taller de R para investigadores 
### Script parte 1 - modelos lineales parte 1

## Distribución normal

# cargamos los paquetes de funciones que usaremos
library(ggplot2)
library(performance)

# para replicar los mismos resultados en cada participante
# usamos set.seed para que cada generacion aleatoria
# nos entregue los exactos mismos resultados
set.seed(111)

# cargamos los datos
predictores <- read.csv("data/predictor_variables.csv", stringsAsFactors = FALSE)

# datos de la variable respuesta continua: precipitación
train_data <- data.frame(precipitacion = predictores$precipitation,
                         elevacion = predictores$elevation_m)


### explorando los datos:

hist(predictores$precipitation)
# variable respuesta es continua

hist(predictores$elevation_m)
# variable predictora es bastante normal, no necesita transformacion

# generamos un gráfico personalizado para visualizar los datos
# vamos a usa el "pipe" que se nota asi: |> en versiones recientes de R
# con versiones aniguos de R, se usa el pipe asi: %>% del paquette `magrittr`
train_data |>
  ggplot(aes(x = elevacion,
             y = precipitacion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

### Ajustando una regresión lineal en R
  
# lm ajusta un modelo lineal usando una formula 
mod <- lm(precipitacion ~ elevacion,
          data = train_data) 
# precipitación es var. respuesta
# y elevación su var. predictora

# chequeo visual de los resultados
check_model(mod)
check_normality(mod)
check_heteroscedasticity(mod)

# summarise es una función para reportar un resumen del modelo
summary(mod)

### Diagnostico y evaluación de los modelos
  
## evaluacion visual de normalidad de los residuales
hist(residuals(mod)) # hist genera un histograma
# residuals(modelo) entrega los valores de los residuales
