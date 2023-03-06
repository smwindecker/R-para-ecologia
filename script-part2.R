### Curso Taller de R para investigadores 
### Script parte 2 - modelos lineales parte 2

# cargamos los paquetes de funciones que usaremos
library(ggplot2)

# para replicar los mismos resultados en cada participante
# usamos set.seed para que cada generacion aleatoria
# nos entregue los exactos mismos resultados
set.seed(111)

## ANOVA

# datos de la variable respuesta continua: 
# precipitación, temperatura mínima y temperatura máxima
# cargamos los datos
predictores <- read.csv("data/predictor_variables.csv", stringsAsFactors = FALSE)

# datos de la variable respuesta continua: precipitación
# variables predictoras categoricas: canyon or mountain range
train_data <- data.frame(precipitacion = predictores$precipitation,
                         mountain_range = predictores$mountain_range)

### visualizar los datos
boxplot(precipitacion ~ mountain_range, 
        data = train_data, 
        pch = 16, 
        xlab = "Mountain range", ylab = "precipitacion (mm)")


# ajustar el modelo
mod <- lm(precipitacion ~ mountain_range, data = train_data)

# chequeo visual de los resultados
plot(mod)

# `summarise` es una función para reportar un resumen del modelo
summary(mod)
# se puede ver que el modelo uso montana "Monitor" como nivel defecto

levels(train_data$mountain_range)
levels(as.factor(train_data$mountain_range))
# Monitor es el primer nivel

# si cambiamos el orden, vamos a tener una montana diferente como defecto:
train_data$mountain_range <- factor(train_data$mountain_range, 
                                    levels = c('Shoshone', 'Monitor', 
                                               'Toiyabe', 'Toquima'))

mod <- lm(precipitacion ~ mountain_range, data = train_data)
summary(mod)

## podemos visualizar como el modelo entiende niveles de variables categoricas
# viendo la matriz
model.matrix(~ mountain_range, data = train_data)


# t-test 
# convertir las niveles de montana de 4 a 2 para hacer ttest
train_data$region <- ifelse(train_data$mountain_range %in% 
                              c("Monitor", "Toquima"), "East", "West")
tmod <- t.test(precipitacion ~ region, data = train_data)
tmod

### Mas variables predictoras
# vamos a mirar la corelacion entre 
# mismo como round(cor(predictor_variables), 2)
predictor_variables <- predictores[, c(7, 8, 15)]
cor_pred <- cor(predictor_variables)
cor_mat <- round(cor_pred, 2)
corrplot::corrplot(cor_mat)

# solucion: sacar variables hasta que ninguna tienen alta correlación

# variables predictoras - transformaciones para normalizar
hist(predictores$elevation_m)
hist(predictores$terrain_ruggedness)
# se ven bien

train_data$terrain_ruggedness <- predictores$terrain_ruggedness
train_data$elevacion <- predictores$elevation_m

mod_multiple <- lm(precipitacion ~ elevacion + terrain_ruggedness, 
                   data = train_data)
summary(mod_multiple)
# tratamos de summarisar 
# precipitacion ~ -144.8 + 0.108*elevacion + 0.02916*terrain_ruggedness 
                  # + N(0, 51.39)

# es buen practica estandardizar variables predictoras continuas 
train_data <- train_data |>
  dplyr::mutate(scaled_elevacion = scale(elevacion),
                scaled_terrain_ruggedness = scale(terrain_ruggedness))
names(train_data)

mod_scaled <- lm(precipitacion ~ scaled_elevacion + scaled_terrain_ruggedness, 
                 data = train_data)
summary(mod_scaled)
# podemos ver que el RSE y los R2 son los mismos
# los estimaciones de las variables cambiaron dado a la escala de las variables
# 1. es modelo convierge mas fácilmente cuando las variables están en la misma
# escala. 
# 2. Mas fácil comparar tamaño de los efectos 


### Múltiples variables predictoras: interacciones
plot(precipitacion ~ elevacion, data = train_data, 
     pch = 16, las = 1, bty = "l", cex = 0.9,
     col = alpha(col_palette[25], 0.8),
     xlab = "elevacion (m)", ylab = "precipitacion (mm)")
mod <- lm(precipitacion ~ elevacion * mountain_range,
          data = train_data)
for (i in seq_len(3))
  abline(coef(mod)[1] - i * 150, coef(mod)[2] + i * 0.07, 
         col = col_palette[floor(i * 75)], lwd = 3)

# recordamos que este formula es un truco para definir el modelo mas rápido
# pero esta descriviendo este modelo:
# precipitacion ~ intercepto + elevacion + montana + 
#                 elevacion * montana
