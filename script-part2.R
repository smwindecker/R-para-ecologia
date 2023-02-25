
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


## ANOVA

# datos de la variable respuesta continua: 
# precipitación, temperatura mínima y temperatura máxima
precipitacion <- predictores$precipitation

# variables predictoras categoricas: canyon or mountain range
mountain_range <- predictores$mountain_range

### visualizar los datos
boxplot(precipitacion ~ mountain_range, pch = 16, 
        xlab = "Mountain range", ylab = "precipitacion (mm)")

# ajustar el modelo
mod <- lm(precipitacion ~ mountain_range)

# chequeo visual de los resultados
plot(mod)

# `summarise` es una función para reportar un resumen del modelo
summary(mod)

# t-test 
# convertir las niveles de montana de 4 a 2 para hacer ttest
region <- ifelse(mountain_range %in% 
                   c("Monitor", "Toquima"), "East", "West")
tmod <- t.test(precipitacion ~ region)
tmod

### Mas variables predictoras
mts_levels <- c('Shoshone', 'Monitor', 'Toiyabe', 'Toquima')
releveled_mts <- factor(predictores$mountain_range, 
                        levels = mts_levels)

# mismo como round(cor(predictor_variables), 2)
predictor_variables <- predictores[, c(7, 8, 14:15)]
cor_pred <- cor(predictor_variables)
cor_mat <- round(cor_pred, 2)
# corrplot::corrplot(cor_mat)

# solucion: sacar variables hasta que ninguna tienen alta correlación

# variables predictoras - transformaciones para normalizar
hist(predictores$sagebrush_cover)
hist(boot::logit(predictores$sagebrush_cover))

# es buen practica estandardizar variables predictoras continuas 
mod_multiple <- lm(precipitation ~ elevation_m + terrain_ruggedness +
                     max_temp, 
                   data = predictores)
mod_scaled <- lm(precipitation ~ scale(elevation_m) + scale(terrain_ruggedness), 
                 data = predictores)

# standardise continuous predictores
predictores_std <- scale(predictor_variables)
summary(predictores_std)

obj <- scale(predictores$elevation_m)
scale <- attr(obj, 'scaled:scale')
center <- attr(obj, 'scaled:center')

(obj * attr('scaled:scale') + attr(obj, 'scaled:center'))

# scale(nuevo_elevacion, scale = scale, center = center)


### Multiples variables predictoras: interacciones
elevacion <- predictores$elevation_m
plot(precipitacion ~ elevacion, pch = 16, las = 1, bty = "l", cex = 0.9,
     col = alpha(col_palette[25], 0.8),
     xlab = "elevacion (m)", ylab = "precipitacion (mm)")
mod <- lm(precipitacion ~ elevacion * mountain_range)
for (i in seq_len(3))
  abline(coef(mod)[1] - i * 150, coef(mod)[2] + i * 0.07, col = col_palette[floor(i * 75)], lwd = 3)

### Multiples predictores: interacciones

# precipitacion ~ intercepto + elevacion + montana + 
#                 elevacion * montana
mod <- lm(precipitacion ~ elevacion * mountain_range)
summary(mod)

# precipitacion ~ intercepto + elevacion * montana
mod <- lm(precipitacion ~ elevacion : mountain_range)
summary(mod)
