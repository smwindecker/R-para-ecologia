### Curso Taller de R para investigadores - La Paz, Bol, Feb 23
### Script part 4 - GLMs & GLMMs

# Section 1 - GLMs

# A: datos de conteos -->  distribucion Poisson [funcion de enlace = log]
# ***regresiones Poisson tambien pueden modelarse usando cloglog como funcion de enlace***

# cargamos los datos del mismo caso de estudio que en el script-part-1
respuestas <- read.csv("data/bird_community.csv", 
                       stringsAsFactors = FALSE)[, -1]
predictores <- read.csv("data/predictor_variables.csv", 
                        stringsAsFactors = FALSE)

# de las 118 especies trabajaremos solo con "greentailedtowhee" 
# usaremos como predictores la temperatura maxima (max_temp), la cobertura de vegetacion (veg_cover)
# y la elevacion (elevation_m)


###jXXXXXXKXKX hist(boot::logit(predictores$sagebrush_cover))

# entonces nuestro training dataset sera de tres columnas, primero la resp y luego las 2 predictoras
train_data <- data.frame(abundancia = respuestas$greentailedtowhee,
                         max_temp = predictores$max_temp,
                         veg_cover = predictores$veg_cover,
                         elevation_m = predictores$elevation_m)

# un poco de exploracion de los datos
hist(train_data$abundancia) # histograma de los conteos

hist(train_data$max_temp) 
# parece bastante normal, asi que probablemente no necesita transformacion

hist(train_data$veg_cover) 
# proporciones generalmente no son normales,
# podemos transformar con logit

# PEEEEERO:
# como logit(0) = -Inf, tenemos que sumarle un pokito a los 0s
# y como logit(1) = Inf, tenemos que restarle un pokito a los 1s
train_data$veg_cover[train_data$veg_cover == 0] <- 0.0001
train_data$veg_cover[train_data$veg_cover == 1] <- 0.9999


library(dplyr) # para usar funciones de tidyverse como vimos ayer
train_data <- train_data |>
  mutate(logit_veg_cover = boot::logit(veg_cover)) # la funcion logit no esta en base R

# chequeamos el resultado
hist(train_data$logit_veg_cover)

# ahora la ultima predictora
hist(train_data$elevation_m)
# parece normal asi que la dejamos


names(train_data)
round(cor(train_data[,c(2,4,5)]), 2) # no consideraremos en la matriz de corr
      # la version no transformada de veg_cover (columna 3)

# elevacion y temperatura max estan correlacionadas con un Pearson's r de -0.53
# sacaremos elevacion del modelo si dejamos solo con |r| < 0.5 
# (este umbral es bastante conservador, algunos investigadores usan 0.7 o incluso 0.8
# asi que si quisieran incluir dos variables con correlacion de r 0.53 no deberia ser problema)

# Finalmente, como nuestro objetivo incluye identificar que variable influye mas
# en la variabilidad de la abundancia de esta especie, estandarizaremos los predictores

train_data <- train_data|>
  mutate(scaled_max_temp = scale(max_temp),
         scaled_logit_veg_cover = scale(logit_veg_cover))
names(train_data)

hist(train_data$max_temp)
hist(train_data$scaled_max_temp)

hist(train_data$logit_veg_cover)
hist(train_data$scaled_logit_veg_cover, breaks = 25)

# Ahora recién vamos a entrenar el modelo
mod_abund <- glm(abundancia ~ as.numeric(scaled_logit_veg_cover) + 
                   as.numeric(scaled_max_temp),
                  family = poisson("log"),
                  data = train_data)

summary(mod_abund)
# bondad de ajuste calculada con el paquete 'performance'
performance::r2(mod_abund) 

confint(mod_abund)

out <- predict(mod_abund,
               type = 'response')
# tambien podemos generar predicciones en nuevos datos

#aqui simulamos datos
NewData <- data.frame(scaled_logit_veg_cover = rnorm(100, 0.5),
                      scaled_max_temp = rnorm(100,-0.5))
# y usamos el argumento 'newdata' con datos de los predictores
out_new <- predict(mod_abund,
               type = 'response',
               newdata = NewData)
# datos ajustados (predicciones sobre mismos datos de entrenamiento)
hist(out)
# predicciones sobre los nuevos datos
hist(out_new)
# B: Datos binarios --> distribucion Bernoulli (o Binomial) [funcion de enlace = logit]

# un ejemplo comun de datos binarios en ecologia son datos de presencia/ausencia
# para simplificar trabajo transformaremos los datos de abundancia en binarios
# donde todos los sitios que tengan abundancia > 0 seran considerados presencia
# y el resto ausencias...al fin y al cabo presencia/ausencia es una simplificacion de abundancia

occupacion <- train_data$abundancia
occupacion[occupacion > 0] <- 1
occupacion[occupacion == 0] <- 0
table(occupacion) 
table(train_data$abundancia) # chequeando que los 0s sean los mismos

datos_PresAus <- data.frame(occupacion = occupacion,
                            max_temp = as.numeric(train_data$scaled_max_temp),
                            veg_cover = as.numeric(train_data$scaled_logit_veg_cover)) 

# regresion logistica
mod_logistico <- glm(occupacion ~ max_temp + veg_cover,
                    family = binomial("logit"),
                    data = datos_PresAus)
summary(mod_logistico)
performance::r2(mod_logistico)
# si se dan cuenta, el valor de r2 disminuye considerablemente
# eso se debe a que al transformar de presencia/ausencia
# una gran cantidad de la variabilidad de la abundancia se pierde


# Section 2 - GLMMs

# ahora haremos los mismos modelos pero agregaremos
# la ID de las monta;as como efecto aleatorio 
# solo para demostrar el uso del codigo de glmm

# agregamos al dataframe las monta;as

# A: datos de conteos -->  distribucion Poisson [funcion de enlace = log]
train_data$mountain <- predictores$mountain_range

library(lme4) # paquete mas usado para glmms 

glmm_poisson <- glmer(abundancia ~ scaled_logit_veg_cover + 
                        scaled_max_temp +
                        (1|mountain),
                      family = poisson("log"),
                      data = train_data)
summary(glmm_poisson)
null_poisson <- glmer(abundancia ~ (1|mountain),
                      family = poisson("log"),
                      data = train_data)
anova(glmm_poisson,
      null_poisson)

performance::r2(null_poisson)
performance::r2(glmm_poisson)

# B: Datos binarios --> distribucion Bernoulli (o Binomial) [funcion de enlace = logit]
datos_PresAus$mountain <- predictores$mountain_range

glmm_binomial <- glmer(occupacion ~ max_temp + veg_cover +
                        (1|mountain),
                      family = binomial("logit"),
                      data = datos_PresAus)
summary(glmm_binomial)
null_binomial <- glmer(occupacion ~ (1|mountain),
                      family = binomial("logit"),
                      data = datos_PresAus)
anova(glmm_binomial,
      null_binomial)

performance::r2(null_binomial)
performance::r2(glmm_binomial)


