## Ejercicio 1: Creendo objectos en R
# 1. Crea 2 objectos numericos
# 2. Crea 2 objectos de clase `character`
# 3. Practica functiones de base como `c()`, `sum()`, `min()`...

## Ejercicio 2: Dataframes y submuestreando
# 1. Crea nuevo data frame
# 2. Practica str(), head(), View(), y otros funciones
# 3. Cambiar la clase de una columna (usando as.factor(), 
#     as.numeric(), as.character()) 
# 4. Submuestrear por columna y/o fila (usando $ y [ , ])

## Ejercicio 3: Modelos lineales
# 1. Carga datos predictores.csv
# 2. Explora y elige una variable respuesta (continua) y una
#     variables predictora (continua o categorica)
# 3. Haz un modelo lineal y revise el resumen




# tidyverse
# shrinkage 
group_mean <- mean(sleepstudy$Reaction)

library(dplyr)
by_subject <- sleepstudy |>
  group_by(Subject) |>
  summarise(mean_rt = mean(Reaction)) |>
  mutate(mean_rt_centered = mean_rt - group_mean)


## 1. elegir y abrir dataset
# mtcars
# iris
# lme4::sleepstudy

## 2. procesar en algun forma para practicar funciones

## 3. pensar en una variable respuesta / predictores razonables

## 4. evaluar si necesitas transformar predictores, 
# confirmar si pueden usar modelo linear ?? 

## 5. lm modelo

