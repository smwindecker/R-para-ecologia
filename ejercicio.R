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

