### Curso Taller de R para investigadores 
### Script parte 3 - modelos mixtos

### Modelos mixtos
library(lme4)
library(ggplot2)

# interceptos aleatorios
# repuesta ~ fixed_effects + (1 | random_effects)

# pendientes aleatorios
# repuesta ~ fixed_effects + (fixed_effects | random_effects)

# cargamos los datos

# estos datos contienen un estudio acerca de deprivacion de sue;o
# Los datos son de tiempo de respuesta promedio (en ms) 
# luego de n-dias de deprivacion.
# Hay datos de 18 individuos en total, con un total de 180 observaciones;
head(lme4::sleepstudy)
length(unique(sleepstudy$Subject))

# graficando un modelo que asume independencia de observaciones

# pipe operator, original version %>% from library(magrittr) 
sleepstudy |>
  ggplot(aes(x = Days,
             y = Reaction)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "RT over successive days") +
  theme_minimal()

# modelo lineal agnóstico (sin reportar variabilidad 
# entre individuos)
naive_model <- lm(data = sleepstudy,
                 Reaction ~ Days)
summary(naive_model)

# mostrando la no-independencia al graficar por individuo
sleepstudy |>
  ggplot(aes(x = reorder(Subject, Reaction),
             y = Reaction)) +
  geom_boxplot() +
  labs(title = "Subject-level variance in RT",
       x = "Subject") +
  theme_minimal()

# respuestas individuales
sleepstudy |>
  ggplot(aes(x = Days,
             y = Reaction,
             color = Subject)) +
  # geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "RT over successive days") +
  theme_minimal()

# modelo con interceptos y pendientes aleatorias
model_full <- lmer(data = sleepstudy,
                   Reaction ~ Days + (1 + Days | Subject),
                   REML = FALSE)
summary(model_full)

# noten que ya no se muestra en el summary los valores de significancia
# de los coeficientes: esto es para promover la contrastacion con modelo
# nulo, que en este caso seria el modelo SOLO CON EFECTOS ALEATORIOS

# guardamos en un objeto el modelo nulo (solo efectos aleatorios)
model_reduced <- lmer(data = sleepstudy,
                     Reaction ~ (1 + Days | Subject),
                     REML = FALSE)

# y usamos ANOVA para compararlo con el modelo completo 
# (efectos fijos y aleatorios)
comparison <- anova(model_full, model_reduced)
comparison

# ahora evaluamos la direccion del efecto de la cantidad 
# de dias sin dormir sobre el tiempo de respuesta en milisegundos
coef_days <- fixef(model_full)["Days"]
coef_days

VarCorr(model_full)

# print the random effects
# ranef(mod_int)

