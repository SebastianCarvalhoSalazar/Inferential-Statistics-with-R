# En el espacio de parametros podemos ver los parametros y sus estadisticas --

# Distribucion normal -----------------------------------------------------

tamano_muestral <- 35
iteraciones <- 100

medias <- vector()
desv_est <- vector()

media_poblacional <- 3
desv_est_poblacional <- 5

for(i in seq_len(iteraciones)){
  muestra <- rnorm(tamano_muestral, media_poblacional, desv_est_poblacional)
  medias[i] <- mean(muestra)
  desv_est[i] <- sd(muestra)
}

plot(medias, desv_est)
points(media_poblacional, desv_est_poblacional, col = 2, cex = 2, pch = 20)


# Distribucion uniforme ---------------------------------------------------


tamano_muestral <- 35
iteraciones <- 100


minimo_poblacional <- 3
maximo_poblacional <- 8

minimos <- vector()
maximos <- vector()

for(i in seq_len(iteraciones)){
  muestra <- runif(tamano_muestral, minimo_poblacional, maximo_poblacional)
  minimos[i] <- min(muestra)
  maximos[i] <- max(muestra)
}

plot(minimos, maximos)
points(minimo_poblacional, maximo_poblacional, col = 2, cex = 2, pch = 20)


# regresion lineal --------------------------------------------------------

tamano_muestral <- 35
iteraciones <- 100


beta_0 <- 1
beta_1 <- -0.3

beta_0_estimado <- vector()
beta_1_estimado <- vector()

genera_y <- function(x, beta_0, beta_1){
beta_1*x + beta_0 + rnorm(length(x), 0, 0.5)
}

for(i in seq_len(iteraciones)){
  X <- seq(-3, 3, length.out = tamano_muestral)
  Y <- genera_y(X, beta_0, beta_1)
  betas_estimados <- coef(lm(Y ~ X)) 
  beta_0_estimado[i] <- betas_estimados[1]
  beta_1_estimado[i] <- betas_estimados[2]
}

plot(beta_0_estimado, beta_1_estimado)
points(beta_0, beta_1, col = 2, cex = 2, pch = 20)

# paquetes ----------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("magrittr")
library("purrr")

colores <- c("#78D92A", "#002E4E", "#058ECD", "#ED2B05", "#F4F7F4")

tamano_muestral <- 35
iteraciones <- 100


# Distribucion normal -----------------------------------------------------

media_poblacional <- 3
desv_est_poblacional <- 5

tibble(
  muestras = replicate(iteraciones, rnorm(tamano_muestral, media_poblacional, desv_est_poblacional), simplify = FALSE),
  medias = map_dbl(muestras, mean),
  desv_est = map_dbl(muestras, sd)
) -> simulaciones

qplot(medias, desv_est, data = simulaciones) + 
  annotate("point", media_poblacional, desv_est_poblacional, colour = colores[3], size = 5) +
  theme_minimal()


# Distribucion uniforme ---------------------------------------------------

minimo_poblacional <- 3
maximo_poblacional <- 8

tibble(
  muestras = replicate(iteraciones, runif(tamano_muestral, minimo_poblacional, maximo_poblacional), simplify = FALSE),
  a = map_dbl(muestras, min),
  b = map_dbl(muestras, max)
) -> simulaciones

qplot(a, b, data = simulaciones) + 
  annotate("point", 3, 8, colour = colores[3], size = 5) +
  theme_minimal()


# Regresion lineal simple -------------------------------------------------

beta_0 <- 1
beta_1 <- -0.3

estima_betas <- function(x, y){
  coef(lm(y ~ x))
}

tibble(
  datos_x = replicate(iteraciones, seq(-3, 3, length.out = tamano_muestral), simplify = FALSE),
  datos_y = map(datos_x, genera_y, beta_0, beta_1),
  betas = map2(datos_x, datos_y, estima_betas),
  beta_0 = map_dbl(betas, extract, 1),
  beta_1 = map_dbl(betas, extract, 2),
) -> simulaciones

qplot(beta_0, beta_1, data = simulaciones) + 
  annotate("point", 1, -0.3, colour = colores[3], size = 5) +
  theme_minimal()
