# Adaptado de dineq::ntiles.wtd 0.1.0, de Rene Schulenberg (GPL-3):
# https://cran.r-project.org/src/contrib/Archive/dineq/
# Conserva el comportamiento historico de la funcion ponderada.
# Las observaciones empatadas permanecen en el mismo intervalo, cerrado a la
# derecha; por ello los grupos no necesariamente tienen igual peso acumulado.
.quintiles_ponderados <- function(x, n, weights) {
  if (!all(weights >= 0, na.rm = TRUE)) {
    stop("At least one weight is negative", call. = FALSE)
  }
  if (all(weights == 0, na.rm = TRUE)) {
    stop("All weights are zero", call. = FALSE)
  }

  seleccion <- !(is.na(x) | is.na(weights))
  paso <- 1 / n
  cortes <- Hmisc::wtd.quantile(
    x[seleccion], weights = weights[seleccion],
    probs = c(seq(paso, 1 - paso, paso)), na.rm = TRUE
  )
  resultado <- as.numeric(.bincode(
    x, breaks = c(-Inf, cortes, Inf), right = TRUE, include.lowest = TRUE
  ))
  resultado[is.na(weights)] <- NA_real_
  resultado
}
