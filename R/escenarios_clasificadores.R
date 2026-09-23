# Contrato comun para los escenarios de armonizacion y sus consumidores.
.normalizar_escenario_clasificador <- function(
    escenario = c("integrated_accepted", "official_strict", "analysis_legacy"),
    legacy = NULL) {
  escenario <- match.arg(escenario)
  if (!is.null(legacy)) {
    if (!is.logical(legacy) || length(legacy) != 1L || is.na(legacy)) {
      stop("`legacy` debe ser TRUE, FALSE o NULL.", call. = FALSE)
    }
    if (isTRUE(legacy) && escenario != "analysis_legacy") {
      escenario <- "analysis_legacy"
    }
    if (!isTRUE(legacy) && escenario == "analysis_legacy") {
      stop("`escenario = 'analysis_legacy'` es incompatible con `legacy = FALSE`.",
           call. = FALSE)
    }
  }
  list(
    escenario = escenario,
    capas = switch(
      escenario,
      official_strict = "oficial",
      integrated_accepted = c("oficial", "panel", "enoe", "consenso"),
      analysis_legacy = c("oficial", "panel", "enoe", "consenso")
    ),
    legacy = identical(escenario, "analysis_legacy")
  )
}
