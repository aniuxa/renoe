# Procesar el perfil reproducible de productos academicos

Este wrapper no mantiene reglas propias. Si recibe microdatos sin el
contrato SINCO, ejecuta la ruta canonica completa; si recibe una salida
ya armonizada, aplica unicamente los consumidores reproducibles.

## Usage

``` r
procesar_productos_academicos(
  data,
  anio = NULL,
  trimestre = NULL,
  escenario = c("integrated_accepted", "official_strict", "analysis_legacy"),
  ...
)
```

## Arguments

- data:

  Microdatos ENOE o salida canonica ya armonizada.

- anio:

  Año requerido cuando `data` aun no fue procesado.

- trimestre:

  Trimestre requerido cuando `data` aun no fue procesado.

- escenario:

  Escenario explicito de armonizacion y consumidores.

- ...:

  Argumentos adicionales de
  [`procesar_variables_enoe()`](https://aniuxa.github.io/renoe/reference/procesar_variables_enoe.md).

## Value

El data frame con el perfil reproducible, su escenario y la marca
`academic_reproducible` cuando se solicita `analysis_legacy`. La marca
identifica la ruta apta para productos académicos congelados; el bundle
de publicación conserva por separado su `run_id` y hashes.
