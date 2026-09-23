# Armoniza SCIAN-Hogares observado hacia SCIAN-Hogares 2018

Usa el regimen documentado de la ENOE: SCIAN-Hogares 2007 hasta 2021-T2
y SCIAN-Hogares 2018 desde 2021-T3. Conserva codigos especiales ENOE y
explicita los resultados preferidos, agregados y pendientes.

## Usage

``` r
.armonizar_scian_hogares_core(
  datos,
  codigo = "p4a",
  periodo = NULL,
  version = NULL,
  adapter_path = NULL,
  catalog_path = NULL,
  estricto = FALSE,
  detalle = TRUE
)
```

## Arguments

- datos:

  data.frame con el codigo observado.

- codigo:

  nombre de la columna de codigo (por defecto, `p4a`).

- periodo:

  columna, vector o escalar `YYYY-Tn`.

- version:

  columna, vector o escalar `2007`/`2018`. Si se omite, se infiere de
  `periodo`.

- adapter_path:

  Ruta opcional al adaptador. Por defecto usa la copia versionada
  incluida en el paquete.

- catalog_path:

  ruta al catalogo oficial verificado. Si es NULL, se localiza dentro
  del mismo producto que `adapter_path`.

- estricto:

  detenerse si quedan codigos no resueltos.

- detalle:

  Si es `TRUE`, conserva toda la trazabilidad. La salida compacta
  tambien conserva calidad, regla y motivo de pendiente.

## Value

`datos` con codigo original, version, resultado y trazabilidad.
