# Obtener metadatos de versiones de cuestionarios ENOE por trimestre

Consulta la información sobre qué versiones de cuestionarios (COE, SDEM,
FD) corresponden a un trimestre específico de la ENOE.

## Usage

``` r
info_trimestre(anio, trimestre)
```

## Arguments

- anio:

  Año del trimestre (2005-2026). Debe ser un valor numérico entre 2005 y
  2026.

- trimestre:

  Número del trimestre (1-4). Donde 1 = ENE-MAR, 2 = ABR-JUN, 3 =
  JUL-SEP, 4 = OCT-DIC.

## Value

Un data.frame con 6 columnas:

- trimestre:

  Código del trimestre (ej. "t105")

- coe_tipo:

  Tipo de cuestionario COE ("ampliado" o "basico")

- coe_v:

  Versión del cuestionario COE (ej. "v1", "v2")

- sdem_v:

  Versión del cuestionario SDEM

- fd:

  Versión del file descriptor

- encoding:

  Encoding recomendado para los archivos

Retorna NULL si no se encuentra información para el trimestre
especificado.

## See also

Other descarga_documenta_enoe:
[`carga_enoe()`](https://aniuxa.github.io/renoe/reference/carga_enoe.md),
[`descarga_enoe()`](https://aniuxa.github.io/renoe/reference/descarga_enoe.md),
[`descargar_cuestionarios()`](https://aniuxa.github.io/renoe/reference/descargar_cuestionarios.md),
[`fusion_enoe()`](https://aniuxa.github.io/renoe/reference/fusion_enoe.md)

## Examples

``` r
# Consultar información para el primer trimestre de 2020
info_trimestre(2020, 1)
#>    trimestre coe_tipo coe_v sdem_v fd encoding
#> 61      t120 ampliado    v5     v4 v1    UTF-8

# Consultar información para el tercer trimestre de 2015
info_trimestre(2015, 3)
#>    trimestre coe_tipo coe_v sdem_v fd encoding
#> 43      t315   basico    v4     v3 v1   latin1
```
