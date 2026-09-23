# Procesar el modulo de cuidado de mercado

Interfaz del modulo desarrollado para el articulo sobre brechas de
ingreso mediante regresiones cuantilicas. Clasifica en memoria; no
descarga, escribe ni reconstruye microdatos. Los modelos pertenecen al
proyecto del articulo.

## Usage

``` r
procesar_cuidado_remunerado(
  data,
  anio = NULL,
  trimestre = NULL,
  variable_codigo = "p3coe",
  variable_ocupacion = "sinco3d",
  variable_actividad = "p4a",
  variable_ocupado = "clase2",
  valor_ocupado = 1
)
```

## Arguments

- data:

  Data frame de personas; puede contener varios trimestres.

- anio, trimestre:

  Periodo opcional, escalar o vector de longitud `nrow(data)`. Si se
  omite, usa `anio` y `trim` en data. Rechaza conflictos.

- variable_codigo:

  Codigo ocupacional observado de cuatro digitos.

- variable_ocupacion:

  Respaldo SINCO observado de tres digitos.

- variable_actividad:

  Variable SCIAN-Hogares.

- variable_ocupado:

  Variable de condicion de ocupacion.

- valor_ocupado:

  Valor que identifica personas ocupadas.

## Value

El data frame con clasificacion y trazabilidad del cuidado. Recalcula
las salidas propias del modulo si existen; preserva los insumos.

## Details

Cuando detecta la salida completa de
[`armonizar_sinco()`](https://aniuxa.github.io/renoe/reference/armonizar_sinco.md),
usa `sinco4d_base2011` y `sinco3d` como insumos canonicos para todos los
periodos. Conserva `p3coe` como codigo original y no vuelve a decidir el
catalogo por su cuenta. Si la ruta canonica no esta presente, conserva
el comportamiento historico basado en el clasificador observado.

Conserva las columnas originales, incluido `sinco3d`. Agrega las salidas
de
[`class_cuidado_rem()`](https://aniuxa.github.io/renoe/reference/class_cuidado_rem.md)
y trazabilidad especifica. `codigo_ocupacion_armonizado` tiene tres
digitos y representa SINCO 2011 cuando la ruta canonica esta disponible;
en el modo heredado conserva la interpretacion anterior. Para los
remanentes CMO sin SINCO 3d canonico, el puente analitico de cuidado
recupera solo la clasificacion dependiente y registra su multiplicidad;
no rellena el SINCO general ni convierte 9999 en ocupacion.
`codigo_ocupacion_original_cuidado` conserva el insumo sin recodificar.
Las correspondencias multiples siguen la primera regla del material de
Damian y quedan identificadas en `calidad_armonizacion_cuidado`.

`trabajo_cuidado_mercado` es el nombre de la tipologia. La funcion
separa la posicion remunerada, la posicion explicita sin pago y la
evidencia de ingreso observado o imputado. Un ingreso imputado positivo
nunca se presenta como remuneracion observada.

## See also

Other cuidado_remunerado:
[`class_cuidado_rem()`](https://aniuxa.github.io/renoe/reference/class_cuidado_rem.md),
[`cmo_to_sinco11_care()`](https://aniuxa.github.io/renoe/reference/cmo_to_sinco11_care.md)

## Examples

``` r
x <- data.frame(p3coe = c(2331, 4111), p4a = c(6111, 6111), clase2 = 1)
procesar_cuidado_remunerado(x, anio = 2022, trimestre = 1)
#>   p3coe  p4a clase2 puente_cmo_requerido puente_cmo_aplicado
#> 1  2331 6111      1                FALSE               FALSE
#> 2  4111 6111      1                FALSE               FALSE
#>   cuidado_ocupacion_medible cuidado_actividad_medible
#> 1                      TRUE                      TRUE
#> 2                      TRUE                      TRUE
#>   cuidado_cmo_8200_domestico clasificador_ocupacion_cuidado
#> 1                      FALSE           SINCO 2019 observado
#> 2                      FALSE           SINCO 2019 observado
#>   sinco_version_cuidado scian_version_cuidado scian_catalogo_alerta class_ocu
#> 1            SINCO 2019    SCIAN-Hogares 2018                 FALSE        11
#> 2            SINCO 2019    SCIAN-Hogares 2018                 FALSE         0
#>   isco_care care_industry_detalle care_industry care_w trabajo_cuidado_mercado
#> 1        23  Servicios educativos             1      1                       1
#> 2         0  Servicios educativos             1      4                       1
#>   cuida_1d clasificador_ocupacion      version_scian
#> 1        1             SINCO 2019 SCIAN-Hogares 2018
#> 2        4             SINCO 2019 SCIAN-Hogares 2018
#>   codigo_ocupacion_original_cuidado codigo_ocupacion_armonizado
#> 1                              2331                         233
#> 2                              4111                         411
#>              metodo_armonizacion_cuidado calidad_armonizacion_cuidado
#> 1 SINCO observado: primeros tres digitos              SINCO observado
#> 2 SINCO observado: primeros tres digitos              SINCO observado
#>   cuidado_n_destinos_puente_cmo cuidado_posicion_remunerada cuidado_sin_pago
#> 1                            NA                          NA               NA
#> 2                            NA                          NA               NA
#>   estado_ingreso_cuidado
#> 1         no_determinado
#> 2         no_determinado
```
