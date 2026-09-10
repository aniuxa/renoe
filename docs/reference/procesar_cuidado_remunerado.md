# Procesar el módulo de cuidado de mercado

Interfaz del modulo desarrollado para el articulo sobre brechas de
ingreso mediante regresiones cuantílicas. Clasifica en memoria; no
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

Usa CMO hasta 2012-II, SINCO 2011 desde 2012-III y SINCO 2019 desde
2021-III, conforme a las reglas existentes del paquete. Prefiere el
codigo observado de cuatro digitos (`p3coe`) para evitar confundir un
`sinco3d` previamente armonizado a 2011 con SINCO 2019. Si falta
`p3coe`, acepta una columna de tres digitos en el clasificador observado
del periodo. Rechaza el respaldo cuando detecta metadatos de
armonizacion general en 2019.

Conserva las columnas originales, incluido `sinco3d`. Agrega las salidas
de
[`class_cuidado_rem()`](https://aniuxa.github.io/renoe/reference/class_cuidado_rem.md)
y trazabilidad especifica. `codigo_ocupacion_armonizado` tiene tres
digitos: es un puente analitico en CMO y un codigo observado en SINCO;
no representa una homologacion universal a SINCO 2011.
`codigo_ocupacion_original_cuidado` conserva el insumo sin recodificar.
Las correspondencias multiples siguen la primera regla del material de
Damian y quedan identificadas en `calidad_armonizacion_cuidado`.

`trabajo_cuidado_mercado` es el nombre principal de la tipología. La
columna `trabajo_cuidado_rem` se conserva como alias deprecado. La
función separa la posición remunerada, la posición explícita sin pago y
la evidencia de ingreso observado o imputado. Un ingreso imputado
positivo nunca se presenta como remuneración observada.

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
#>   isco_care care_industry_detalle care_industry care_w cuida_total
#> 1        23  Servicios educativos             1      1           1
#> 2         0  Servicios educativos             1      4           1
#>   trabajo_cuidado_mercado trabajo_cuidado_rem cuida_1d clasificador_ocupacion
#> 1                       1                   1        1             SINCO 2019
#> 2                       1                   1        4             SINCO 2019
#>        version_scian codigo_ocupacion_original_cuidado
#> 1 SCIAN-Hogares 2018                              2331
#> 2 SCIAN-Hogares 2018                              4111
#>   codigo_ocupacion_armonizado            metodo_armonizacion_cuidado
#> 1                         233 SINCO observado: primeros tres digitos
#> 2                         411 SINCO observado: primeros tres digitos
#>   calidad_armonizacion_cuidado cuidado_posicion_remunerada cuidado_sin_pago
#> 1              SINCO observado                          NA               NA
#> 2              SINCO observado                          NA               NA
#>   estado_ingreso_cuidado
#> 1         no_determinado
#> 2         no_determinado
```
