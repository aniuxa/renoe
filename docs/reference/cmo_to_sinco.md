# Convertir codigos CMO a SINCO (3 y 4 digitos)

Esta funcion toma codigos CMO de 4 digitos -ya sea proporcionados
directamente en `cmo_4d` o a traves de `p3coe`- y devuelve su
correspondencia con codigos SINCO 2011 (3 y 4 digitos), usando una tabla
de equivalencias incluida en el paquete. La cobertura depende del nivel
de desagregacion y de las capas seleccionadas.

## Usage

``` r
cmo_to_sinco(
  data,
  codigos = NULL,
  var_origen = "cmo_4d",
  keep_labels = FALSE,
  usar_reglas_enoe = NULL,
  capas = c("oficial", "panel", "enoe", "consenso")
)
```

## Arguments

- data:

  Un data.frame que contenga la variable `cmo_4d` o `p3coe`.

- codigos:

  Opcional: data.frame de equivalencias. Si se omite, se usa una tabla
  interna del paquete.

- var_origen:

  Nombre de la variable que contiene el codigo CMO (por defecto
  `cmo_4d`).

- keep_labels:

  Logico. Si TRUE, mantiene las etiquetas si existen.

- usar_reglas_enoe:

  Compatibilidad: TRUE activa todas las capas; FALSE selecciona solo
  oficial. NULL utiliza `capas`.

- capas:

  Capas habilitadas: `oficial`, `panel`, `enoe` y `consenso`, todas por
  defecto. Oficial siempre se incluye; panel aplica reglas validadas
  longitudinalmente usando CMO; enoe requiere variables auxiliares ENOE.
  Las columnas `sinco3d` y `sinco4d` preexistentes se reemplazan de
  manera explicita para que la funcion pueda ejecutarse nuevamente sin
  crear sufijos `.x` y `.y`.

  La capa consenso actua posteriormente en
  [`procesar_clasificaciones_reproducibles()`](https://aniuxa.github.io/renoe/reference/procesar_clasificaciones_reproducibles.md);
  no aumenta la desagregacion SINCO identificada por esta funcion.

## Value

El `data.frame` original con columnas adicionales: `cmo_4d`, `sinco4d` y
`sinco3d`.

## Examples

``` r
datos <- data.frame(p3coe = c(1101, 1102, 1167))
cmo_to_sinco(datos)
#>   p3coe cmo_4d sinco4d sinco3d        regla_cmo_sinco tipo_regla_cmo_sinco
#> 1  1101   1101    2261     226 CMO_OFFICIAL_UNIQUE_4D   official_unique_4d
#> 2  1102   1102    2254     225 CMO_OFFICIAL_UNIQUE_4D   official_unique_4d
#> 3  1167   1167    2112     211 CMO_OFFICIAL_UNIQUE_4D   official_unique_4d
#>   alcance_regla_cmo_sinco                             detalle_regla_cmo_sinco
#> 1  general_clasificadores Correspondencia oficial inequivoca a cuatro digitos
#> 2  general_clasificadores Correspondencia oficial inequivoca a cuatro digitos
#> 3  general_clasificadores Correspondencia oficial inequivoca a cuatro digitos
#>   n_destinos_regla_cmo_sinco
#> 1                          1
#> 2                          1
#> 3                          1
```
