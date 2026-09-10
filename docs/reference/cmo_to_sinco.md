# Convertir códigos CMO a SINCO (3 y 4 dígitos)

Esta función toma códigos CMO de 4 dígitos —ya sea proporcionados
directamente en `cmo_4d` o a través de `p3coe`— y devuelve su
correspondencia con códigos SINCO 2011 (3 y 4 dígitos), usando una tabla
de equivalencias incluida en el paquete. Funciona para el 82% de las
ocupaciones.

## Usage

``` r
cmo_to_sinco(data, codigos = NULL, var_origen = "cmo_4d", keep_labels = FALSE)
```

## Arguments

- data:

  Un data.frame que contenga la variable `cmo_4d` o `p3coe`.

- codigos:

  Opcional: data.frame de equivalencias. Si se omite, se usa una tabla
  interna del paquete.

- var_origen:

  Nombre de la variable que contiene el código CMO (por defecto
  `cmo_4d`).

- keep_labels:

  Lógico. Si TRUE, mantiene las etiquetas si existen. Las columnas
  `sinco3d` y `sinco4d` preexistentes se reemplazan de manera explícita
  para que la función pueda ejecutarse nuevamente sin crear sufijos `.x`
  y `.y`.

## Value

El `data.frame` original con columnas adicionales: `cmo_4d`, `sinco4d` y
`sinco3d`.

## Examples

``` r
datos <- data.frame(p3coe = c(1101, 1102, 1167))
cmo_to_sinco(datos)
#>   p3coe cmo_4d sinco4d sinco3d
#> 1  1101   1101    2261     226
#> 2  1102   1102    2254     225
#> 3  1167   1167    2112     211
```
