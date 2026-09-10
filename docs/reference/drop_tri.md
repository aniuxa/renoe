# Eliminar sufijo '\_tri' y el prefijo 'cve' de nombres de variables

Esta función renombra las variables eliminando el sufijo '\_tri' y el
prefijo cve si existe. Para compatibilidad de la serie

## Usage

``` r
drop_tri(data)
```

## Arguments

- data:

  Un data.frame o tibble con nombres de variables posiblemente
  terminados en '*tri' y que inicien con 'cve*',

## Value

Un data.frame con los nombres de variables modificados.
