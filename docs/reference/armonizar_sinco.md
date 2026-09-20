# Armonizar ocupacion hacia SINCO 2011 con un contrato auditable

Produce el contrato de salida SINCO y agrega los campos `sinco2011_*`
que consumen las clasificaciones existentes. Una seleccion por panel es
`preferred`; una condicion ENOE es `conditional`, nunca una equivalencia
oficial `unique` a cuatro digitos. La clave 9999 puede corresponder
oficialmente a 9999 entre catalogos, pero
`sinco2011_codigo_especial = TRUE` y `sinco2011_comparable = FALSE`
impiden tratarla como ocupacion sustantivamente identificada en
clasificaciones derivadas.

## Usage

``` r
armonizar_sinco(datos, ...)
```

## Arguments

- datos:

  Microdatos ENOE con `anio`, `trim` y `p3coe`.

- ...:

  Argumentos adicionales para el motor SINCO interno.
