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
armonizar_sinco(
  datos,
  escenario = c("integrated_accepted", "official_strict", "analysis_legacy"),
  legacy = NULL,
  capas = NULL,
  ...
)
```

## Arguments

- datos:

  Microdatos ENOE con `anio`, `trim` y `p3coe`.

- escenario:

  Contrato de decision: `official_strict` conserva solo equivalencias
  oficiales; `integrated_accepted` agrega reglas aceptadas de panel y
  condiciones ENOE; `analysis_legacy` habilita ademas rescates
  historicos no transportables.

- legacy:

  Compatibilidad explicita. `TRUE` selecciona `analysis_legacy`; `FALSE`
  impide combinar ese escenario.

- capas:

  Interfaz de bajo nivel. Si se proporciona se respeta, pero no habilita
  por si sola rescates historicos.

- ...:

  Argumentos adicionales para el motor SINCO interno.
