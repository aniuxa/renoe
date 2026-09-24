# Imputar ingresos ocupacionales con MICE

Esta funcion aplica imputacion de ingresos mensuales (`ingocup`) para
personas ocupadas en la ENOE utilizando modelos de imputacion multiple
con el paquete `mice`. Se utiliza el logaritmo del ingreso como variable
objetivo y se imputan los valores faltantes en funcion de variables
donantes como edad, escolaridad, ocupacion, horas trabajadas, entre
otras.

## Usage

``` r
imputa_ingocup(
  data,
  vars_donantes = c("edad", "anios_es", "c_ocu11c", "pos_ocu", "rama_est2", "ing7c",
    "ent", "hrsocup", "t_loc"),
  id_vars = c("folio3", "trim", "anio"),
  method = "pmm",
  seed = 1234,
  plot = FALSE,
  anio = NULL,
  trimestre = NULL
)
```

## Arguments

- data:

  Un data frame con personas ocupadas (`clase2 == 1`) y variables de
  ingreso (`ingocup`), variables donantes y metadatos de identificacion.

- vars_donantes:

  Vector con nombres de variables que se utilizaran como predictores
  para la imputacion.

- id_vars:

  Vector con nombres de variables identificadoras (por defecto:
  `folio3`, `trim`, `anio`).

- method:

  Metodo de imputacion utilizado por `mice` (por defecto: `"pmm"`).

- seed:

  Semilla aleatoria para reproducibilidad.

- plot:

  Logico. Si `TRUE`, se muestra un grafico comparando la distribucion
  del ingreso original vs imputado.

- anio:

  Año del trimestre, si `data` no contiene esta variable.

- trimestre:

  Trimestre del año (1-4), si `data` no contiene esta variable.

## Value

Un data frame con las variables:

- ingocup_imp:

  Ingreso mensual imputado

- log_ingocup_imp:

  Logaritmo del ingreso imputado

- imp_ingocup:

  Indicador binario de si el ingreso fue imputado (1 = si)

## Details

La imputacion se realiza primero por bloques de sexo y entidad
federativa. Los casos que no pueden imputarse dentro de esos bloques
pasan a un modelo conjunto de respaldo, en el cual el sexo y la entidad
se incorporan como covariables. Si no existen las variables `folio3`,
`anio` o `trim`, se generan automaticamente con funciones auxiliares
([`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md)
y
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md)).

La imputacion de ingresos se realiza unicamente para personas ocupadas
(`clase2 == 1`) con datos validos de edad, y en caso de estar
disponible, tambien de años de escolaridad (`anios_es`).

La variable a imputar es el logaritmo natural del ingreso mensual
(`log_ingocup_imp`), y la imputacion se realiza utilizando el metodo
especificado (por defecto `"pmm"`, predictive mean matching) a traves
del paquete `mice`.

Las imputaciones se hacen primero de forma separada por bloques
definidos por el sexo (`sex`) y la entidad federativa (`ent`), para
capturar mejor las heterogeneidades contextuales. Cuando un bloque no
contiene donantes o variacion suficiente, sus casos pendientes se
imputan conjuntamente usando `sex`, `ent` y las demas variables donantes
disponibles como predictores. Las variables identificadoras nunca se
usan como predictores.

Las variables utilizadas como predictoras ("donantes") incluyen, si
estan presentes:

- `edad`: Edad en años.

- `anios_es`: Años aprobados de escolaridad.

- `c_ocu11c`: 11 grandes grupos ocupacionales.

- `pos_ocu`: Posicion en la ocupacion.

- `rama_est2`: Rama de actividad.

- `ing7c`: Indicador de percepcion de ingresos.

- `ent`: Clave de entidad federativa.

- `hrsocup`: Horas trabajadas a la semana.

- `t_loc`: Tamano de localidad.

Solo se consideran aquellas variables donantes que estan disponibles en
el conjunto de datos.

## See also

Other procesamiento_enoe:
[`.armonizar_sinco_enoe_core()`](https://aniuxa.github.io/renoe/reference/dot-armonizar_sinco_enoe_core.md),
[`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md),
[`calcular_desajuste_horizontal()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_horizontal.md),
[`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md),
[`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md),
[`procesar_clases_damian()`](https://aniuxa.github.io/renoe/reference/procesar_clases_damian.md),
[`procesar_contribucion_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_contribucion_hogar.md),
[`procesar_cuidado_extra()`](https://aniuxa.github.io/renoe/reference/procesar_cuidado_extra.md),
[`procesar_estudio_trabajo()`](https://aniuxa.github.io/renoe/reference/procesar_estudio_trabajo.md),
[`procesar_libro1()`](https://aniuxa.github.io/renoe/reference/procesar_libro1.md),
[`procesar_tiempo()`](https://aniuxa.github.io/renoe/reference/procesar_tiempo.md),
[`procesar_variables_enoe()`](https://aniuxa.github.io/renoe/reference/procesar_variables_enoe.md),
[`procesar_vars_hogar()`](https://aniuxa.github.io/renoe/reference/procesar_vars_hogar.md),
[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md),
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md),
[`reglas_enoe()`](https://aniuxa.github.io/renoe/reference/reglas_enoe.md),
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)
