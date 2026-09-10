# Imputar ingresos ocupacionales con MICE

Esta función aplica imputación de ingresos mensuales (`ingocup`) para
personas ocupadas en la ENOE utilizando modelos de imputación múltiple
con el paquete `mice`. Se utiliza el logaritmo del ingreso como variable
objetivo y se imputan los valores faltantes en función de variables
donantes como edad, escolaridad, ocupación, horas trabajadas, entre
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
  ingreso (`ingocup`), variables donantes y metadatos de identificación.

- vars_donantes:

  Vector con nombres de variables que se utilizarán como predictores
  para la imputación.

- id_vars:

  Vector con nombres de variables identificadoras (por defecto:
  `folio3`, `trim`, `anio`).

- method:

  Método de imputación utilizado por `mice` (por defecto: `"pmm"`).

- seed:

  Semilla aleatoria para reproducibilidad.

- plot:

  Lógico. Si `TRUE`, se muestra un gráfico comparando la distribución
  del ingreso original vs imputado.

- anio:

  Año del trimestre, si `data` no contiene esta variable.

- trimestre:

  Trimestre del año (1–4), si `data` no contiene esta variable.

## Value

Un data frame con las variables:

- ingocup_imp:

  Ingreso mensual imputado

- log_ingocup_imp:

  Logaritmo del ingreso imputado

- imp_ingocup:

  Indicador binario de si el ingreso fue imputado (1 = sí)

## Details

La imputación se realiza primero por bloques de sexo y entidad
federativa. Los casos que no pueden imputarse dentro de esos bloques
pasan a un modelo conjunto de respaldo, en el cual el sexo y la entidad
se incorporan como covariables. Si no existen las variables `folio3`,
`anio` o `trim`, se generan automáticamente con funciones auxiliares
([`crear_folios()`](https://aniuxa.github.io/renoe/reference/crear_folios.md)
y
[`procesar_vars_sociodemo()`](https://aniuxa.github.io/renoe/reference/procesar_vars_sociodemo.md)).

La imputación de ingresos se realiza únicamente para personas ocupadas
(`clase2 == 1`) con datos válidos de edad, y en caso de estar
disponible, también de años de escolaridad (`anios_es`).

La variable a imputar es el logaritmo natural del ingreso mensual
(`log_ingocup_imp`), y la imputación se realiza utilizando el método
especificado (por defecto `"pmm"`, predictive mean matching) a través
del paquete `mice`.

Las imputaciones se hacen primero de forma separada por bloques
definidos por el sexo (`sex`) y la entidad federativa (`ent`), para
capturar mejor las heterogeneidades contextuales. Cuando un bloque no
contiene donantes o variación suficiente, sus casos pendientes se
imputan conjuntamente usando `sex`, `ent` y las demás variables donantes
disponibles como predictores. Las variables identificadoras nunca se
usan como predictores.

Las variables utilizadas como predictoras ("donantes") incluyen, si
están presentes:

- `edad`: Edad en años.

- `anios_es`: Años aprobados de escolaridad.

- `c_ocu11c`: 11 grandes grupos ocupacionales.

- `pos_ocu`: Posición en la ocupación.

- `rama_est2`: Rama de actividad.

- `ing7c`: Indicador de percepción de ingresos.

- `ent`: Clave de entidad federativa.

- `hrsocup`: Horas trabajadas a la semana.

- `t_loc`: Tamaño de localidad.

Solo se consideran aquellas variables donantes que están disponibles en
el conjunto de datos.

## See also

Other procesamiento_enoe:
[`armoniza_sinco()`](https://aniuxa.github.io/renoe/reference/armoniza_sinco.md),
[`armonizar_carreras_enoe()`](https://aniuxa.github.io/renoe/reference/armonizar_carreras_enoe.md),
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md),
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
[`sinco2019_to_sinco2011()`](https://aniuxa.github.io/renoe/reference/sinco2019_to_sinco2011.md)
