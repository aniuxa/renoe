# Migración de renoe 0.1.4 a renoe 0.2.0

`renoe 0.2.0` incorpora soporte para los trimestres publicados de 2026,
cambia varias definiciones metodológicas y conserva salidas de
transición cuando es posible. Esta guía resume los cambios que requieren
revisar código o resultados.

## ENOE 2026-T1 y 2026-T2

[`info_trimestre()`](https://aniuxa.github.io/renoe/reference/info_trimestre.md)
reconoce 2026-T1 como cuestionario ampliado (`v6a`) y 2026-T2 como
cuestionario básico (`v7`). La carga normaliza los nuevos nombres
geográficos `cve_ent`, `cve_mun`, `cve_loc` y `cve_ageb` a los nombres
históricos, conserva `cvegeo` y mantiene ceros iniciales en localidad y
AGEB. Los trimestres 2026-T3 y T4 no están habilitados mientras no hayan
sido publicados y auditados.

El recurso interno de IPC cubre de 2005-T1 a 2026-T2.
[`ipc_enoe()`](https://aniuxa.github.io/renoe/reference/ipc_enoe.md)
ahora valida esquema, unicidad y dominio, y falla explícitamente si
falta el trimestre pedido.

Los calendarios de procesamiento y seguimiento admiten 85 trimestres
disponibles. Los paneles proyectados al cierre son el 81 (2025-T1 a
2026-T1) y el 82 (2025-T2 a 2026-T2). La versión del paquete no
reconstruye esos productos: la fusión, el procesamiento y la generación
de paneles deben ejecutarse después, en una operación controlada que
conserve las salidas anteriores.

## Uso del tiempo

Las duraciones 98 y 99 ya no se convierten silenciosamente en cero. La
salida principal distingue duración desconocida, realización
desconocida, reactivo no seleccionado y batería no medible. Para
reproducir temporalmente la definición anterior use
`tratamiento_faltantes = "historico_cero"` o consulte las columnas con
sufijo `_legacy`. Revise los totales parciales y sus banderas de
incompletitud antes de comparar con bases procesadas con 0.1.4.

## Desajuste educativo

[`procesar_vars_laborales()`](https://aniuxa.github.io/renoe/reference/procesar_vars_laborales.md)
ya no calcula por sí solo la medida estadística completa. Llame después
a
[`calcular_desajuste_estadistico()`](https://aniuxa.github.io/renoe/reference/calcular_desajuste_estadistico.md):

``` r

datos <- procesar_vars_laborales(datos)
datos <- calcular_desajuste_estadistico(
  datos,
  periodo_referencia = "trimestre"
)
```

`mismatch` y `mismatch2` usan `-1` para sobreeducación, `0` para ajuste
y `1` para subeducación. `esco_ref` nombra la referencia estadística
observada. La referencia anual sólo debe calcularse después de acumular
los trimestres; la unidad es persona-trimestre y no se deduplican
personas automáticamente.

## Cuidado de mercado

Use `trabajo_cuidado_mercado` como indicador principal. En 0.2.0,
`trabajo_cuidado_rem` se mantuvo como alias transitorio; 0.3.0 lo retira
y conserva únicamente `trabajo_cuidado_mercado`. La posición remunerada,
el trabajo sin pago y la observación o imputación del ingreso son
dimensiones separadas; un ingreso positivo imputado no se presenta como
remuneración observada. Los casos fuera del universo ocupado o sin
clasificación medible permanecen como `NA`.

## Etiquetas y productos derivados

Los diccionarios CSV distribuidos con el paquete son el contrato
portable de etiquetas. Use
[`aplicar_etiquetas_enoe()`](https://aniuxa.github.io/renoe/reference/aplicar_etiquetas_enoe.md)
antes de exportar a formatos que admitan esos metadatos. No asuma que
todos los lectores externos interpretan atributos específicos de R
conservados en Parquet.

Las bases `enoe_fusion_*`, `enoe_procesa_*` y los paneles no se
actualizan al instalar 0.2.0. Deben regenerarse por separado, conservar
versiones anterior y nueva y comparar filas, universos, ponderadores y
variables afectadas.
