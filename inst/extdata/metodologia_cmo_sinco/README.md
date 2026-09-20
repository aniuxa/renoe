# Expediente metodológico CMO-SINCO

Este directorio contiene las tablas distribuidas con renoe para auditar la
armonización CMO-SINCO. No se editan manualmente: se regeneran con los scripts
de data-raw/cmo_sinco.

## Arquitectura aceptada

Todo el sistema permanece por ahora dentro de `renoe`. Se mantienen modulos
separables para motor comun, tablas versionadas, adaptadores ENOE y
clasificaciones consumidoras, sin dependencias circulares. La extraccion a un
paquete independiente se reconsiderara al incorporar sistematicamente ENIGH,
ENUT u otras fuentes y despues de estabilizar la API.

Los armonizadores siguen el principio `interfaz comun, evidencia visible`:
incluso la salida compacta conserva calidad y regla; la salida auditable agrega
estado, fase, auxiliares, destinos plausibles y motivo de pendiente. La misma
convencion ya esta integrada para carreras: el SINCO observado pudo aportar
evidencia al construir reglas congeladas, pero no es insumo de ejecucion.
`armonizar_carreras_enoe()` rechaza las marcas de un SINCO ya armonizado y la
ruta laboral aplica carreras observadas, SCIAN y despues SINCO.

## Alcance

- general_classifiers: equivalencia oficial única o convergencia oficial; no
  depende del diseño de la ENOE.
- validacion_panel_ENOE: hallazgo de esta revisión validado con el panel,
  ponderador y partición train-validation; su aplicación usa solamente CMO.
- aplicacion_auxiliar_ENOE: regla que sí necesita SCIAN, p4a o p4f.
- puente_historico_ENOE: puente agregado previo; algunas excepciones usan
  pos_ocu o tue2 y no constituye una equivalencia oficial completa.

## Regla de decisión

Las funciones de armonizacion y procesamiento aceptan `capas`: `oficial`,
`panel`, `enoe` y `consenso`. Todas estan
activas por defecto. `nivel_maximo_sinco` informa la desagregacion alcanzada;
`capas_sinco_activas` registra la configuracion y las variables de regla
identifican la evidencia aplicada. `resumir_cobertura_sinco(x, "fac")`
resume cobertura por nivel. Una convergencia 3d nunca se cuenta como 4d.

Consenso compara la misma clasificacion calculada con el puente historico
CMO en 2012-T2 y con SINCO observado en 2012-T3, en personas con empleo
continuo. Se aplica solo a pendientes y por clasificacion: una agrupacion
puede recuperarse aunque su clase detallada siga sin resolver. La seleccion
exige concordancia de categorias, destino historico en la tabla oficial,
mayoria ponderada reciproca >50%, estabilidad y al menos cinco pares en
train y validation. Validation participa en la seleccion de reglas; no es
una evaluacion independiente posterior. Cobertura no equivale a exactitud.

`procesar_clasificaciones_sinco(x)` activa el consenso de panel.
`metodo_consenso = "oficial"` permite comparar unanimidad de destinos
oficiales, como alternativa. `procesar_vars_laborales()` aplica por defecto
la opcion de panel al final del procesamiento. No se usa el primer destino
historico sin validacion para rellenar pendientes de esta capa.

En la ruta analitica, `procesar_vars_laborales()` y
`procesar_variables_enoe()` activan por defecto
`rescate_egp_damian_no_validado = TRUE` para el libro y los analisis de
Gerardo. Recupera exclusivamente EGP aun faltante, no sobrescribe resultados
mejor sustentados y no completa SINCO ni ISCO88. Su procedencia queda marcada
como `legado_no_validado`; el motor de bajo nivel conserva la opcion apagada.

`consenso` selecciona familias de manera independiente: 1/"egp", 2/"care",
3/"isco88", 4/"supervision", 5/"clase_ocupacional". Acepta varios valores;
NULL activa todas y character(0) ninguna. Care mantiene su clasificacion
propia y declara respaldo empirico mediante consenso_validado_care y
regla_consenso_care. Sus 270 reglas no heredan la evidencia de EGP.

La pantalla de panel no sustituye la revisión conceptual. Una regla automática
requiere simultáneamente admisibilidad estadística y coherencia sustantiva.
Una mayoría ponderada puede rechazarse cuando el auxiliar no distingue el
contenido ocupacional; una correspondencia conceptualmente plausible permanece
pendiente cuando no es estable o no alcanza el tamaño mínimo. Los rechazos se
conservan como resultados auditables.
## Archivos principales

- `NOTA_METODOLOGICA_ARMONIZACION_SINCO.md`: nota canonica de decisiones,
  evidencia, precedencia, consenso, riesgo y limites de uso.
- `registro_decisiones_metodologicas.csv`: registro legible por maquina de
  cada decision metodologica, incluida la sensibilidad no aceptada, su
  implementacion y limitacion.

- `cobertura_sensibilidad_legado_damian_egp_2012t2.csv` y
  `sensibilidad_legado_damian_egp_por_cmo_2012t2.csv`: ganancia separada de
  cobertura y reglas CMO del legado EGP no validado.

- `cobertura_maxima_decisiones_sinco.csv`: cobertura muestral y ponderada de
  todas las salidas en el maximo nivel de decisiones aceptadas, con nivel
  requerido, capas, riesgo y recomendacion.
- `COBERTURA_MAXIMA_DECISIONES.md`: sintesis legible del techo actualmente
  integrado; distingue disponibilidad de salida de exactitud.

- REVISION_RESCATE_EGP_Y_2019.md: decisiones del rescate original EGP13 y
  explicacion por codigo de los pendientes 2019. El rescate acepta solo
  propuestas originales coincidentes con todos los destinos oficiales.
- revision_rescate_egp13_original_2012t2.csv y resumen_rescate_egp13_original_2012t2.csv:
  propuestas por CMO/actividad/posicion/tamano y estado de revision.
- desglose_pendientes_sinco2019_2021t3.csv: cinco codigos con equivalencias
  multiples y diez con destino faltante en la tabla del paquete.

- COBERTURA_DOS_PUENTES.md y cobertura_efectiva_dos_puentes.csv: ejecucion
  efectiva con todas las capas y familias en 2012-T2 y 2021-T3; registros,
  ponderaciones y pendientes. Reproducir con evaluar_cobertura_dos_puentes.R.
- calidad_sinco_dos_puentes.csv y procedencia_clasificaciones_dos_puentes.csv:
  causas de pendientes del puente y metodo de cada clasificacion.
- diagnosticos_cobertura_dos_puentes.csv: codigos no especificados y estado
  de ingreso entre quienes fueron clasificados como cuidadores de mercado.

- INVENTARIO_DEPENDENCIAS.md: inventario revisado de insumos directos,
  dependencias entre salidas, versiones ocupacionales y orden de consenso.

- reglas_enoe_aceptadas.csv: 33 reglas, evidencia ponderada y estabilidad.
- convergencias_oficiales_3d.csv: CMO ambiguos a 4d que convergen a 3d.
- equivalencias_oficiales_perfil.csv: perfil de ambigüedad por CMO.
- equivalencias_oficiales_largo.csv: relación oficial completa.
- diccionario_capas.csv: precedencia, nivel resultante y alcance.
- pantalla_remanentes_enoe.csv: candidatos examinados y motivo de rechazo.
- disponibilidad_auxiliares_pendientes.csv: disponibilidad de SCIAN, p4a y p4f.
- cobertura_convergencias_2012t2.csv: conteo y población de convergencias.
- cobertura_niveles_y_escenarios_2012t2.csv: cobertura 1d-4d por escenario.
- cobertura_consumidores_sinco_2012t2.csv: skill_level, mismatch y mismatch2.
- cobertura_clasificaciones_dependientes_sinco_2012t2.csv: aplicación y cobertura de cada clasificación que consume SINCO.
- aplicacion_reglas_enoe_2012t2.csv: reglas efectivamente aplicadas.
- inventario_consumidores_sinco.csv: módulos del paquete dependientes de SINCO.
- reglas_consenso_clasificaciones_panel.csv: 3,079 reglas por CMO,
  clasificacion y categoria historica, con evidencia y criterios.
- pantalla_consenso_clasificaciones_panel.csv: candidatos y rechazos.
- transiciones_clasificaciones_panel.csv: pares de categorias por split.
- cobertura_validacion_care_panel_2012t2.csv: cobertura del puente propio
  de cuidado y respaldo de panel, distinguiendo cuidado y no cuidado.
- comparacion_fusion_egp_2012t2.csv, auditoria_fusion_egp_por_cmo_2012t2.csv
  y transiciones_egp_general_propio_2012t2.csv: diagnostico de puente propio,
  consenso oficial e hibrido; el hibrido sin validacion no es el predeterminado.
- auditoria_cmo_nivel_por_capa_2012t2.csv: cobertura por codigo y capa.
- auditoria_destinos_y_consensos.csv: destinos oficiales y coincidencias
  entre sus resultados; los consensos son candidatos para revision, no
  reglas nuevas aplicadas automaticamente.
- matriz_requisitos_granularidad_sinco.csv: nivel minimo y auxiliares por
  clasificacion, incluida la de cuidado remunerado con su puente propio.

## Reproducción

1. Ejecutar data-raw/cmo_sinco/generar_tablas_metodologicas.R.
2. Definir RENOE_ENOE_PROJECT_ROOT con la ruta a los microdatos de panel.
3. Ejecutar con Rscript evaluar_panel_enoe.R, evaluar_cobertura_consumidores.R
   y evaluar_granularidad_dependencias.R, pasando la ruta ENOE como argumento.
4. Ejecutar las pruebas del paquete.

Para reproducir consenso, ejecutar evaluar_consenso_clasificaciones_panel.R
antes de evaluar_cobertura_consumidores.R. evaluar_fusion_egp.R reproduce
la comparacion diagnostica con el puente propio de Damian.

Nota: el nombre correcto de la variable de ambiente en R usa solamente
caracteres ASCII: RENOE_ENOE_PROJECT_ROOT.
