# Inventario de dependencias de las clasificaciones ocupacionales

Revision del codigo del paquete: 2026-09-13. Describe la implementacion
actual, no una propuesta de nuevas reglas. No se modificaron clasificadores
para elaborar este inventario. Distingue insumos sustantivos de columnas
que una funcion envolvente exige por conveniencia de interfaz.

## Respuesta sobre supervision y teletrabajo

`supervisa_damian` no es insumo de `clasificar_susceptibilidad_teletrabajo()`.
Ambas salidas se construyen a partir de codigos ocupacionales, con listas
diferentes. Por ejemplo, 3201 y 3101 son ocupaciones supervisoras en Damian;
solo 3201 aparece en la lista de teletrabajo SINCO 2011. Recuperar supervision
por consenso no identifica el codigo 4d ni permite deducir teletrabajo.

Fuente: [teletrabajo](../../../R/clasificar_susceptibilidad_teletrabajo.R)
y [clasificaciones Damian](../../../R/procesar_clases_damian.R).

## Dependencias de las salidas base

| Salida | Insumo ocupacional sustantivo | Otras variables o tablas | Dependencia de otra clasificacion |
|---|---|---|---|
| skill_level | SINCO 1d base 2011 | Agrupacion interna de nueve divisiones | Ninguna |
| skill_actual | Ninguno | cs_p13_1, cs_p15 | Ninguna; es escolaridad |
| mismatch | Indirectamente SINCO 1d | clase2 | skill_level y skill_actual |
| esco_ref | SINCO 1d | anios_es, anio, trim, clase2; fac si ponderado; muestra de referencia | Ninguna; media por division y periodo |
| mismatch2 | Indirectamente SINCO 1d | anios_es, umbral; requisitos de esco_ref | esco_ref |
| grupo_ocu9_damian | SINCO 1d; se prefiere derivarlo de 4d cuando existe | Ninguna para el grupo | Ninguna |
| isco88_damian | SINCO 2011 4d | correspondencia_sinco2011_isco88_damian.csv | Ninguna |
| clase_ocu_damian | SINCO 4d via ISCO; respaldo por SINCO 1d | recuperar_sin_isco | ISCO88 o grupo_ocu9_damian |
| calificada_damian, manual_damian | Indirecto | Ninguna adicional | clase_ocu_damian |
| supervisa_damian | SINCO 2011 4d | Lista propia de codigos supervisores | Ninguna; no depende de ISCO |
| posocup_damian | Ninguno | pos_ocu | Ninguna |
| tam_est_damian | Ninguno | emple7c | Ninguna |
| clase_egp13_damian | ISCO88 y excepciones directas SINCO 2011 4d | posocup_damian, tam_est_damian; clase2 para elegibilidad cuando existe | No consume supervisa_damian ni clase_ocu_damian; usa reglas secuenciales propias |
| clase_egp7_damian, clase_alt6_damian, baja_damian, alta_damian | Indirecto | Ninguna adicional | clase_egp13_damian |
| macro_egp4_damian, macro_solis4_damian, egp3_damian | Indirecto | Ninguna adicional | clase_egp7_damian |
| class_ocu, isco_care | SINCO 3d propio del modulo care | clase2, periodo; excepcion CMO 8200 | No consumen isco88_damian |
| care_industry, care_industry_detalle | Ninguno | p4a, clase2, version SCIAN segun periodo | Ninguna |
| care_w | SINCO 3d indirectamente | Industria de cuidado | class_ocu, isco_care, care_industry |
| trabajo_cuidado_mercado | Indirecto | Nombre canonico unico | care_w y class_ocu |
| cuida_1d | Indirecto | Ninguna adicional | class_ocu y care_w |
| cuidado_posicion_remunerada, cuidado_sin_pago | Indirecto | pos_ocu | trabajo_cuidado_mercado |
| estado_ingreso_cuidado | Indirecto | ingocup, ingocup_imp, imp_ingocup, sin_pago, pos_ocu | trabajo_cuidado_mercado |
| susceptible_teletrabajo | SINCO observado 4d en la version del periodo | anio, trim y listas Cervantes 2011/2019 | No consume supervision, ISCO, EGP ni care |

Fuentes adicionales: [laborales](../../../R/procesar_vars_laborales.R),
[desajuste estadistico](../../../R/calcular_desajuste_estadistico.R),
[tipologia care](../../../R/class_cuidado_rem.R) y
[modulo cuidado](../../../R/procesar_cuidado_remunerado.R).

## Puentes, versiones y restricciones

- `armonizar_sinco()` convierte CMO hasta 2012-T2, conserva SINCO 2011 en
  2012-T3/2021-T2 y lleva SINCO 2019 a base 2011 desde 2021-T3. Sus columnas
  sinco1d/2d/3d/4d son armonizadas a base 2011.
- `cmo_to_sinco()` obtiene 3d/4d con tabla oficial, reglas panel y auxiliares
  SCIAN/p4a/p4f. El puente historico `cmo_to_sinco1d()` tiene excepciones
  que requieren pos_ocu o tue2. Consenso de clasificaciones no completa SINCO.
- `procesar_clases_damian()` exige sinco4d, pos_ocu y emple7c como columnas,
  aunque no todas sus salidas necesitan sustantivamente esos tres insumos.
  Por defecto conserva su puente CMO historico; la envolvente nueva llama
  expresamente con usar_puente_cmo=FALSE para evaluar el puente general.
- Care conserva su propio puente CMO y usa el clasificador observado desde
  2012-T3, junto con el SCIAN del periodo. No usa isco88_damian: `isco_care`
  es una agrupacion propia del modulo, no una equivalencia con ese campo.
- Teletrabajo solo devuelve resultados comparables desde 2012-T3. Desde
  2021-T3 interpreta la entrada como SINCO 2019. No debe recibir sinco4d
  armonizado a base 2011 como si fuera 2019. Para los microdatos observados
  puede explicitarse `variable_sinco = "p3coe"` y conservar anio/trim.
  La funcion clasifica por pertenencia a listas y no valida exhaustivamente
  el catalogo de entrada; su requisito sustantivo sigue siendo 4d observado.
- SINCO 2d se conserva como producto, pero no se encontro un clasificador
  sustantivo actual que lo consuma directamente. resumir_cobertura_sinco()
  usa los cuatro niveles solo para medir disponibilidad.
- `armonizar_carreras_enoe()` no utiliza SINCO como insumo de ejecucion. El
  SINCO observado solo aporto evidencia externa al construir y validar reglas
  que despues quedaron congeladas. La funcion registra esta ausencia de uso y
  rechaza entradas con marcas producidas por `armonizar_sinco()`.
- La ruta laboral aplica, cuando existen los insumos, el orden carreras
  observadas -> SCIAN -> SINCO. Esto impide usar una ocupacion ya imputada para
  reconstruir el campo de estudios con el que posteriormente se compara.

## Dependencias de la capa consenso

| Selector | Salidas | Evidencia y condiciones de aplicacion |
|---|---|---|
| 1 / egp | EGP13 y cada agrupacion | CMO + categoria historica de cada salida; calcularla requiere las reglas EGP y auxiliares pertinentes; tabla de panel especifica |
| 2 / care | Validacion de trabajo_cuidado_mercado | CMO + categoria care ya calculada con su propio puente y p4a; anade respaldo de panel, no rellena una categoria historica desconocida |
| 3 / isco88 | ISCO88 | CMO + ISCO historico; tabla de concordancia especifica |
| 4 / supervision | Supervision | CMO + indicador historico; tabla propia; no traslada su validacion a teletrabajo o EGP |
| 5 / clase_ocupacional | Grupo, clase, calificada, manual | CMO + categoria historica correspondiente; cada salida tiene reglas independientes |

La tabla de aplicacion del consenso panel usa CMO y la categoria historica.
Los requisitos adicionales de continuidad del empleo, enlaces personales,
ponderadores y particion train/validation se utilizan para generar/validar
las reglas; no se requieren como panel al aplicar una regla a una persona.
Los auxiliares necesarios para calcular su categoria historica si se requieren.

El consenso oficial alternativo evalua todos los destinos posibles bajo
pos_ocu y emple7c para las salidas Damian; cada salida exige unanimidad.
La evidencia de care sigue siendo independiente. Teletrabajo aun no forma
parte de ninguna de estas familias de consenso.

Fuente: [envolvente de clasificaciones](../../../R/procesar_clasificaciones_sinco.R).

## Orden de calculo e implicaciones detectadas

1. Las clasificaciones base Damian se calculan juntas.
2. El consenso completa cada salida faltante de forma independiente.
3. Se actualizan indicadores de cobertura; no se ejecuta de nuevo EGP ni
   las demas clasificaciones dependientes tras recuperar ISCO88.
   El rescate original revisado, incorporado posteriormente, si completa
   agrupaciones EGP faltantes cuando acepta EGP13 por unanimidad oficial.
4. Care se procesa por su via propia y recibe un indicador de respaldo.

Por tanto, recuperar ISCO88 actualmente no desencadena automaticamente una
nueva evaluacion de EGP o clase ocupacional. Recuperar EGP13 tampoco
desencadena una nueva derivacion de EGP7 y demas agrupaciones en esta capa:
pueden recuperarse separadamente, lo cual permite mas cobertura a niveles
agregados, pero exige revisar coherencia cuando varios niveles estan presentes.

La variable supervisa_damian no tiene consumidores directos encontrados en
las funciones revisadas. EGP incluye reglas conceptuales de supervision con
listas propias y prioridades distintas; sustituirlas por ese indicador
requeriria una decision metodologica, no solo conectar dos variables.

El siguiente paso de revision es definir propagacion y controles de
coherencia por estas dependencias antes de ampliar consensos. Este inventario
no implementa esas modificaciones.
