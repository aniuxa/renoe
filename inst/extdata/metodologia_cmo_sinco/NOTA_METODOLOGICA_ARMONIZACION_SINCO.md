# Nota metodologica: armonizacion ocupacional y clasificaciones dependientes

## Estado y recomendacion

Las capas `oficial`, `panel`, `enoe` y `consenso` permanecen activadas por
defecto en `armonizar_sinco()` y en el procesamiento interno de `renoe`. Esta es
una decision aceptada e integrada, no una activacion experimental. Cada
observacion conserva la calidad, el nivel maximo sustentado, la regla aplicada,
la capa, la evidencia y, cuando corresponde, el motivo de pendiente.

El maximo publicado incluye exclusivamente decisiones aceptadas e integradas.
Los candidatos rechazados o pendientes se conservan para sensibilidad, pero no
entran en la cobertura predeterminada.

## Secuencia de decision

1. Se identifica el catalogo observado por periodo: CMO hasta 2012-T2, SINCO
   2011 entre 2012-T3 y 2021-T2, y SINCO 2019 desde 2021-T3.
2. Se conservan el codigo y catalogo de origen.
3. Se aplican equivalencias oficiales unicas al mayor nivel disponible.
4. En CMO se aplican reglas globales validadas por panel y luego reglas ENOE
   condicionadas por SCIAN, p4a o p4f.
5. Si los destinos oficiales convergen solo a 3d, 2d o 1d, se informa ese nivel
   sin presentarlo como equivalencia 4d.
6. Las clasificaciones consumidoras se calculan con sus requisitos propios.
7. El consenso se aplica por clasificacion y solamente a resultados faltantes;
   nunca inventa SINCO4d.
8. EGP puede aplicar despues rescates aceptados por unanimidad oficial o por
   reglas ENOE validadas. Tampoco estos rescates rellenan SINCO o ISCO.

## Evidencia de panel y criterio conceptual

Una regla de panel exige destino oficialmente admisible, mayoria ponderada
unica en ambas direcciones, proporciones superiores a 50%, al menos cinco
pares concordantes en train y validation y estabilidad entre subconjuntos.
Validation forma parte de la seleccion; no es una evaluacion externa.

La pantalla estadistica no sustituye el juicio conceptual. SCIAN, actividad y
lugar de trabajo solo se usan cuando distinguen sustantivamente los destinos.
El panel puede respaldar una regla semantica con muestra limitada; en ese caso
se declara `panel_semantico`, no evidencia de panel plena.

## Consenso de clasificaciones

El consenso predeterminado compara la misma clasificacion bajo el puente
historico CMO y el SINCO observado del panel. Se decide separadamente para
ISCO-88, supervision, EGP13 y cada agrupacion EGP, clase ocupacional y cuidado.
Por eso una agrupacion puede resolverse aunque EGP13 permanezca pendiente.

En 2012-T2, el consenso de panel eleva ISCO-88 de 86.03% a 88.40%, supervision
de 86.28% a 99.22% y EGP13 de 86.33% a 93.55%. Los rescates EGP posteriores
elevan EGP13 a 96.52%. La unanimidad de destinos oficiales se conserva como
escenario de sensibilidad, no como sustituto silencioso del consenso de panel.

## Decisiones EGP especificas

- `ENOE_EGP_7201_P4A4690_P4F2` recupera EGP13=3 para faltantes compatibles con
  venta por catalogo. Combina evidencia de panel y coherencia conceptual.
- `ENOE_EGP_1204_P4A8112` recupera EGP13=2. Se clasifica como
  `panel_semantico` porque el respaldo muestral es limitado.
- El rescate historico general solo acepta la propuesta si todos los destinos
  SINCO oficiales producen la misma EGP13 bajo posicion y tamano observados.

Estas reglas conservan las clasificaciones resueltas y no completan SINCO4d.

## Sensibilidad con el legado de Damian

Las decisiones historicas de Damian que aun proponen EGP, pero que no alcanzan
justificacion suficiente del panel ni unanimidad oficial, se activan por
defecto en `procesar_vars_laborales()` y `procesar_variables_enoe()` para el
libro y los analisis de Gerardo. El motor de bajo nivel
`procesar_clasificaciones_sinco()` conserva el argumento apagado por defecto,
para que una llamada metodologica directa no incorpore el legado inadvertidamente.

La capa se ejecuta despues de todos los rescates aceptados, rellena solamente
EGP faltante y nunca sobrescribe una clasificacion resuelta. No completa
SINCO4d ni ISCO88. Cada caso queda identificado con metodo
`legado_damian_no_validado`, evidencia `legado_no_validado` y una regla
`LEGADO_DAMIAN_EGP_<CMO>`. Sus resultados deben presentarse como una
procedencia analitica diferenciada, no como aumento de validez o equivalencia
oficial. Para reproducir el maximo sin legado se usa
`rescate_egp_damian_no_validado = FALSE`.

En ENOE 2012-T2 recupera 5,689 registros (1,524,222 personas ponderadas):
EGP13 pasa de 96.52% a 99.86% por registros y de 96.75% a 99.87% ponderado.
Permanecen 232 registros sin propuesta historica. La ganancia es de cobertura,
no una estimacion de exactitud.

## Riesgo y uso

En el tramo CMO se consideran de riesgo alto SINCO4d, ISCO-88 y EGP13. Deben
presentarse con estratificacion por evidencia y sensibilidad. SINCO3d, las
agrupaciones EGP, supervision, cuidado y desajuste tienen riesgo intermedio.
Los grandes grupos y las clases manual/calificada son mas estables, pero su
recuperacion agregada tampoco constituye una equivalencia oficial detallada.

Desde SINCO 2019, el riesgo general es menor porque el puente es oficial, pero
las correspondencias multiples permanecen sin resolver. El consenso del panel
no se aplica actualmente a ese tramo. Teletrabajo solo es comparable desde
2012-T3 y su cobertura computacional no demuestra validez de todos los codigos.

## Procedencia y reproducibilidad

El registro completo de decisiones se encuentra en
`registro_decisiones_metodologicas.csv`. Las decisiones individuales de reglas
se conservan en `reglas_enoe_aceptadas.csv`,
`reglas_consenso_clasificaciones_panel.csv` y las tablas de pantalla y rechazo.
La cobertura maxima y el riesgo se documentan en
`cobertura_maxima_decisiones_sinco.csv`.

El efecto separado del legado no validado se reproduce con
`data-raw/cmo_sinco/evaluar_sensibilidad_legado_damian_egp.R` y se documenta
en `cobertura_sensibilidad_legado_damian_egp_2012t2.csv` y
`sensibilidad_legado_damian_egp_por_cmo_2012t2.csv`.

La arquitectura permanece dentro de `renoe`, con limites entre motor, tablas
versionadas, adaptadores ENOE y consumidores. Una separacion se reconsiderara
cuando se incorporen sistematicamente ENIGH, ENUT u otras fuentes y la API este
estabilizada.
