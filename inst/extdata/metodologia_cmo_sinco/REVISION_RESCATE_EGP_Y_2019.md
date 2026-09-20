# Rescate EGP13 y pendientes SINCO 2019

## EGP13: partir de los pendientes del camino con consenso panel

ENOE 2012-T2, 170,325 ocupados. Antes del nuevo rescate habia 159,337 casos
clasificados (93.55%) y 10,988 pendientes. La version historica propone una
clase para 10,756 de estos pendientes, pero esa cobertura no basta para aceptarla.

| Revision | Registros | Poblacion ponderada | Decision |
|---|---:|---:|---|
| Original coincide con EGP13 bajo todos los destinos oficiales | 4,308 | 1,177,766 | Incorporar con procedencia diferenciada |
| Original propone clase, pero no hay unanimidad oficial | 6,448 | 1,699,356 | Conservar propuesta para revision semantica |
| Tampoco resuelve el original | 232 | 62,700 | Mantener pendiente |

La recuperacion revisada llega a 163,645 registros (96.08%). No sobrescribe
ninguna clase resuelta por el camino actual ni completa SINCO4d. Se activa
con rescate_egp=TRUE, despues del consenso panel, para familia egp. Puede
desactivarse con FALSE. La propuesta y su estado quedan en
egp13_original_propuesto y revision_rescate_egp13. Las agrupaciones EGP solo
se completan con este rescate si tambien estaban faltantes.

El criterio de admisibilidad es especifico de EGP: el destino historico
pertenece a la relacion oficial y toda alternativa oficial produce la misma
clase EGP13 bajo pos_ocu y emple7c observados. No afirma que el destino
ocupacional original de cuatro digitos sea el verdadero.

## Revision sustantiva inicial de los casos que no se incorporan

- CMO 7201 (vendedores ambulantes): el puente original propone SINCO 4224
  (vendedores por catalogo) y EGP IIIa en 1,551 pendientes. El codigo CMO por
  si solo no acredita venta por catalogo; los otros destinos incluyen venta
  ambulante y recoleccion. No se acepta en bloque. Se requiere actividad,
  tareas y contexto de venta; p4f puede aportar evidencia auxiliar.
- CMO 5141 (supervisores/inspectores de fabricacion metalurgica y productos
  metalicos): 589 pendientes propuestos en EGP II mediante SINCO 2630.
  La denominacion de supervision no basta: EGP contiene excepciones y
  prioridades entre ISCO, supervision y posicion. Debe revisarse la frontera
  entre clase de servicio y supervision manual antes de adoptar la propuesta.
- CMO 1204 (tecnicos en ingenieria quimica, industrial y mecanica): 581
  propuestas EGP II. La ocupacion CMO agrupa especialidades con destinos
  distintos; la recuperacion requiere revisar tareas tecnicas y la
  equivalencia ISCO usada, no solo el titulo generico de tecnico.
- CMO 1250 (tecnicos en ciencias sociales): 421 propuestas EGP IIIa.
  La frontera entre tecnico y auxiliar cambia la clase; no se generaliza
  una asignacion condicionada por SCIAN a todas las personas del CMO.
- Los coordinadores/jefes de departamento de CMO 6101, 6110 y 6121 plantean
  otra frontera: direccion de servicios frente a supervision operativa.
  Posicion, tamano y funcion sustantiva deben sostener la asignacion.

Estos conteos corresponden a combinaciones CMO/destino original/clase
propuesta; las tablas detalladas conservan ademas p4a, pos_ocu y emple7c.
No se aceptaron propuestas adicionales mediante una lectura solo nominal.

## SINCO 2019: que significan los 2,042 y los 84

Son registros ocupados de ENOE 2021-T3, no cantidades de codigos diferentes.

| Codigo 2019 | Destinos 2011 en la tabla del paquete | Registros | Nivel comun identificable |
|---|---|---:|---|
| 7341 | 7341, 7352 | 1,010 | 2d: 73; 1d: 7 |
| 8154 | 8154, 8155 | 432 | 3d: 815; 2d: 81; 1d: 8 |
| 2531 | 2512, 2531 | 286 | 2d: 25; 1d: 2 |
| 2429 | 2412, 2427 | 164 | 2d: 24; 1d: 2 |
| 7342 | 7342, 7352 | 150 | 2d: 73; 1d: 7 |

Un destino multiple significa que el codigo 2019 admite mas de una
ocupacion 2011. Ejemplo: 2429 puede llevar a medicos especialistas (2412)
o fisioterapeutas, audiologos y logopedas (2427). El puente conserva esa
ambiguedad; no elige una profesion sin informacion adicional.

Los 84 registros sin equivalencia se distribuyen en diez codigos con
destino NA explicito en la tabla incluida: 5299 (29), 5319 (16), 9599 (12),
5119 (9), 7599 (6), 7199 (4), 1329 (4), 5419 (2), 7299 (1) y 1319 (1).
No son codigos de entrada faltantes. Antes de asignarlos debe cotejarse el
anexo original y las definiciones para distinguir ausencia de equivalencia
de un posible problema de transcripcion; esta auditoria no crea destinos.

El programa actual deja sin resolver tambien 3d/2d/1d cuando 4d es multiple.
La tabla muestra que 432 casos podrian recuperarse a 3d y los 2,042 a 2d/1d
sin elegir arbitrariamente un destino 4d. Es una oportunidad distinta de
recuperar EGP y no se implemento en esta revision, solicitada como explicacion.

## Reproduccion

Rscript data-raw/cmo_sinco/revisar_rescate_egp_y_2019.R RUTA_ENOE

Salidas: revision_rescate_egp13_original_2012t2.csv,
resumen_rescate_egp13_original_2012t2.csv y
desglose_pendientes_sinco2019_2021t3.csv. Para la cobertura del camino actual:
evaluar_cobertura_dos_puentes.R.
