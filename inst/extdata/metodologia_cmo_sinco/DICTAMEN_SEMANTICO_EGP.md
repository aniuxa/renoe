# Dictamen de las propuestas historicas EGP13

## Actualizacion: tecnicos y categoria panel_semantico

Se incorpora `ENOE_EGP_1204_P4A8112`: CMO1204 y p4a=8112 recuperan la
categoria 2 de EGP13 propuesta por el puente historico (SINCO2621), solo
cuando EGP13 falta y no hay contradicciones con agrupaciones ya resueltas.
No requiere p4f y no rellena SINCO. Comparte los controles de capa, familia,
periodo y desactivacion de la regla 7201.

La decision autorizada es analitica, no una equivalencia demostrada. El
CMO identifica un conjunto tecnico y 8112 es compatible con reparacion y
mantenimiento, pero no elimina la ambiguedad entre especialidades. El panel
favorece EGP2 con 9 concordancias en train y 6 en validation, 67.0% y 60.9%
ponderados. Los 188 candidatos no son el tamano de la muestra del panel.

`categoria_evidencia_egp13=panel_semantico` registra esta limitacion, separada
de la capa ejecutora `enoe` y del metodo `regla_enoe`. La regla 7201 mantiene
categoria `panel` por su contraste condicionado con 75/77 concordancias.
La distincion es una revision explicita por regla, no un umbral automatico
ni una probabilidad de acierto. Las propuestas experimentales no se activan
por defecto ni se equiparan a panel_semantico. No se reclasificaron en bloque
las demas reglas historicas del paquete.

La evidencia y cobertura activa de 1204 se guardan en
`cobertura_regla_enoe_egp_1204_activa.csv`. Las objeciones de la revision inicial
que siguen abajo se conservan como antecedente y limite, no como estado de
activacion vigente.

## Actualizacion: regla ENOE integrada

La regla `ENOE_EGP_7201_P4A4690_P4F2` esta incorporada al flujo del paquete.
Requiere capa `enoe`, familia `egp`, `rescate_egp=TRUE` y correspondencia
SINCO-ISCO predeterminada. No exige activar `consenso`. Se aplica despues
de las recuperaciones anteriores, solo sobre EGP13 faltante y sin conflictos
con agrupaciones existentes. No modifica SINCO, ISCO, supervision ni cuidado.
El metodo por salida es `regla_enoe` y la revision `aceptado_regla_enoe`.

Ambito: periodo CMO de ENOE, desde 2005 hasta 2012T2. La evidencia empirica
proviene del panel 2012T2-T3; aplicarla a otros trimestres de ese periodo es
una extension analitica de la regla, no validacion longitudinal adicional.
Para desactivarla, quitar `enoe` de las capas del clasificador o utilizar
`rescate_egp=FALSE` (esto ultimo tambien desactiva el rescate original oficial).

Los apartados siguientes conservan el dictamen y las simulaciones previas.
Las menciones a 'no activada' describen esa etapa historica, no el estado actual.
La verificacion activa y su cobertura se guardan por separado en
`cobertura_regla_enoe_egp_7201_activa.csv`, reproducible con
`probar_integracion_rescate_semantico_egp.R`.

## Decision validada para integrar, todavia no activada

CMO 7201 + p4a=4690 + p4f=2: recuperar EGP3 historica en 571 registros
(123,300 personas ponderadas) actualmente pendientes. No rellenar SINCO4d
ni presentar la regla como equivalencia oficial. Limitar el rescate a EGP
faltante, propuesta historica EGP3 y destino historico 4224 admisible en la
correspondencia oficial. Conservar las clasificaciones ya resueltas.

La revision del catalogo SCIAN-Hogares 2007, aplicado en 2012, corrige una
posible lectura demasiado amplia de 4690: identifica canales como Internet,
catalogos y television, no comercio minorista generico. Combinado con CMO
7201, aporta evidencia especifica a favor de la propuesta historica de venta
por catalogo. P4f=2 incluye venta de casa en casa y en la calle; por si solo
no distingue catalogo de otras modalidades ambulantes.

En el panel de continuidad laboral 2012T2-T3, dentro de esta combinacion:

| Subconjunto | Coincidencias con SINCO4224 y EGP3 | Proporcion ponderada |
|---|---:|---:|
| Train | 75 | 92.8% |
| Validation | 77 | 95.0% |

El denominador incluye los destinos y clases discrepantes. Las proporciones
no son certeza individual ni exactitud contra una verdad externa. La
reciprocidad es poco informativa cuando la clase antigua es constante.
La seleccion de candidatos ya utilizo ambos subconjuntos; este contraste
condicionado no constituye un tercer conjunto independiente de prueba.

La pantalla marginal de lugar sugeria EGP11 para muchos de estos registros.
La evidencia especifica de actividad y lugar indica por que no debe
aplicarse ese resultado marginal como veto automatico. De los 606 candidatos
con p4a=4690, se validan aqui solo los 571 con p4f=2; los otros 35 requieren
revision propia. Los 571 no se suman a los 606 ni a los 579 conflictos de la
pantalla: son grupos superpuestos de la misma poblacion.

## Decisiones que no quedan validadas automaticamente

- 1204: reparacion/mantenimiento (8112) no separa todas las especialidades.
  En el codigo, 2637 se traduce a ISCO7241 y no necesariamente a la EGP2
  obtenida con 2621. Hace falta distinguir equipo o tareas.
- 1230: salud, posicion o tamano no demuestran por si solos que la persona
  realiza analisis de laboratorio y no control de plagas, destino oficial
  que puede cambiar EGP.
- 1250: ser subordinado o trabajar en una unidad grande no distingue
  auxiliares sociales/juridicos de auxiliares pedagogicos.
- 5380: una fabrica quimica puede tener tratamiento de agua. El sector no
  identifica el proceso que opera la persona. Debe revisarse ademas por que
  8134 figura en las reglas de supervision propias de EGP.
- 6101: el nivel educativo del establecimiento no distingue jefatura de
  area y supervision/inspeccion educativa.

Estas observaciones no rechazan las propuestas historicas para cada persona:
rechazan validarlas automaticamente con los auxiliares explorados.

## Hallazgo de implementacion que afecta al contraste agricola

Las pruebas sinteticas confirman que SINCO9111/9113 -> ISCO9211, con
pos_ocu=1 y emple7c=1, produce EGP11. En `procesar_clases_damian.R`, las
reglas generales ISCO9111:9333 se ejecutan antes de la regla agricola9211.
La primera asignacion prevalece. Esto explica una via de discrepancia frente
a destinos agricolas de la tabla historica, que producen EGP13.

No se cambio el orden: primero debe cotejarse con la fuente EGP original y
la decision conceptual del esquema. El do-file localizado en esta revision
es el puente CMO-SINCO, no el do-file EGP. El panel reproduce salidas del
codigo y no puede resolver por si solo esta cuestion de implementacion.

## Productos y alcance

### Prueba posterior de integracion simulada

`probar_integracion_rescate_semantico_egp.R` reproduce la cobertura de
referencia con todas las familias activas y los auxiliares SCIAN/p4a/p4f.
La simulacion recupera 571 casos, sin contradicciones con agrupaciones EGP
preexistentes y sin sobrescribir valores conocidos. EGP13, EGP7, alt6 y
macro_solis4 recuperan 571 faltantes cada una. Las otras agrupaciones ya
estaban resueltas y coinciden con la propuesta historica.

Se verifico que SINCO y las salidas de otras familias permanecen identicas.
El resultado simulado es 164,216 clasificados de 170,325 ocupados (96.41%),
frente a 163,645 (96.08%) en el flujo activo. Las 30 comprobaciones existentes
de `test-rescate-egp-original.R` y `test-consenso-clasificaciones.R` pasaron,
sin fallos ni advertencias de testthat.

La simulacion solo completa valores de clasificacion en memoria. No es una
activacion de la regla ni prueba de sus futuros controles de configuracion,
procedencia o cobertura; estos deberan implementarse y probarse al integrar.
Los resultados se guardan en `integracion_semantica_egp_resumen.csv` e
`integracion_semantica_egp_conflictos_agrupaciones.csv`.

`validar_decisiones_semanticas_egp.R` reproduce los contrastes, verifica los
571 casos, recorre 28 combinaciones de posicion/tamano por destino y guarda
las tablas de sensibilidad, soporte condicionado, dictamen e impacto.
Las funciones de produccion no cambiaron: la cobertura sigue en 96.08%.
Integrar exclusivamente esta regla elevaria la cobertura no ponderada a
96.41%, siempre que las pruebas de integracion confirmen ese incremento.

La habilidad de tablas se utilizo para separar evidencia y decisiones,
preservar fuentes y evitar dobles conteos. Se mantuvieron scripts R y CSV,
como se solicito para la reproducibilidad del paquete; no se creo un libro Excel.

Fuentes: tabla comparativa oficial incluida en
`data-raw/cmo_sinco/inputs/cmo_sinco2011_oficial_largo.csv`; catalogo local
verificado `scian_hogares_enoe/inputs/oficial/catalogo_2007.csv`, procedente de
`clasificaciones_enoe_2012.pdf`, paginas 9-16; codigo R del paquete y do-file
historico identificado en la tabla de trazabilidad. Para las modalidades de
lugar, vease tambien la [reconstruccion historica de variables ENOE de INEGI](https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/metodologias/est/702825001356.pdf),
apartado XI.5. Este codigo de actividad y las preguntas auxiliares son propios
de la ENOE y su version; no se deben trasladar a otras encuestas o revisiones
del catalogo sin verificar sus equivalencias.
