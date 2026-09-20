# Revision de EGP13 desde las reglas existentes

El punto de partida son las propuestas del puente historico, no un nuevo
clasificador. `trazabilidad_reglas_historicas_egp13.csv` identifica, para cada CMO
pendiente, el destino historico, la linea efectiva del do-file, las alternativas
posteriores, la fila del CSV usado por `cmo_to_sinco11_care()` y los valores de
la tabla general usada por `cmo_to_sinco()`. Los valores de esta ultima se
conservan sin normalizar para hacer visible su almacenamiento historico.

El script `data-raw/cmo_sinco/trazar_reglas_historicas_egp.R` verifica que los
6,448 registros pendientes coinciden con el primer destino Stata y con el
puente care. La condicion `sinco11 == -1` impide que las reglas posteriores
revisen esa primera asignacion: mayor cobertura no equivale por si sola a
mejor identificacion ocupacional. Este paso del puente no usa SCIAN ni otras
preguntas ENOE; la construccion posterior de EGP si utiliza auxiliares.

## Evidencia auxiliar exploratoria

El script `explorar_auxiliares_egp_semanticos.R` parte de la EGP historica y
examina las variables disponibles frente a EGP del periodo siguiente del
panel. Sus tablas no activan reglas de produccion ni acreditan validacion
semantica. Entre los 6,448 pendientes:

- 961 tienen propuestas estadisticas no contradictorias que conservan la EGP
  original: CMO 1250 (386), 1230 (216), 1204 (188), 5380 (81), 6101 (63) y
  7201 (27).
- 1,225 tienen propuestas no contradictorias que cambian la EGP original.
- 579 reciben propuestas contradictorias entre auxiliares; todos son 7201.
- 3,683 no tienen candidatos bajo esta pantalla.

Estos grupos son personas deduplicadas; no sumar las aplicaciones de reglas
superpuestas. La mayoria superior a 50% en ambos subconjuntos es una pantalla,
no una garantia conceptual. Se seleccionan candidatos usando train y
validation, por lo que validation no es una prueba independiente posterior
a la seleccion. Cuando la clase historica es constante dentro de un grupo,
la reciprocidad puede resultar poco informativa.

## Criterio para la siguiente decision

Revisar primero los 961 candidatos que preservan la decision existente:
comparar las tareas del CMO y de sus destinos oficiales, identificar la
distincion relevante para EGP, y exigir que la pregunta ENOE realmente mida
esa distincion. Sector, posicion o lugar pueden apoyar la decision, pero no
demuestran por si solos especialidad, jerarquia o venta por catalogo. Para
7201, las contradicciones entre actividad y lugar exigen especial cautela.

Las propuestas que cambian EGP requieren ademas auditar la prioridad de las
reglas de EGP; una coincidencia con la salida del codigo no prueba que su
interpretacion sustantiva sea correcta. No se recuperan automaticamente.

Esta revision no modifica la cobertura obtenida tras el rescate oficial:
163,645 de 170,325 ocupados (96.08% sin ponderar) en 2012T2. Los nuevos
candidatos son potencial de revision, no recuperaciones realizadas.
