# Corrección de la fusión ENOE 2020-T1

## Naturaleza y detección

Una tabulación transversal simple de `clase_alt6_damian` detectó 3,011
personas ocupadas sin clasificación en 2020-T1. La revisión de las cinco tablas
de la publicación vigente de INEGI (VIV, HOG, SDEM, COE1 y COE2) mostró que
2,851 personas tenían `ur = 1` en SDEM y `ur = 2` en COE, aunque coincidían las
llaves personales restantes y COE contenía `P3`.

`ur` representa el ámbito y no identifica a la persona en el enlace SDEM–COE
de este trimestre. La versión 0.3.1 lo excluye sólo de esa unión. Se mantiene en
las uniones VIV–HOG y HOG–SDEM y continúa disponible como variable descriptiva.

## Resultado y pendientes explícitos

La fusión corregida conserva 409,071 registros, de los cuales 184,064 son
personas ocupadas. Entre ellas no quedan valores faltantes de `P3`. Las
clasificaciones `clase_egp13_damian` y `clase_alt6_damian` conservan 162 casos
sin asignación (factor 41,820): 133 corresponden al código oficial `9999`
(factor 34,101) y 29 no cumplen una regla EGP aplicable (factor 7,719). Estos
casos no se imputan ni se resuelven con otros trimestres del panel.

## Alcance e invalidación incremental

La fuente corregida invalida el transversal/PINI 2020-T1, los paneles 57–61 que
incluyen ese trimestre y cualquier activo clasificado, agregado, tabla, figura
o manifiesto que dependa de esos archivos. Los activos sin una ruta de
dependencia desde 2020-T1 se conservan y se verifican por identidad de
insumos, configuración, código aplicable y hashes.

La candidata interna 0.3.0 no se publicó y queda sustituida por 0.3.1. Todo
producto regenerado debe registrar versión, SHA-256 del tarball, escenario,
`run_id`, fuentes y la ruta de dependencia que justificó su invalidación.

## Controles obligatorios

La prueba de regresión `test-fusion-2020t1-regresion.R` demuestra que la llave
anterior deja `P3` sin pareja, mientras que la corregida conserva unicidad,
multiplicidad uno-a-uno, filas, suma de `fac`, universo ocupado y columnas
críticas. También verifica que la excepción no cambie las llaves de otros
trimestres.