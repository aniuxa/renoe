# Concordancia analitica CMO-SINCO para cuidado

Archivo: `concordancia_cmo_sinco_cuidado.csv`.

Origen inmediato: los tres vectores de `R/cmo_to_sinco11_care.R` existentes
antes de la integracion del modulo. Esa implementacion atribuye la concordancia
al programa `cmo a sinco11.do` de Damian (material del proyecto, 2026).
Se conservan esa atribucion y el orden, sin atribuirle validacion oficial.

- `cmo`: 447 claves unicas del catalogo embebido original.
- `sinco11`: destino seleccionado por la primera regla.
- `n_destinos`: numero de destinos reportado por la implementacion anterior.

No es una tabla larga de todos los destinos ni una homologacion general.
La comparacion contra la implementacion anterior verifico las 447 salidas,
las etiquetas y los casos faltante y sin correspondencia. La validacion
sustantiva de destinos multiples debe hacerse con los materiales del articulo.
