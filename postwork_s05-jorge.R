"
Sesión 5
Postwork

Requisitos
- Haber desarrollado los postworks anteriores
- Cubrir los temas del prework
- Replicar los ejemplos de la sesión

Desarrollo

Instrucciones

El data frame iris contiene información recolectada por Anderson sobre 50 flores
de 3 especies distintas (setosa, versicolor y virginca), incluyendo medidas en
centímetros del largo y ancho del sépalo así como de los pétalos.

Estudios recientes sobre las mismas especies muestran que:

I. En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm

II. En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm

III. En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande 
     que el promedio del largo del pétalo de la especie versicolor.

IV. En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.

Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente 
para concluir que los datos recolectados por Anderson están en línea con los nuevos estudios.

Utiliza 99% de confianza para todas las pruebas, en cada caso realiza el planteamiento
de hipótesis adecuado y concluye"


"
I. En promedio, el largo del sépalo de la especie setosa es 5.7 cm

   Ho: mu = 5.7
   Ha: mu != 5.7
"

setosa.sepal.length <- iris[iris$Species == "setosa", "Sepal.Length"]
t.test(x = setosa.sepal.length, alternative = "two.sided", mu = 5.7)

"
La media de la muestra es de 5.006 cm.
El p-value de la prueba t resulta muy pequeño (menor al orden de 10^-16), esto
representa una probabilidad casi nula de que una población con una media verdadera
de 5.7 cm produjera una muestra con la media observada.
Como la confianza seleccionada es 99%, se rechaza la hipótesis nula (se requería
un p-value menor a 0.01).
Se concluye que la muestra de Anderson es inconsistente con los nuevos estudios,
para este parámetro (longitud del sétalo de la especie setosa) a un nivel de 
confianza de 99%.
"

"
II. En promedio, el ancho del pétalo de la especie virginica es menor a 2.1 cm

    Ho: mu >= 2.1
    Ha: mu < 2.1
"

# Vamos a usar dplyr para seleccionar los datos en esta ocasión
library(dplyr)

virginica.petal.width <- 
  iris %>%
  filter(Species == "virginica") %>%
  select(Petal.Width)

t.test(x = virginica.petal.width, alternative = "less", mu = 2.1)

"
La media del ancho del pétalo para la especie virgínica en la muestra es de 2.026 cm.
El p-value obtenido en la prueba t es de 0.03132. Esto representa una probabilidad
del 3.1% de que una población con una media verdadera mayor a 2.1 cm hubiera
producido producido esta muestra con una media menor.
Como el nivel de significancia seleccionado es del 99%, este p-value es insuficiente
para rechazar la hipótesis nula (se hubiera requerido un valor menor a 0.01) y
se concluye que no hay evidencia significativa para suponer que la media poblacional
no sea mayor o igual a 2.1 cm.
De nuevo, se observa que la muestra de Anderson no es consistente con los nuevos
hallazgos en lo que respecta al ancho del pétalo para la especie virgínica.
"

"
III. En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande 
     que el promedio del largo del pétalo de la especie versicolor.
     
     Ho: mu(virginica.petal.length) - mu(versicolor.petal.length) = 1.1
     Ha: mu(virginica.petal.length) - mu(versicolor.petal.length) < 1.1
     
     Se seleccionó la hipótesis alternativa con símbolo menor que en lugar de 
     diferente por el planteamiento de que el largo del pétalo en virgínica es
     *mayor*. Me pareció un mejor contraste (no es mayor).
"

virginica.petal.length <-
  iris %>%
  filter(Species == "virginica") %>%
  select(Petal.Length)

versicolor.petal.length <-
  iris %>%
  filter(Species == "versicolor") %>%
  select(Petal.Length)

# Primero, verificamos igualdad de varianzas
var.test(
  virginica.petal.length[,1],
  versicolor.petal.length[,1],
  ratio=1,
  alternative = "two.sided",
  conf.level = 0.99
)

# NOTA:
# Al usar directamente las variables con los datos: virginica.petal.length y 
# versicolor.petal.length, se obtenía un error:
# Error in is.finite(x) : default method not implemented for type 'list'
# (las variables son data.frames).
# Se probó usando c() para convertir los data.frames en vectores, pero se obtiene
# el mismo error y c(virginica.petal.length) sigue siendo tipo list.
# Para eliminar el error se extrajo la (única) columna usando la notación:
# virginica.petal.legth[,1]

# El p-value obtenido es 0.2637, por lo que no se rechaza la hipótesis nula y se
# concluye que no hay evidencia para suponer que las varianzas seas diferentes
# (se hubiera requerido un p-value menor a 0.01).

t.test(
  x = virginica.petal.length,
  y = versicolor.petal.length,
  alternative = "less",
  mu = 1.1,
  var.equal = TRUE, 
  conf.level = 0.99
)


# El p-value es muy grande (0.968), lo que era de esperarse dada la forma como se planteó
# la hipótesis alternativa (que la diferencia fuera menor a 1.1 cm) siendo que,
# desde un principio, la diferencia en las muestras observada ya es mayor a 1.1 cm.
# En este caso, los datos de Anderson sí son consistentes con los nuevos datos
# al nivel de confianza seleccionado.

"
IV. En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.

    Ho: mu(setosa.sepal.width) = mu(versicolor.sepal.width) = mu(virginica.sepal.width)
    Ha: Al menos una de la mu es diferente
    
    La prueba t de Student no es apropiada para comparar las medias de más de dos grupos,
    para esto se utiliza el análisis de varianza (ANOVA).
    En este caso en particular, se desea saber si la especie afecta el ancho del
    sépalo.
"
summary(
  aov(
    Sepal.Width ~ Species,
    data = iris
  )   
)

# El valor p de la prueba F es extremadamente pequeño (órdenes de magnitud menor al
# requerido por el nivel de significancia: 0.01), por lo que se rechaza la hipóteis
# nula y se concluye que la media de los anchos de sépalo difieren entre especies,
# por lo que, de nueva cuenta, las observaciones de Anderson no son consistentes con
# los nuevos datos.

# Los siguientes box plots ejemplifican las diferencias entre las especies:

boxplot(Sepal.Width ~ Species,
        data = iris
)

