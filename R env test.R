# Solution with vectorizations functions
N <- 1000000

# En este caso se usa sapply y unes los pasos de declaración de
# vectores y del cálculo en uno solo.

doors <- c("A", "B", "C")

# Lo que se establece al azar es la puerta del coche y la elegida.
car <- sample(x = doors, size = N, replace = T)
choice <- sample(x = doors, size = N, replace = T)

# En función de las otras dos variables, se deben establecer el resto.
# Monty no puede ser ni la elegida ni la del coche, aunque si se ha elegido
# la del coche hay dos entre las que tenemos que elegir al azar,
# por eso se usa sample.
monty <- sapply(1:N, function(i) sample(x = doors[!doors %in% c(choice[i], car[i])], size = 1)) # Size = 1 porque el bucle te lo hace sapply.

# La cerrada no puede ser monty ni la elegida.
closed <- sapply(1:N, function(i) doors[!doors %in% c(choice[i], monty[i])])
win_stay <- car == choice
win_switch <- car == closed

table(win_stay)
table(win_switch)

res <- cbind(cumsum(win_stay), cumsum(win_switch))
colnames(res) <- c("Stay", "Switch")
head(res)

matplot(
  # x al no declararlo se autoincrementa del 1 a los valores de y.
  y = res,
  # Tenemos más de una variable que represtnar, configuramos los colores.
  col = c("red", "blue"),
  # pch corresponde al caracter que se usa para represtnar cada punto.
  # Cada uno tiene un código y el 16 corresponde a puntos gorditos.
  pch = 16,
  ylab = "Number of wins", # Etiqueta del eje x.
  xlab = "Iteration", # Etiqueta del eje y.
  # cex es el número por el que se multiplica el tamaño estándar de
  # los caracteres de representación. 2 quiere decir que representa los
  # puntos el doble de grande de lo normal.
  cex = 2,
  cex.axis = 1, # Lo mismo para las marcas de eje.
  cex.lab = 1 # Los mismo para las fuentes de las etiquetas.
)

# Creamos la leyenda de la gráfica.
legend(

  # Coordenadas de la ubicación de la leyenda.
  x = 1.5e+5,
  y = 6e+5,
  c("Stay", "Switch"), # Indicadores de la leyenda.
  # Usamos el mismo caracter y colores que en la gráfica,
  # además del mismo tamaño.
  col = c("red", "blue"),
  pch = 16,
  cex = 1
)

# En esta simulación larga el resultado sale 1/3 y 2/3 casi de manera perfecta.