##IMPORTANTE##
# Hay que tener instalada la librería "mixtools"
#install.packages("mixtools")

require("mixtools")

## logaritmo_varianza - Función para obtener set aleatorios con la varianza y 
#                       media que queramos de una distribución logarítmica
#                       normal centrada

# INPUTS - n (número de datos por set), delta (para obtener datos con medias
#          diferentes) y varianza (para cambiar la varianza en un set)

# OUTPUTS - dato (vector con un set de datos)

logaritmo_varianza  <- function(n, varianza, delta) {
  
  datos <- rlnorm(n, 0, 1) - exp(0.5) 
  # Se resta la media teórica exp(0.5) para obtener una media centrada en 0 y 
  # poder comparar los errores tipo I
  
  # Hacemos que la varianza sea 1 al dividir por la desviación teórica
  num_estandar <- datos / sqrt((exp(1) - 1) * exp(1)) 
  
  # Obtenemos datos con la varianza^2 y media (delta) que indiquemos
  dato <- num_estandar * varianza + delta
 
 return(dato)
}


## logaritmo_varianza - Función para obtener set aleatorios con la varianza y 
#                       media que queramos de una mezcla bimodal simétrica
#                       centrada

# INPUTS - n (número de datos por set), delta (para obtener datos con medias
#          diferentes) y varianza (para cambiar la varianza en un set)

# OUTPUTS - dato (vector con un set de datos)

bimodal_varianza <- function(n, varianza, delta) {
  
  # Del paquete mixtools
  datos <- rnormmix (100, lambda = c(0.5, 0.5), mu = c(-1, 1), sigma = c(1, 1))
  
  # lambda c(0.5, 0.5) es para que sea simétrica
  # mu y sigma controlan la media y la varianza, se deja en 1 ya que,
  # normalizamos a media 0 y varianza 1
  
  # Normalizamos la varianza que es la suma de sigma^2 más mu^2 que siempre da 2
  num_estandar <- datos / sqrt(2)
  dato <- num_estandar * varianza + delta
  
}


## simulación - Función que sirve para obtener datos aleatiros de diferentes
#               distribuciones y hacer las simulaciones con el método perverso

# INPUTS - n (número de datos por set), delta (para obtener datos con medias
#          diferentes), distribucion (seleccionar el tipo de distribución) y 
#          varianza (para cambiar la varianza en un set)

# OUTPUTS - Vector con 4 resultados, los 3 primeros pueden ser TRUE/FALSE
#           e indican si el test de la T, Wilcoxon y los de normalidad han dado
#           valores p menores a 0.05, el último puede ser 1/0 dependiendo si se
#           escogido o no el test de la T

simulacion <- function(n, delta, distribucion, varianza) {   
  if (distribucion == 1) {  # Normal
    x <- rnorm(n, 0, 1)     # Si delta = 0 ambas muestras misma distribución
    y <- rnorm(n, delta, varianza) # Si delta ≠ 0, las medias difieren 
    
  } else if (distribucion == 2) { # Exponencial centrada
    # Se resta la media teorica (1) para que este centrada. Si no está centrada, 
    # aun con delta = 0  el error tipo I no es interpretable
    
    x <- rexp(n) - 1                
    y <- (rexp(n) - 1) * varianza + delta
    
  } else if (distribucion == 3) {# Distribución de la T con 3 grados de libertad
    # La varianza es 1, para hacer comprarables los tamaños de efecto entre 
    # distribuciones y evitar que diferencias de escala influyan en la potencia
    
    x <- rt(n, 3) / sqrt(3) # Normalizamos a varianza = 1
    y <- (rt(n, 3) / sqrt(3)) * varianza  + delta
    
  } else if (distribucion == 4) {     # Log-normal centrada
    x <- logaritmo_varianza(n, 1, 0)
    y <- logaritmo_varianza(n, varianza, delta)
    
  } else if (distribucion == 5) {     # Mezcla bimodal simétrica centrada
    x <- bimodal_varianza(n, 1, 0)
    y <- bimodal_varianza(n, varianza, delta)
    
  }
  
  p_t <- t.test(x, y)$p.value   # Se hace test de Welch
  p_w <- wilcox.test(x, y, exact = FALSE)$p.value 
  # exact = FALSE para que el cálculo sea más rápido
  
  # Hacemos los test de normalidad
  p_norm_x <- shapiro.test(x)$p.value
  p_norm_y <- shapiro.test(y)$p.value
  
  # Elegimos test de la T, cuando ambos valores p de los test de normalidad 
  # son menores a 0.05
  
  if (p_norm_x > 0.05 && p_norm_y > 0.05) {
    p_p <- p_t
    chose_t <- 1  # Si se elige el test de la t, la variable toma valor 1
  } else {
    p_p <- p_w # Si se elige Wilcoxon, la  variable toma valor 0
    chose_t <- 0
  }
  
  # Los resultados devuelven TRUE/FALSE y 1/0
  
  c(
    t_test = p_t < 0.05,
    wilcoxon = p_w < 0.05,
    perverso = p_p < 0.05,
    chose_t = chose_t
  )
}


## repeticiones - Para ejecutar la función simulación muchas veces´

# INPUTS - n (número de datos por set), delta (para obtener datos con medias
#          diferentes), distribucion (seleccionar el tipo de distribución), 
#          varianza (para cambiar la varianza en un set) y B (número de veces
#          que se ejecuta la función simulación)

# OUTPUTS - Una media de cuantas veces ha salido TRUE en el test de la T,
#           Wilcoxon y el procedimiento perverso y el último, probabilidad de 
#           elegir el test de la T según el procedimiento

repeticiones <- function(n, delta, distribucion, varianza, B = 10000) {
  
  resultado <- replicate(B, simulacion(n, delta, distribucion, varianza))
  
  rowMeans(resultado)
}


## inicio - Función para iniciar el script

# OUTPUTS - Data frame con los resultados de las diferentes simulaciones

inicio <- function() {
  datos <- c(10, 20, 50, 100)
  deltas <- c(0, 0.2, 0.5)
  tipo_distribucion <- 1:5
  varianza <- c(1, 2, 4)
  
  resultado <- calculo_resultados (distribuciones = tipo_distribucion, 
                                   deltas = deltas, ns = datos, 
                                   varianzas = varianza)
  
  return(resultado)
}


## calculo_resultados - Para obtener los resultados de las simulaciones

# INPUTS - n (número de datos por set), delta (para obtener datos con medias
#          diferentes), distribucion (seleccionar el tipo de distribución) y
#          varianza (para cambiar la varianza en un set)

# OUTPUTS - Data frame con los resultados de las diferentes simulaciones

calculo_resultados <- function(distribuciones, ns, deltas, varianzas) {
  
  resultados <- data.frame()
  
  # Hacemos varios bucles para que haga todas las posibles simulaciones
  
  for (d in distribuciones) { 
    for (n in ns) {
      for (delta in deltas) {
        for (v in varianzas) {
          tasas <- repeticiones(n, delta, d, v)
        
          resultados <- rbind(
            resultados,
            data.frame(
              distribucion = d,
              n = n,
              delta = delta,
              metodo = c("t_test", "wilcoxon", "perverso"),
              varianza = v^2,
              tasa = tasas[1:3],
              prob_elegir_t = tasas["chose_t"]
            )
          )
        }
      }
    }
  }
  return(resultados) # Resultados debe tener 540 objetos
}

## visualizar - Para ver resultados específicos

# INPUTS - resultados (data frame obtenido de calculo_resultados)

# OUTPUTS - 3 subsets, con el error tipo I (delta = 0), potencia (delta = 0.5)
#           procedimiento perverso (metodo = "perverso")

visualizar <- function(resultados) {
  
  # Error Tipo I vs Potencia
  
  subset(resultados, delta == 0) 
  # delta == 0 - No hay diferencia real entre grupos, 
  # tasa = error tipo I empirico (Valor "correcto": aprox 0.05)
  
  subset(resultados, delta == 0.5) 
  # detla > 0 - Hay diferencia real, tasa = potencia (Cuanto mayor, mejor)
  
  # Solo las tasas del método perverso
  subset(resultados, metodo == "perverso") 
  
}

resultados <- inicio()