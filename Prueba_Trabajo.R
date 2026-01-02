####CODIGO ACTUALIZADO:

#FUNCION DE SIMULACION CORREGIDA
#Distribuciones no centradas.

## Creo una función para obtener números aleatorios logarítmicos con la media y varianza que queramos

logaritmo_varianza  <- function(num, varianza, delta) {
  
 num_estandar <- num / sqrt((exp(1) - 1) * exp(1)) # Hacemos que la varianza sea 1 al dividir por la desviación teórica
 dato <- num_estandar * varianza + delta # Obtenemos números con la varianza y media (delta) que indiquemos
 
 return(dato)
}

simulacion <- function(n, delta, distribucion, varianza) {   #se cambia nombre de argumento a delta, para que refleje tamaño del efecto
  if (distribucion == 1) {            # Normal
    x <- rnorm(n, 0, 1)               # delta = 0 ambas muestras misma distribución
    y <- rnorm(n, delta, varianza)           # delta ≠ 0 diferencia clara de localizacion
    
  } else if (distribucion == 2) {     # Exponencial centrada
    x <- rexp(n) - 1                  #se resta la media teorica (1) para que este centrada. Si no está centrada, aun con delta = 0  el error tipo I no es interpretable.
    y <- (rexp(n) - 1) * varianza + delta
    
  } else if (distribucion == 3) {     # t(df=3) escalada a varianza 1. hace comprarables tamaños de efecto entre distribuciones y evita que diferencias de escala influyan en la potencia.
    x <- rt(n, 3) / sqrt(3)
    y <- (rt(n, 3) / sqrt(3)) * varianza  + delta
    
  } else if (distribucion == 4) {     # Log-normal centrada
    x_1 <- rlnorm(n, 0, 1) - exp(0.5)   # se resta la media teorica exp(0.5). Mismo razonamiento que en la exponencial
    y_1 <- rlnorm(n, 0, 1) - exp(0.5)
    
    x <- logaritmo_varianza(x_1, 1, 0)
    y <- logaritmo_varianza(y_1, varianza, delta)
  }
  
  p_t <- t.test(x, y)$p.value   # Se hace test de Welch, las varianzas no tiene por qué ser iguales
  p_w <- wilcox.test(x, y, exact = FALSE)$p.value
  
  p_norm_x <- shapiro.test(x)$p.value
  p_norm_y <- shapiro.test(y)$p.value
  
  if (p_norm_x > 0.05 && p_norm_y > 0.05) {
    p_p <- p_t
    chose_t <- 1  # se indica explicitamente que se usó el t-test
  } else {
    p_p <- p_w
    chose_t <- 0
  }
  
  c(
    t_test = p_t < 0.05,
    wilcoxon = p_w < 0.05,
    perverso = p_p < 0.05,
    chose_t = chose_t
  )
}


##REPETICIONES DE MONTE CARLO

repeticiones <- function(n, delta, distribucion, varianza, B = 10000) {
  
  resultado <- replicate(B, simulacion(n, delta, distribucion, varianza))
  rowMeans(resultado)
}


## Con esto inicias el script     

inicio <- function() {
  datos <- c(10, 20, 50, 100)
  deltas <- c(0, 0.2, 0.5)
  tipo_distribucion <- 1:4
  varianza <- c(1, 2, 4)
  
  resultado <- calculo_resultados (distribuciones = tipo_distribucion, deltas = deltas, ns = datos, varianzas = varianza)
  
  return(resultado)
}


## Te da los resultados de las simulaciones

calculo_resultados <- function(distribuciones, ns, deltas, varianzas) {
  
  resultados <- data.frame()
  
  for (d in distribuciones) {    #se evita ejecucion interactiva para que las simulaciones sean reproducibles y no se produzcan errores por interaccion manual
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
              varianza = v,
              tasa = tasas[1:3],
              prob_elegir_t = tasas["chose_t"]
            )
          )
        }
      }
    }
  }
}


# Resultados debe tener 432 objetos

visualizar <- function(resultados) {
  
  #ERROR TIPO I vs POTENCIA
  subset(resultados, delta == 0) # delta == 0. No hay diferencia real entre grupos. tasa = error tipo I empirico. (Valor "correcto": aprox 0,05)
  #Interpretación típica: “Bajo la hipótesis nula, el procedimiento perverso no mantiene siempre el nivel nominal del 5%, y su comportamiento depende del tamaño muestral y de la distribución.”
  
  subset(resultados, delta == 0.5) # detla > 0. Si hay diferencia real. tasa =potencia. Cuanto mayor, mejor
  # Interpretación típica:“Para un efecto moderado, el procedimiento perverso suele mostrar menor potencia que el mejor de los métodos fijos, debido a la selección basada en el test de normalidad.”
  
  
  # Solo las tasas del método perverso
  
  subset(resultados, metodo == "perverso") #columna prob_elegir_t es fundamental para la discusion
  # interpretación: Valores cercanos a 1: casi siempre t test  ///////  Valores cercanos a 0: casi siempre Wilcoxon
  # Para n pequeño: prob_elegir_t alta //////Para n grande: prob_elegir_t baja (sobre todo en distribuciones no normales()
  # Conclusión típica: “El criterio de selección depende fuertemente del tamaño muestral, no solo de la forma de la distribución.”
  
}

resultados <- inicio()