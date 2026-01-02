## Función que simule un set de datos con diferentes distribuciones

simulacion <- function(n, distri, datos, varianza) {
  
# Elegir la distribución que queramos ("normal", "exponencial", "de la t" y "logarítmica")
  
  if (datos == 1) {  
    datos1 <- rnorm (n, 0, varianza)
    datos2 <- rnorm (n, distri, varianza)
  } else if (datos == 2) {
    datos1 <- rexp (n)
    datos2 <- rexp (n) + distri
  } else if (datos == 3) {
    datos1 <- rt (n, df = 3)
    datos2 <- rt (n, df = 3) + distri
  } else if (datos == 4) {
    datos1 <- rlnorm (n, 0, sdlog = varianza)
    datos2 <- rlnorm (n, meanlog = distri, 1, sdlog = varianza)
  }  
  
  # Los p valores de cada test (test de la T, Wilcoxon y de normalidad)
  
  p_t <- t.test (datos1, datos2)$p.value
  p_wil <- wilcox.test (datos1, datos2)$p.value
  p_norm_1 <- shapiro.test(datos1)$p.value
  p_norm_2 <- shapiro.test(datos2)$p.value
  
  # Devuelve TRUE si el p valor es mayor de 0.05  
  
  if (p_norm_1 > 0.05 && p_norm_2 > 0.05) {
    p_final <- p_t
    seleccion <- 1
  } else {
    p_final <- p_wil
    seleccion <- 0
  }
  
  resultado <- c (test_t = p_t < 0.05, test_wil = p_wil < 0.05, perverso = p_final < 0.05, hizo_t <- seleccion)
  
  # Da como resultado 3 columnas con TRUE o FALSE dependiendo del p-valor
  
  return(resultado)
}

## Función que ejecuta la función anterior 10000 veces

repeticiones <- function (n, delta = 0, distribucion = 1, varianza) {
  
  resultados <- replicate (10000, simulacion (n, delta, distribucion, varianza))
  
  medias <- rowMeans(resultados)
  
  # Te da el error tipo I si delta = 0 o la potencia de cada test si deslta no es 0
  
  elegir_t <- sum (resultados[4, ])
  elegir_wil <- 10000 - elegir_t
  
  return(list(medias [1:3], elegir_t, elegir_wil))
}

## Función para iniciar el script

inicio <- function() {
  
  datos <- as.numeric (readline("¿Cuántos datos por set quieres en tus simulaciones? "))
  distribucion <- menu_distribucion ()
  delta <- as.numeric (readline("Indica el número del delta "))
  varianza <- menu_varianza ()
  
  repeticiones (datos, delta ,distribucion, varianza)
}

## Función qué sirve como menú para elegir la distribución

menu_distribucion <- function() {
  
  # Se muestran las opciones
  
  cat ("¿Qué distribución quieres?\n")
  cat ("1 - Normal\n")
  cat ("2 - Exponencial\n")
  cat ("3 - de la T con 3 grados de libertad\n")
  cat ("4 - Logarítmica\n")
  
  # Se elige la opción
  
  opcion <- as.integer ((readline ("¿Qué opción quieres? ")))
  
  return(opcion)
}

## Igual pero para cambiar la varianza

menu_varianza <- function() {
  opcion <- readline("¿Quieres cambiar la varianza?\ny/n\n")
  
  if (opcion == "y") {
    vari <- as.numeric (readline("Valor de la varianza "))
  } else {
    vari <- 1 # Si se dice que no, varianza = 1
  }
  
  return(vari)
}

inicio ()



####CODIGO ACTUALIZADO:

#FUNCION DE SIMULACION CORREGIDA
#Distribuciones no centradas.



simulacion <- function(n, delta, distribucion) {   #se cambia nombre de argumento a delta, para que refleje tamaño del efecto
  if (distribucion == 1) {            # Normal
    x <- rnorm(n, 0, 1)               # delta = 0 ambas muestras misma distribución
    y <- rnorm(n, delta, 1)           # delta ≠ 0 diferencia clara de localizacion
    
  } else if (distribucion == 2) {     # Exponencial centrada
    x <- rexp(n) - 1                  #se resta la media teorica (1) para que este centrada. Si no está centrada, aun con delta = 0  el error tipo I no es interpretable.
    y <- rexp(n) - 1 + delta
    
  } else if (distribucion == 3) {     # t(df=3) escalada a varianza 1. hace comprarables tamaños de efecto entre distribuciones y evita que diferencias de escala influyan en la potencia.
    x <- rt(n, 3) / sqrt(3)
    y <- rt(n, 3) / sqrt(3) + delta
    
  } else if (distribucion == 4) {     # Log-normal centrada
    x <- rlnorm(n, 0, 1) - exp(0.5)   # se resta la media teorica exp(0.5). Mismo razonamiento que en la exponencial
    y <- rlnorm(n, 0, 1) - exp(0.5) + delta
  }
  
  p_t <- t.test(x, y, var.equal = FALSE)$p.value   # antes se asumia varianzas iguales. Ahora Welch ( var.equal = FALSE). Mas robusto cuando varianzas difieren
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

repeticiones <- function(n, delta, distribucion, B = 10000) {
  
  resultado <- replicate(B, simulacion(n, delta, distribucion))
  colMeans(resultado)
}


#BUCLES DE TAMAÑOS MUESTRALES     

ns <- c(10, 20, 50, 100)
deltas <- c(0, 0.2, 0.5)
distribuciones <- 1:4

resultados <- data.frame()

for (d in distribuciones) {    #se evita ejecucion interactiva para qUe las simulaciones sean reproducibles y no se produzcan errores por interaccion manual
  for (n in ns) {
    for (delta in deltas) {
      
      tasas <- repeticiones(n, delta, d)
      
      resultados <- rbind(
        resultados,
        data.frame(
          distribucion = d,
          n = n,
          delta = delta,
          metodo = c("t_test", "wilcoxon", "perverso"),
          tasa = tasas[1:3],
          prob_elegir_t = tasas["chose_t"]
        )
      )
    }
  }
}

head(resultados)   #para mirar el objeto resultados. Aparece data.frame 
nrow(resultados)    #COMPROBACIÓN IMPORTANTE. DEbe dar 144. 4 distribuciones*4n*3deltas*3metodos

#ERROR TIPO I vs POTENCIA
subset(resultados, delta == 0) # delta == 0. No hay diferencia real entre grupos. tasa = error tipo I empirico. (Valor "correcto": aprox 0,05)
#Interpretación típica: “Bajo la hipótesis nula, el procedimiento perverso no mantiene siempre el nivel nominal del 5%, y su comportamiento depende del tamaño muestral y de la distribución.”

subset(resultados, delta == 0.5) # detla > 0. Si hay diferencia real. tasa =potencia. Cuanto mayor, mejor
# Interpretación típica:“Para un efecto moderado, el procedimiento perverso suele mostrar menor potencia que el mejor de los métodos fijos, debido a la selección basada en el test de normalidad.”

subset(resultados, metodo == "perverso") #columna prob_elegir_t es fundamental para la discusion
# interpretación: Valores cercanos a 1: casi siempre t test  ///////  Valores cercanos a 0: casi siempre Wilcoxon
# Para n pequeño: prob_elegir_t alta //////Para n grande: prob_elegir_t baja (sobre todo en distribuciones no normales()
# Conclusión típica: “El criterio de selección depende fuertemente del tamaño muestral, no solo de la forma de la distribución.”
