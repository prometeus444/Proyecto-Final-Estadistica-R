## Función que simule un set de datos con diferentes distribuciones

simulacion <- function(n, distri, datos) {
  
# Elegir la distribución que queramos ("normal", "exponencial", "de la t" y "logarítmica")
  
  if (datos == 1) {  
    datos1 <- rnorm (n)
    datos2 <- rnorm (n, distri, 1)
  } else if (datos == 2) {
    datos1 <- rexp (n)
    datos2 <- rexp (n) + distri
  } else if (datos == 3) {
    datos1 <- rt (n, df = 3)
    datos2 <- rt (n, df = 3) + distri
  } else if (datos == 4) {
    datos1 <- rlnorm (n, 0, 1)
    datos2 <- rlnorm (n, meanlog = distri, 1)
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

repeticiones <- function (n, delta = 0, distribucion = 1) {
  
  resultados <- replicate (10000, simulacion (n, delta, distribucion))
  
  medias <- rowMeans(resultados)
  
  # Te da el error tipo I si delta = 0 o la potencia de cada test si deslta no es 0
  
  elegir_t <- sum (resultados[4, ])
  elegir_wil <- 10000 - elegir_t
  
  return(list(medias [1:3], elegir_t, elegir_wil))
}

## Función para iniciar el script

inicio <- function() {
  
  datos <- as.numeric (readline("¿Cuántos datos por set quieres en tus simulaciones? "))
  distribucion <- menu ()
  delta <- as.numeric (readline("Indica el número del delta "))
  
  repeticiones (datos, delta ,distribucion)
}

## Función qué sirve como menú para elegir la distribución

menu <- function() {
  
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

inicio ()