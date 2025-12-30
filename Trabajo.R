## Función que simule un set de datos con diferentes distribuciones

simulacion <- function(n, distri, datos) {
  
# Elegir la distribución que queramos ("normal", "exponencial", "de la t" y "logarítmica")
  
  if (datos == "normal") {  
    datos1 <- rnorm (n)
    datos2 <- rnorm (n, distri, 1)
  } else if (datos == "exponencial") {
    datos1 <- rexp (n)
    datos2 <- rexp (n) + distri
  } else if (datos == "distribucion_t") {
    datos1 <- rt (n, df = 3)
    datos2 <- rt (n, df = 3) + distri
  } else if (datos == "logaritmica") {
    datos1 <- rlnorm (n, 0, 1)
    datos2 <- rlnorm (n, meanlog = distri, 1)
  }  
  
  # Los p valores de cada test (test de la T, Wilcoxon y de normalidad)
  
  p_t <- t.test (datos1, datos2)$p.value
  p_wil <- wilcox.test (datos1, datos2)$p.value
  p_norm <- shapiro.test(c(datos1, datos2))$p.value
  
  # Devuelve TRUE si el p valor es mayor de 0.05  
  
  if (p_norm > 0.05) {
    p_final <- p_t
  } else {
    p_final <- p_wil
  }
  
  resultado <- c (test_t = p_t < 0.05, test_wil = p_wil < 0.05, perverso = p_final < 0.05)
  
  # Da como resultado 3 columnas con TRUE o FALSE dependiendo del p-valor
  
  return(resultado)
}

## Función que ejecuta la función anterior 10000 veces

repeticiones <- function (n, delta = 0, distribucion = "normal") {
  
  resultados <- replicate (10000, simulacion (n, delta, distribucion))
  
  medias <- rowMeans(resultados)
  
  # Te da el error tipo I si delta = 0 o la potencia de cada test si deslta no es 0
  
  return(medias)
}