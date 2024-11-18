#' Proyecta la prima estocástica, para todas las personas
#' @param interes tasa de interés a tomar
#' @param inflacion tasa de inflación a tomar
proy_prima <- function(interes, inflacion) {
  # Inicialización de los valores
  edades <- descripcion$edad
  rango <- 120 - min(edades)
  unicos <- length(edades)
  estados <- rep(1L, unicos)
  unifs <- matrix(runif(rango * unicos), nrow = rango, ncol = unicos)
  v <- (1 + inflacion) / (1 + interes)
  vp <- matrix(0, nrow = unicos, ncol = 6)
  v_power <- v^(0:rango)
  max_iters <- 120 - edades

  # Iteración entre edades 
  for (i in 1:rango) {
    # Accede directamente a las probabilidades acumuladas en `lista`
    locales <- sapply(1:unicos, function(x) {
      if (i <= max_iters[x] && estados[x] < 6) {
        lista[[x]][[estados[x]]][i, ]  # Ya es acumulada
      } else {
        rep(NA, 6)
      }
    })
    
    # Actualización de valores presentes para estados vivos
    vp[cbind(1:unicos, estados)] <- vp[cbind(1:unicos, estados)] + (v_power[i] * (estados < 6))
    
    # Localiza las probabilidades uniformes en la iteración
    unif_col <- unifs[i, ]
    
    # Calcula los nuevos estados
    nuevos_estados <- sapply(1:unicos, function(j) {
      estado <- estados[j]
      if (estado == 6) {
        return(estado)
      }
      # Obtiene directamente las probabilidades acumuladas precalculadas
      probs <- locales[(estado - 1):6, j]
      
      # Observa cuál es el nuevo estado por medio de comparación al cumsum
      nuevo_estado <- which(probs > unif_col[j])[1]
      
      if (is.na(nuevo_estado)) {
        return(estado)
      } else if (estado == 1) {
        return(nuevo_estado)
      } else {
        # Ajuste porque el cumsum empieza solo con las probabilidades necesarias
        return(nuevo_estado + estado - 2)
      }
    })
    
    # Actualización de estados y valores presentes de estados muertos
    estados <- ifelse(estados == 6 & vp[cbind(1:unicos, 6)] == 0, estados, nuevos_estados)
    vp[estados == 6 & vp[cbind(1:unicos, 6)] == 0, 6] <- v_power[i + 1]
  }
  
  return(vp)
}

#' Ejecuta la proyección en paralelo
#' @param n número de iteraciones
#' @param interes tasa de interés a tomar
#' @param inflacion tasa de inflación a tomar
proy_prima_par <- function(n, interes, inflacion) {
  cl <- makeCluster(2) 
  clusterExport(
    cl,
    varlist = c("descripcion", "lista", "proy_prima", "interes", "inflacion"),
    envir = environment())
  resultados <- parSapply(cl, 1:n, function(x) {
    proy_prima(interes, inflacion)
  })
  stopCluster(cl)
  return(array(unlist(resultados), dim = c(length(descripcion$edad), 6, n)))
}