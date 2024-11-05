proy_prima <- function(interes, inflacion) {
  # Inicialización de los valores
  estados <- rep(1L, 102)
  edades <- descripcion$edad
  rango <- 120 - min(edades)
  unifs <- matrix(runif(rango * 102), nrow = rango, ncol = 102)
  v <- (1 + inflacion) / (1 + interes)
  vp <- matrix(0, nrow = 102, ncol = 6)
  v_power <- v^(0:rango)
  max_iters <- 120 - edades
  
  # Iteración entre edades 
  for (i in 1:rango) {
    # Accede directamente a las probabilidades acumuladas en `lista`
    locales <- sapply(1:102, function(x) {
      if (i <= max_iters[x] && estados[x] < 6) {
        lista[[x]][[estados[x]]][i, ]  # Ya es acumulada
      } else {
        rep(NA, 6)
      }
    })
    
    # Actualización de valores presentes para estados vivos
    vp[cbind(1:102, estados)] <- vp[cbind(1:102, estados)] + (v_power[i] * (estados < 6))
    
    # Localiza las probabilidades uniformes en la iteración
    unif_col <- unifs[i, ]
    
    # Calcula los nuevos estados
    nuevos_estados <- sapply(1:102, function(j) {
      estado <- estados[j]
      if (estado == 6) {
        return(estado)
      }
      # Obtiene directamente las probabilidades acumuladas precalculadas
      probs <- locales[(estado - 1):6, j]
      
      # Observa cuál es el nuevo estado por medio de comparación al `cumsum`
      nuevo_estado <- which(probs > unif_col[j])[1]
      
      if (is.na(nuevo_estado)) {
        return(estado)
      } else if (estado == 1) {
        return(nuevo_estado)
      } else {
        # Ajuste porque el `cumsum` empieza solo con las probabilidades necesarias
        return(nuevo_estado + estado - 2)
      }
    })
    
    # Actualización de estados y valores presentes de estados muertos
    estados <- ifelse(estados == 6 & vp[cbind(1:102, 6)] == 0, estados, nuevos_estados)
    vp[estados == 6 & vp[cbind(1:102, 6)] == 0, 6] <- v_power[i + 1]
  }
  
  return(vp)
}

proy_prima_par <- function(n, interes, inflacion) {
  cl <- makeCluster(detectCores()/2) 
  
  # Exportar las variables necesarias al clúster
  clusterExport(
    cl,
    varlist = c("descripcion", "lista", "proy_prima", "interes", "inflacion"),
    envir = environment())
  
  # Ejecutar la proyección en paralelo
  resultados <- parSapply(cl, 1:n, function(x) {
    proy_prima(interes, inflacion)
  })
  
  # Lectura de los resultados
  stopCluster(cl)
  return(array(unlist(resultados), dim = c(102, 6, n)))
}