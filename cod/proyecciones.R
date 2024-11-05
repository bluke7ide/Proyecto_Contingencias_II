proy_vp <- function() {
  # Inicialización de los valores
  estados <- rep(1, 5000)
  unifs <- matrix(runif(rango * 5000), nrow = rango, ncol = 5000)
  vp <- matrix(0, nrow = 5000, ncol = 6)
  
  # Iteración entre edades 
  for (i in 1:rango) {
    # Variable indicadora para omitir personas en estado 6
    ind_activo <- (estados != 6)
    
    # Inicializar locales como una matriz de NA con dimensiones apropiadas
    locales <- matrix(NA, nrow = 6, ncol = sum(ind_activo))
    
    # Llenar sólo las columnas necesarias
    ids_activos <- which(ind_activo)
    for (j in seq_along(ids_activos)) {
      x <- ids_activos[j]
      if (i <= max_iters[x] && estados[x] < 6) {
        locales[, j] <- lista[[portfolio$id[x]]][[estados[x]]][i, ]
      }
    }
    
    if(all(is.na(locales))) {
      break
    }
    
    # Actualización de valores presentes para estados vivos
    vp[cbind(1:5000, estados)] <- vp[cbind(1:5000, estados)] + (v_power[i] * (estados < 6))
    
    # Localiza las probabilidades uniformes en la iteración
    unif_col <- unifs[i, ]
    
    # Calcula los nuevos estados sólo para quienes no están en estado 6
    nuevos_estados <- numeric(length(ids_activos))
    for (j in seq_along(ids_activos)) {
      idx <- ids_activos[j]
      estado <- estados[idx]
      probs <- locales[(estado - 1):6, j]
      
      nuevo_estado <- which(probs > unif_col[idx])[1]
      
      # Si el estado es 1, entonces el which dará las posiciones correctas
      if (!is.na(nuevo_estado)) {
        if (estado == 1) {
          nuevos_estados[j] <- nuevo_estado
        } else {
          nuevos_estados[j] <- nuevo_estado + estado - 2
        }
      } else {
        nuevos_estados[j] <- estado
      }
    }
    
    # Actualización de estados, incluyendo a los que ya están en estado 6
    estados[ids_activos] <- nuevos_estados
    vp[estados == 6 & vp[cbind(1:5000, 6)] == 0, 6] <- v_power[i + 1]
  }
  
  vp_df <- as.data.frame(vp)
  colnames(vp_df) <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  return(vp_df)
}

proy_vivos <- function() {
  estados <- rep(1, 5000)
  unifs <- matrix(runif(rango * 5000), nrow = rango, ncol = 5000)
  vivos_h <- matrix(0, nrow = 101, ncol = 6)
  vivos_m <- matrix(0, nrow = 101, ncol = 6)
  vivos_h[1, 1] <- hombres
  vivos_m[1, 1] <- mujeres
  
  for (i in 1:rango) {
    ind_activo <- (estados != 6)
    
    # Inicializa `locales` como matriz de `NA` y llena las columnas para los estados activos
    locales <- matrix(NA, nrow = 6, ncol = sum(ind_activo))
    ids_activos <- which(ind_activo)
    
    for (j in seq_along(ids_activos)) {
      x <- ids_activos[j]
      if (i <= max_iters[x] && estados[x] < 6) {
        locales[, j] <- lista[[portfolio$id[x]]][[estados[x]]][i, ]
      }
    }
    
    if(all(is.na(locales))) {
      vivos_h[(i + 1):101, 6] <- hombres
      vivos_m[(i + 1):101, 6] <- mujeres
      break
    }
    
    unif_col <- unifs[i, ]
    
    # Calcula `nuevos_estados` sin `sapply` usando los índices activos
    nuevos_estados <- numeric(length(ids_activos))
    for (j in seq_along(ids_activos)) {
      idx <- ids_activos[j]
      estado <- estados[idx]
      probs <- locales[(estado - 1):6, j]
      
      nuevo_estado <- which(probs > unif_col[idx])[1]
      
      # Ajusta el estado con base en el valor actual de `estado`
      if (!is.na(nuevo_estado)) {
        if (estado == 1) {
          nuevos_estados[j] <- nuevo_estado
        } else {
          nuevos_estados[j] <- nuevo_estado + estado - 2
        }
      } else {
        nuevos_estados[j] <- estado
      }
    }
    
    # Actualiza `estados` y calcula los vivos
    estados[ids_activos] <- nuevos_estados
    vivos_h[i + 1, ] <- tabulate(estados[sexos], nbins = 6)
    vivos_m[i + 1, ] <- tabulate(estados[!sexos], nbins = 6)
  }
  
  # Convierte `vivos` en un `data.frame` con nombres de columna
  vivos_h <- as.data.frame(vivos_h)
  colnames(vivos_h) <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  vivos_m <- as.data.frame(vivos_m)
  colnames(vivos_m) <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  return(list(vivos_m, vivos_h))
}
