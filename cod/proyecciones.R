proy_vp <- function() {
  # Inicializa las matrices 
  estados <- rep(1, 5000)
  unifs <- matrix(runif(rango * 5000), nrow = rango, ncol = 5000)
  vp <- matrix(0, nrow = 5000, ncol = 6)
  for (i in 1:rango) {
    # Verifica quienes no están muertos
    ids_activos <- which(estados != 6)
    # Si no hay más activos, finaliza el bucle
    if (length(ids_activos) == 0) {
      break
    }
    # Realiza una copia de los nuevos estados por seguridad
    nuevos_estados <- estados
    # Obtiene las probabilidades en esa iteración
    unif_col <- unifs[i, ]
    # Para cada persona activa
    for (j in seq_along(ids_activos)) {
      # Obtiene su id y estado
      idx <- ids_activos[j]
      estado <- estados[idx]
      # Mientras esa persona no haya alcanzado su edad terminal
      if (i <= max_iters[idx]) {
        # Agarra la lista de probabilidades de su estado, las que no son cero
        probs <- lista[[portfolio$id[idx]]][[estado]][i, (estado - 1):6]
        # Calcula cuál probabilidad está de primero con cumsum ya hecho
        nuevo_estado <- which(probs > unif_col[idx])[1]
        # Y pone el nuevo estado dependiendo de la posición del which
        nuevos_estados[idx] <- if (estado == 1) {
          nuevo_estado
        } else {
          nuevo_estado + estado - 2
        }
      }
    }
    estados[ids_activos] <- nuevos_estados[ids_activos]
    # Pone el valor presente si está activo
    vp[cbind(1:5000, estados)] <- vp[cbind(1:5000, estados)] + (v_power[i] * (estados < 6))
    # Si se acaba de morir, porque su vp de muerto es 0, entonces que le acredite a final de año
    vp[estados == 6 & vp[cbind(1:5000, 6)] == 0, 6] <- v_power[i + 1]
  }
  
  vp_df <- as.data.frame(vp)
  colnames(vp_df) <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  return(vp_df)
}

proy_vivos <- function() {
  # Inicializa las matrices
  estados <- rep(1, 5000)
  unifs <- matrix(runif(rango * 5000), nrow = rango, ncol = 5000)
  vivos_h <- matrix(0, nrow = 101, ncol = 6)
  vivos_m <- matrix(0, nrow = 101, ncol = 6)
  vivos_h[1, 1] <- hombres
  vivos_m[1, 1] <- mujeres
  for (i in 1:rango) {
    # Observa cuales no están muertos
    ids_activos <- which(estados != 6)
    # Si no hay más activos, finaliza el bucle
    if (length(ids_activos) == 0) {
      vivos_h[(i + 1):101, 6] <- hombres
      vivos_m[(i + 1):101, 6] <- mujeres
      break
    }
    nuevos_estados <- estados
    # Localiza los unifs locales
    unif_col <- unifs[i, ]
    for (j in seq_along(ids_activos)) {
      # Localiza el id y el estado de esta persona
      idx <- ids_activos[j]
      estado <- estados[idx]
      # Mientras no haya alcanzado su edad terminal
      if (i <= max_iters[idx]) {
        # Agarra la lista de probabilidades de su estado, las que no son cero
        probs <- lista[[portfolio$id[idx]]][[estado]][i, (estado - 1):6]
        # Calcula cuál probabilidad está de primero con cumsum ya hecho
        nuevo_estado <- which(probs > unif_col[idx])[1]
        # Y pone el nuevo estado dependiendo de la posición del which
        nuevos_estados[idx] <- if (estado == 1) {
          nuevo_estado
        } else {
          nuevo_estado + estado - 2
        }
      }
    }
    # Actualización de estados y conteo en vivos_h y vivos_m
    estados[ids_activos] <- nuevos_estados[ids_activos]
    vivos_h[i + 1, ] <- tabulate(estados[sexos], nbins = 6)
    vivos_m[i + 1, ] <- tabulate(estados[!sexos], nbins = 6)
  }
  # Convertir vivos_h y vivos_m a dataframes
  vivos_h <- as.data.frame(vivos_h)
  colnames(vivos_h) <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  vivos_m <- as.data.frame(vivos_m)
  colnames(vivos_m) <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  return(list(vivos_m, vivos_h))
}

proy_vivos_par <- function(n) {
  # Crear un clúster usando la mitad de los núcleos disponibles
  cl <- makeCluster(detectCores() / 2) 
  
  # Exportar las variables necesarias al clúster
  clusterExport(
    cl,
    varlist = c("lista",
                "portfolio",
                "max_iters",
                "sexos",
                "hombres",
                "mujeres",
                "rango",
                "v_power",
                "proy_vivos"),
    envir = environment()
  )
  
  # Ejecutar la proyección en paralelo
  resultados <- parSapply(cl, 1:n, function(x) {
    proy_vivos()
  })
  
  # Detener el clúster después de la ejecución
  stopCluster(cl)
  
  # Resultado: una lista de matrices
  return(resultados)
}
