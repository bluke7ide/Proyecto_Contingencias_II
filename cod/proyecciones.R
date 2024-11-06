proyeccion <- function(){
  # Inicializa las matrices
  estados <- rep(1, 5000)
  unifs <- matrix(runif(rango * 5000), nrow = rango, ncol = 5000)
  vivos_h <- matrix(0, nrow = 101, ncol = 6)
  vivos_m <- matrix(0, nrow = 101, ncol = 6)
  vivos_h[1, 1] <- hombres
  vivos_m[1, 1] <- mujeres
  vp_h <- matrix(0, nrow = 5, ncol = 6)
  vp_m <- matrix(0, nrow = 5, ncol = 6)
  for (i in 1:rango) {
    # Observa cuales no están muertos
    ids_activos <- which(estados != 6)
    # Si no hay más activos, finaliza el bucle
    if (length(ids_activos) == 0) {
      vivos_h[(i + 1):101, 6] <- hombres
      vivos_m[(i + 1):101, 6] <- mujeres
      break
    }
    # Genera copias
    new_estados <- estados
    unif_col <- unifs[i, ]
    # Calculamos para cada persona activa el estado nuevo
    for (j in ids_activos) {
      estado <- estados[j]
      # Toma la fila de probabilidades correspondiente
      probs <- lista[[portfolio$id[j]]][[estado]][i, (estado - 1):6]
      # Y obtiene cuál es el siguiente, por medio del which
      actual <- which(probs > unif_col[j])[1]
      # Which solo toma los que ocupa, por lo que hay que trasladar el resultado
      new_estados[j] <- if (estado == 1) actual else actual + estado - 2

    }
    # Ante las múltiples llamadas, mejor tenerlas acá
    v_i <- v_power[i]
    v_i1 <- v_power[i + 1]
    # Contabiliza transiciones para ingresos e egresos (proy_vp)
    for (estado_ant in 1:5) {
      cond_h <- estados == estado_ant & sexos
      cond_m <- estados == estado_ant & !sexos
      
      # Operación matricial para agregar al valor presente
      trans_h <- as.numeric(table(factor(new_estados[cond_h], levels = 1:6)))
      trans_m <- as.numeric(table(factor(new_estados[cond_m], levels = 1:6)))
      
      vp_h[estado_ant, ] <- vp_h[estado_ant, ] + trans_h * c(v_i, v_i, v_i, v_i, v_i, v_i1)
      vp_m[estado_ant, ] <- vp_m[estado_ant, ] + trans_m * c(v_i, v_i, v_i, v_i, v_i, v_i1)
    }
    # Actualización de estados y conteo de personas (proy_vivos)
    estados[ids_activos] <- new_estados[ids_activos]
    vivos_h[i + 1, ] <- tabulate(estados[sexos], nbins = 6)
    vivos_m[i + 1, ] <- tabulate(estados[!sexos], nbins = 6)
  }
  return(list(vivos_h, vivos_m, vp_h, vp_m))
}

proyeccion_par <- function(n, cores) {
  # Crear un clúster seguro
  cl <- makeCluster(min(detectCores()/2, cores)) 
  # Exportar las variables necesarias al clúster
  clusterExport(
    cl,
    varlist = variables,
    envir = environment()
  )
  # Ejecutar la proyección en paralelo
  resultados <- parSapply(cl, 1:n, function(x) {
    proyeccion()
  })
  # Detener el clúster después de la ejecución
  stopCluster(cl)
  return(resultados)
}