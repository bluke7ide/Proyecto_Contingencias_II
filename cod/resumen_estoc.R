esperanza <- function(lista){
  # Solo admite las paralelizadas
  n_fil_vivos <- nrow(lista[[1]])
  n_col_vivos <- ncol(lista[[1]])
  n_fil_vp <- 5
  n_col_vp <- 6
  
  # Creamos una matriz para almacenar el percentil 50
  m_vivos_h <- matrix(0, nrow = n_fil_vivos, ncol = n_col_vivos)
  m_vivos_m <- matrix(0, nrow = n_fil_vivos, ncol = n_col_vivos)
  m_vp_h <- matrix(0, nrow = n_fil_vp, ncol = n_col_vp)
  m_vp_m <- matrix(0, nrow = n_fil_vp, ncol = n_col_vp)
  
  # Calculamos el percentil 50 para cada casilla
  for (fila in 1:n_fil_vivos) {
    for (col in 1:n_col_vivos) {
      # Extraemos los valores de la misma posición en cada matriz
      vivos_h <- sapply(lista[1,], function(mat) mat[fila, col])
      vivos_m <- sapply(lista[2,], function(mat) mat[fila, col])
      # Calculamos la media y lo asignamos a la matriz resultante
      m_vivos_h[fila, col] <- mean(vivos_h)
      m_vivos_m[fila, col] <- mean(vivos_m)

    }
  }
  for(fila in 1:n_fil_vp){
    for(col in 1:n_col_vp){
      # Extraemos los valores de la misma posición en cada matriz
      vp_h <- sapply(lista[3,], function(mat) mat[fila, col])
      vp_m <- sapply(lista[4,], function(mat) mat[fila, col])
      # Calculamos la media y lo asignamos a la matriz resultante
      m_vp_h[fila, col] <- mean(vp_h)
      m_vp_m[fila, col] <- mean(vp_m)
    }
  }
  
  names <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  rownames <- c("From Able",
                "From Mild",
                "From Moderate",
                "From Severe",
                "From Profound")
  
  m_vivos_h <- as.data.frame( m_vivos_h)
  colnames(m_vivos_h) <- names
  m_vivos_h <- data.frame(x = 2024:2124, m_vivos_h)
  m_vivos_m <- as.data.frame(m_vivos_m)
  colnames(m_vivos_m) <- names
  m_vivos_m <- data.frame(x = 2024:2124, m_vivos_m)
  m_vp_h <- as.data.frame(m_vp_h, row.names = rownames)
  colnames(m_vp_h) <- names
  m_vp_m <- as.data.frame(m_vp_m, row.names = rownames)
  colnames(m_vp_m) <- names
  return(list(m_vivos_h, m_vivos_m, flujos(m_vp_h), flujos(m_vp_m)))
}

perc_0_995 <- function(lista){
  # Solo admite las paralelizadas
  n_fil_vivos <- nrow(lista[[1]])
  n_col_vivos <- ncol(lista[[1]])
  n_fil_vp <- 5
  n_col_vp <- 6
  
  # Creamos una matriz para almacenar el percentil 50
  m_vivos_h <- matrix(0, nrow = n_fil_vivos, ncol = n_col_vivos)
  m_vivos_m <- matrix(0, nrow = n_fil_vivos, ncol = n_col_vivos)
  m_vp_h <- matrix(0, nrow = n_fil_vp, ncol = n_col_vp)
  m_vp_m <- matrix(0, nrow = n_fil_vp, ncol = n_col_vp)
  
  # Calculamos el percentil 50 para cada casilla
  for (fila in 1:n_fil_vivos) {
    for (col in 1:n_col_vivos) {
      # Extraemos los valores de la misma posición en cada matriz
      vivos_h <- sapply(lista[1,], function(mat) mat[fila, col])
      vivos_m <- sapply(lista[2,], function(mat) mat[fila, col])
      # Calculamos la media y lo asignamos a la matriz resultante
      if(col < 4){
        m_vivos_h[fila, col] <- quantile(vivos_h, 0.995)
        m_vivos_m[fila, col] <- quantile(vivos_m, 0.995)
      } else {
        m_vivos_h[fila, col] <- quantile(vivos_h, 0.005)
        m_vivos_m[fila, col] <- quantile(vivos_m, 0.005)
      }

      
    }
  }
  for(fila in 1:n_fil_vp){
    for(col in 1:n_col_vp){
      # Extraemos los valores de la misma posición en cada matriz
      vp_h <- sapply(lista[3,], function(mat) mat[fila, col])
      vp_m <- sapply(lista[4,], function(mat) mat[fila, col])
      # Calculamos la media y lo asignamos a la matriz resultante
      if(col <4){
        m_vp_h[fila, col] <- quantile(vp_h, 0.995)
        m_vp_m[fila, col] <- quantile(vp_m, 0.995)
      } else {
        m_vp_h[fila, col] <- quantile(vp_h, 0.005)
        m_vp_m[fila, col] <- quantile(vp_m, 0.005)
      }
    }
  }
  
  names <- c("Able", "Mild", "Moderate", "Severe", "Profound", "Dead")
  rownames <- c("From Able",
                "From Mild",
                "From Moderate",
                "From Severe",
                "From Profound")
  m_vivos_h <- as.data.frame( m_vivos_h)
  colnames(m_vivos_h) <- names
  m_vivos_h <- data.frame(x = 2024:2124, m_vivos_h)
  m_vivos_m <- as.data.frame(m_vivos_m)
  colnames(m_vivos_m) <- names
  m_vivos_m <- data.frame(x = 2024:2124, m_vivos_m)
  m_vp_h <- as.data.frame(m_vp_h, row.names = rownames)
  colnames(m_vp_h) <- names
  m_vp_m <- as.data.frame(m_vp_m, row.names = rownames)
  colnames(m_vp_m) <- names
  return(list(m_vivos_h, m_vivos_m, flujos(m_vp_h), flujos(m_vp_m)))
}