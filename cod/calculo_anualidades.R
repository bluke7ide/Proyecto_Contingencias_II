#' Hace el cálculo financiero, sin valor presente, de las anualidades.
#' Esta función es la base a calculo_vp
#' @param x edad de la persona
#' @param tables la tabla resultante de degradar_mort
calculo_acumulado <- function(x, tables){
  # Se elimina la edad para hacer eficiente el calculo
  t1 <- tables$Able %>% select(-x) 
  t2 <- tables$Mild %>% select(-x) 
  t3 <- tables$Moderate %>% select(-x)
  t4 <- tables$Severe %>% select(-x)
  t5 <- tables$Profound %>% select(-x)
  
  # Preparar la multiplicación matricial
  estados <- as.numeric(t1[1,])
  suma <- estados
  for(i in 2:(120-x)){
    matriz_t <- rbind(t1[i,], t2[i,], t3[i,], t4[i,], t5[i,], c(0,0,0,0,0,1)) 
    matriz_t <- as.matrix(matriz_t)
    estados <- estados %*% matriz_t
    suma <- suma + estados
  }
  return(suma)
}

#' Hace el cálculo a valor presente de las anualidades.
#' Esta función es la base a calculo_vp
#' @param x edad de la persona
#' @param tables la tabla resultante de degradar_mort
#' @param interes tasa de interés a tomar
#' @param inflacion tasa de inflacion a tomar
calculo_vp <- function(x, tables, interes, inflacion){
  # Preparación de la iteración
  v <- (1+inflacion)/(1+interes)
  t1 <- tables$Able %>% select(-x)
  t2 <- tables$Mild %>% select(-x) 
  t3 <- tables$Moderate %>% select(-x)
  t4 <- tables$Severe %>% select(-x)
  t5 <- tables$Profound %>% select(-x)
  estados <- as.numeric(t1[1,])
  suma <- estados
  seguro <- 0
  
  # Multiplicación matricial
  for(i in 2:(120-x)){
    matriz_t <- rbind(t1[i,], t2[i,], t3[i,], t4[i,], t5[i,], c(0,0,0,0,0,1)) 
    matriz_t <- as.matrix(matriz_t)
    temp <- estados %*% matriz_t
    
    # Modificable según el tipo de desembolso/prima
    seguro <- seguro  + (temp[6]- estados[6])*v^i
    estados <- temp
    suma <- suma + estados*v^(i-1)
  }
  suma[6] <- seguro
  return(suma)
}