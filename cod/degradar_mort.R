degradar_mort <- function(x, sex){
  if(sex == 1){
    data <- Males
  } else {
    data <- Females
  }
  Able <- data$Able[(x+1):120,]
  Mild <- data$Mild[(x+1):120,]
  Moderate <- data$Moderate[(x+1):120,]
  Severe <- data$Severe[(x+1):120,]
  Profound <- data$Profound[(x+1):120,]
  
  # Constantes a multiplicar
  degr <- sapply(x:119, function(y) red_factor_rest(y, 28 + y - x, 2))
  mort <- sapply(x:119, function(y) red_factor_mort(y, 28 + y - x, 2))
  
  # Mejora las transiciones que empeoran
  Able$Mild <- Able$Mild * degr
  Able$Moderate <- Able$Moderate * degr
  Able$Severe <- Able$Severe * degr
  Able$Profound <- Able$Profound * degr
  Mild$Moderate <- Mild$Moderate * degr
  Mild$Severe <- Mild$Severe * degr
  Mild$Profound <- Mild$Profound * degr
  Moderate$Severe <- Moderate$Severe * degr
  Moderate$Profound <- Moderate$Profound * degr
  Severe$Profound <- Severe$Profound * degr
  
  # Mejora las probabilidades de muerte
  Able$Dead <- Able$Dead * mort
  Mild$Dead <- Mild$Dead * mort
  Moderate$Dead <- Moderate$Dead * mort
  Severe$Dead <- Severe$Dead * mort
  Profound$Dead <- Profound$Dead * mort
  
  # Recalcula las probabilidades de permanecer en el estado
  Able <- Able %>% mutate(Able = 1 -rowSums(select(., -x, -Able)))
  Mild <- Mild %>% mutate(Mild = 1 -rowSums(select(., -x, -Mild)))
  Moderate <- Moderate %>% mutate(Moderate = 1 -rowSums(select(., -x, -Moderate)))
  Severe <- Severe %>% mutate(Severe = 1 -rowSums(select(., -x, -Severe)))
  Profound <- Profound %>% mutate(Profound = 1 -rowSums(select(., -x, -Profound)))
  
  # Devuelve la lista para la edad x
  Tables <- list(Able = Able,
                 Mild = Mild,
                 Moderate = Moderate,
                 Severe = Severe,
                 Profound = Profound)
  return(Tables)
}