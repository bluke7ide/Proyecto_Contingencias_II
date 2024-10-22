tablas <- function(sex){
# Inicia los dataframes
Able <- data.frame(x = 0:119,
                   Able = 0,
                   Mild = 0, 
                   Moderate = 0,
                   Severe = 0,
                   Profound = 0,
                   Dead = 0)
Mild <- Able
Moderate <- Able
Severe <- Able
Profound <- Able
Mild$Able <- 0.15
Moderate$Mild <- 0.15
Severe$Moderate <- 0.1
Profound$Severe <- 0.05

# Pone las probabilidades de muerte
Able$Dead[1:110] <- sapply(0:109,function(x) prob(x,0, sex))
Mild$Dead[1:110] <- sapply(0:109,function(x) prob(x,1, sex))
Moderate$Dead[1:110] <- sapply(0:109,function(x) prob(x,2, sex))
Severe$Dead[1:110] <- sapply(0:109,function(x) prob(x,3, sex))
Profound$Dead[1:110] <- sapply(0:109, function(x) prob(x,4, sex))

# Interpola las muertes 110:120

Able$Dead[120] <- 1
Mild$Dead[120] <- 1
Moderate$Dead[120] <- 1
Severe$Dead[120] <- 1
Profound$Dead[120] <- 1

interpol <- function(table){
  return(
    smooth.spline(c(table$x[100:110], 120),
                  c(table$Dead[100:110], 1))
  )
}

Able$Dead[110:119] <- predict(interpol(Able), 110:119)$y
Mild$Dead[110:119] <- predict(interpol(Mild), 110:119)$y
Moderate$Dead[110:119] <- predict(interpol(Moderate), 110:119)$y
Severe$Dead[110:119] <- predict(interpol(Severe), 110:119)$y
Profound$Dead[110:119] <- predict(interpol(Profound), 110:119)$y

# Pone las probabilidades de transición entre estados

Able$Profound <- sapply(0:119,function(x) deteriorate(x,sex, 0, 4))
Able$Severe <- sapply(0:119,function(x) deteriorate(x,sex, 0, 3))
Able$Moderate <- sapply(0:119,function(x) deteriorate(x,sex, 0, 2))
Able$Mild <- sapply(0:119,function(x) deteriorate(x,sex, 0, 1))
Able$Severe[110:120] <- Able$Moderate[110:120]*coef_red
Able$Profound[110:120] <- Able$Profound[110:120]*coef_red
Able$Moderate[110:120] <- Able$Moderate[110:120]*coef_red
Able$Mild[110:120] <- Able$Mild[110:120]*coef_red

Mild$Profound <- sapply(0:119,function(x) deteriorate(x,sex, 1, 4))
Mild$Severe <- sapply(0:119,function(x) deteriorate(x,sex, 1, 3))
Mild$Moderate <- sapply(0:119,function(x) deteriorate(x,sex, 1, 2))
Mild$Severe[110:120] <- Mild$Moderate[110:120]*coef_red
Mild$Profound[110:120] <- Mild$Profound[110:120]*coef_red
Mild$Moderate[110:120] <- Mild$Moderate[110:120]*coef_red
Mild$Able[110:120] <- Mild$Able[110:120]*coef_red

Moderate$Profound <- sapply(0:119,function(x) deteriorate(x,sex, 2, 4))
Moderate$Severe <- sapply(0:119,function(x) deteriorate(x,sex, 2, 3))
Moderate$Severe[110:120] <- Moderate$Severe[110:120]*coef_red
Moderate$Profound[110:120] <- Moderate$Profound[110:120]*coef_red
Moderate$Mild[110:120] <- Moderate$Mild[110:120]*coef_red

Severe$Profound <- sapply(0:119,function(x) deteriorate(x,sex, 3, 4))
Severe$Moderate[110:120] <- Severe$Moderate[110:120]*coef_red
Severe$Profound[110:120] <- Severe$Profound[110:120]*coef_red

Profound$Severe[110:120] <- Profound$Severe[110:120]*coef_red

# Hace la última columna, 1 - todas
Able <- Able %>% mutate(Able = 1 -rowSums(select(., -x, -Able)))
Mild <- Mild %>% mutate(Mild = 1 -rowSums(select(., -x, -Mild)))
Moderate <- Moderate %>% mutate(Moderate = 1 -rowSums(select(., -x, -Moderate)))
Severe <- Severe %>% mutate(Severe = 1 -rowSums(select(., -x, -Severe)))
Profound <- Profound %>% mutate(Profound = 1 -rowSums(select(., -x, -Profound)))

Tables <- list(Able = Able,
               Mild = Mild,
               Moderate = Moderate,
               Severe = Severe,
               Profound = Profound)
return(Tables)
}