Profound <- data.frame(x = 0:119,
                       Able = 0,
                       Mild = 0,
                       Moderate = 0,
                       Severe = 0.05,
                       Profound = 0,
                       Dead = 0)
Profound$Dead[1:110] <- sapply(0:109, function(x) prob(x,4))
Profound$Severe[110:120] <- Profound$Severe[110:120]*coef_red
Profound$Dead[120] <- 1
interpol <- smooth.spline(c(Profound$x[100:110], 120),
                          c(Profound$Dead[100:110], 1))
Profound$Dead[110:119] <- predict(interpol, 110:119)$y
Profound$Profound <- 1 - Profound$Severe - Profound$Dead

Severe <- data.frame(x = 0:119,
                     Able = 0,
                     Mild = 0,
                     Moderate = 0.1,
                     Severe = 0,
                     Profound = 0,
                     Dead = 0)
Severe$Dead[1:110] <- sapply(0:109,function(x) prob(x,3))
Severe$Dead[120] <- 1
interpol <- smooth.spline(c(Severe$x[100:110], 120),
                          c(Severe$Dead[100:110], 1))
Severe$Dead[110:119] <- predict(interpol, 110:119)$y
Severe$Profound <- sapply(0:119,function(x) prob_estim(x,1, 3, 4))
Severe$Moderate[110:120] <- Severe$Moderate[110:120]*coef_red
Severe$Profound[110:120] <- Severe$Profound[110:120]*coef_red
Severe$Severe <- 1 - Severe$Moderate - Severe$Profound - Severe$Dead 

Moderate <- data.frame(x = 0:119,
                       Able = 0,
                       Mild = 0.15,
                       Moderate = 0,
                       Severe = 0,
                       Profound = 0,
                       Dead = 0)
Moderate$Dead[1:110] <- sapply(0:109,function(x) prob(x,2))
Moderate$Dead[120] <- 1
interpol <- smooth.spline(c(Moderate$x[100:110], 120),
                          c(Moderate$Dead[100:110], 1))
Moderate$Dead[110:119] <- predict(interpol, 110:119)$y
Moderate$Profound <- sapply(0:119,function(x) prob_estim(x,1, 2, 4))
Moderate$Severe <- sapply(0:119,function(x) prob_estim(x,1, 2, 3))
Moderate$Severe[110:120] <- Moderate$Severe[110:120]*coef_red
Moderate$Profound[110:120] <- Moderate$Profound[110:120]*coef_red
Moderate$Mild[110:120] <- Moderate$Mild[110:120]*coef_red
Moderate$Moderate <- 1 - 
  Moderate$Mild - 
  Moderate$Severe - 
  Moderate$Profound - 
  Moderate$Dead 

Mild <- data.frame(x = 0:119,
                   Able = 0.15,
                   Mild = 0, 
                   Moderate = 0,
                   Severe = 0,
                   Profound = 0,
                   Dead = 0)
Mild$Dead[1:110] <- sapply(0:109,function(x) prob(x,1))
Mild$Dead[120] <- 1
interpol <- smooth.spline(c(Mild$x[100:110], 120),
                          c(Mild$Dead[100:110], 1))
Mild$Dead[110:119] <- predict(interpol, 110:119)$y
Mild$Profound <- sapply(0:119,function(x) prob_estim(x,1, 1, 4))
Mild$Severe <- sapply(0:119,function(x) prob_estim(x,1, 1, 3))
Mild$Moderate <- sapply(0:119,function(x) prob_estim(x,1, 1, 2))
Mild$Severe[110:120] <- Mild$Moderate[110:120]*coef_red
Mild$Profound[110:120] <- Mild$Profound[110:120]*coef_red
Mild$Moderate[110:120] <- Mild$Moderate[110:120]*coef_red
Mild$Able[110:120] <- Mild$Able[110:120]*coef_red
Mild$Mild <- 1 - 
  Mild$Able - 
  Mild$Moderate - 
  Mild$Severe - 
  Mild$Profound - 
  Mild$Dead 

Able <- data.frame(x = 0:119,
                   Able = 0,
                   Mild = 0, 
                   Moderate = 0,
                   Severe = 0,
                   Profound = 0,
                   Dead = 0)
Able$Dead[1:110] <- sapply(0:109,function(x) prob(x,1))
Able$Dead[120] <- 1
interpol <- smooth.spline(c(Able$x[100:110], 120),
                          c(Able$Dead[100:110], 1))
Able$Dead[110:119] <- predict(interpol, 110:119)$y
Able$Profound <- sapply(0:119,function(x) prob_estim(x,1, 0, 4))
Able$Severe <- sapply(0:119,function(x) prob_estim(x,1, 0, 3))
Able$Moderate <- sapply(0:119,function(x) prob_estim(x,1, 0, 2))
Able$Mild <- sapply(0:119,function(x) prob_estim(x,1, 0, 1))
Able$Severe[110:120] <- Able$Moderate[110:120]*coef_red
Able$Profound[110:120] <- Able$Profound[110:120]*coef_red
Able$Moderate[110:120] <- Able$Moderate[110:120]*coef_red
Able$Mild[110:120] <- Able$Mild[110:120]*coef_red
Able$Able <- 1 - 
  Able$Mild - 
  Able$Moderate - 
  Able$Severe - 
  Able$Profound - 
  Able$Dead 

Tables <- list(Able = Able,
               Mild = Mild,
               Moderate = Moderate,
               Severe = Severe,
               Profound = Profound)
rm(Able, Mild, Moderate, Severe, Profound)