piv <- MaleProb[["Able"]]
piv[8,] <- c(119, 0, 0, 0, 0, 0, 1)
modelo <- data.frame(x = 20:119)
modelo <- left_join(modelo, piv, by = "x")

modelo[1:61, 2] <- as.double(spline(piv[1:7, 1], piv[1:7, 2], xout = 20:80)$y)
modelo[61:99, 2] <- as.double(spline(piv[6:8, 1], piv[6:8, 2], xout = 80:118)$y)
plot(modelo$Able)

mucar <- function(x, p1, p2, p3, p4, p5, p6){
  if(x <=65){
    return((p1+p2*p3^x)/(1+p4*p3^x+p5*p3^(-x))+p6)
  } else {
    v <- x - 65
    return(p1*v^5 + p2*v^4 + p3*v^3 + p4*v^2 + p5*v + p6)
  }
}