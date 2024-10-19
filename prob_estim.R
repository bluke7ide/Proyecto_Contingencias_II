prob_estim <- function(sex, x, ini, end){
  
  new_car <- function(x, alpha, A, B, C, D, E){
    core <- A + (D-A)/(A+B^{C-x})
    return(alpha*(core*(1-exp(-((x-E)/4)^2)/3)))
  }
  
  severity <- function(x, w, P, Q, R, n){
    f <- P + (1-P)/(1+Q^(R-x))
    scale <- 0
    for( i in 1:4){
      scale <- scale + w[i]*f^(i-1)
    }
    return(w[n]*f^(n-1)/scale)
  }
  
  if(sex == 1){
    alpha <- 7.95952
    A <- 0.000959
    B <- 1.0959238
    C <- 93.47352
    D <- 0.119353
    E <- 68.873716
    P <- 0.5129
    Q <- 1.4268
    R <- 85.0082
    w <- c(1, 0.6684, 0.6732, 1.1445)
    factor <- 1.195
  } else {
    alpha <- 7.95978
    A <- 0.000927
    B <- 1.102981
    C <- 93.4994
    D <- 0.11935
    E <- 68.87697
    P <- 0.3453
    Q <- 1.2513
    R <- 85.4748
    w <- c(1, 0.7671, 1.618, 4.602)
    factor <- 1.25
  }

  p1 <- new_car(x, alpha, A, B, C, D, E)
  p2 <- severity(x, w, P, Q, R, end)
  return(p1*p2*factor^{ini})
}