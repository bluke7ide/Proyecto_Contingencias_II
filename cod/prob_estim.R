deteriorate <- function(x, sex, ini, end){
  # Checked
  new_car <- function(x, alpha, A, B, C, D, E){
    core <- A + (D-A)/(1+B^{C-x})
    return(alpha*(core*(1-exp(-((x-E)/4)^2)/3)))
  }
  severity <- function(x, w, P, Q, R, n){
    f <- P + (1-P)/(1+Q^(R-x))
    scale <- 0
    for( i in 1:4){
      scale <- scale + w[i]*f^(i-1)
    }
    return((w[n]*f^(n-1))/scale)
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

add_mort <- function(x, n){
  # Checked
  return(0.15*max(n-2,0)/((1+1.1^{50-x})*5))
}

prob <- function(x, n, sex){
  # Checked
  return((Aus_tables[[sex]]$`q_{x}`[x+1]) + add_mort(x,n))
}

red_factor_rest <- function(x, t, sex, series){
  # Mejora las transiciones con respecto al tiempo
  k <- 0.3
  if(series == 1){
    c <- 0.625
    h <- 0.106
  } else if(series == 2){
    c <- 0.6
    h <- 0.197
  } else {
    c <- 0.566
    h <- 0.272
  }
  if(x > 65){
    alpha <- 1 + (1-c)*(x-120)/65
    f <- ((120-x)*h + (x-65)*k)/65
  } else {
    alpha <- c
    f <- h
  }
  return(alpha + (1-alpha)*(1-f)^(t/20))
}

red_factor_mort <- function(x, t, sex){
  if (sex == 1) {
    c <- 0.163
    h <- 0.403
    k <- 0.303
  } else {
    c <- 0.149
    h <- 0.355
    k <- 0.293
  }
  if(x > 65){
    alpha <- 1 + (1-c)*(x-125)/65
    f <- ((125-x)*h + (x-65)*k)/65
  } else {
    alpha <- c
    f <- h
  }
  return(alpha + (1-alpha)*(1-f)^(t/20))
}















