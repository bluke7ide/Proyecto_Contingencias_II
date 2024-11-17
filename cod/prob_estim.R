#' Calcula la probabilidad de que una persona de edad x con sexo sex
#' de estado ini se deteriore, no mejorar a un estado end.
#' @param x edad de la persona
#' @param sex sexo de la persona
#' @param ini estado inicial de la persona
#' @param end estado final de la persona
deteriorate <- function(x, sex, ini, end){
  #' Calcula la probabilidad de entrar a cualquier estado
  #' @param x la edad de la persona
  new_car <- function(x, alpha, A, B, C, D, E){
    core <- A + (D-A)/(1+B^{C-x})
    return(alpha*(core*(1-exp(-((x-E)/4)^2)/3)))
  }
  #' Calcula la probabilidad de entrar a un estado, dado que entró a uno
  #' @param x la edad de la persona
  severity <- function(x, w, P, Q, R, n){
    f <- P + (1-P)/(1+Q^(R-x))
    scale <- 0
    for( i in 1:4){
      scale <- scale + w[i]*f^(i-1)
    }
    return((w[n]*f^(n-1))/scale)
  }
  
  # Parámetros de las funciones
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
  # Cálculo previo 
  p1 <- new_car(x, alpha, A, B, C, D, E)
  p2 <- severity(x, w, P, Q, R, end)
  
  # Factor de deterioro
  return(p1*p2*factor^{ini})
}

#' Adiciona una probabilidad a los estados Severe y Profound
#' @param x la edad de la persona
#' @param n el estado de la persona. Si es <2, entonces no hace nada
add_mort <- function(x, n){
  return(0.15*max(n-2,0)/((1+1.1^{50-x})*5))
}

#' Calcula la probabilidad de muerte de una persona de edad x
#' de sexo sex con un estado n
#' @param x la edad de la persona
#' @param n el estado de la persona. Si es <2, entonces solo accede
#' @param sex sexo de la persona
prob <- function(x, n, sex){
  # Accede a las tablas de mortalidades para adicionarle la mortalidad extra
  return((Aus_tables[[sex]]$`q_{x}`[x+1]) + add_mort(x,n))
}

#' Calcula el factor de reducción para las transiciones de estados
#' @param x la edad de la persona
#' @param t el tiempo a aplicar
#' @param series conjunto de constantes a aplicar
red_factor_rest <- function(x, t, series){
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
    alpha <- 1 + (1-c)*(x-119)/54
    f <- ((119-x)*h + (x-65)*k)/54
  } else {
    alpha <- c
    f <- h
  }
  return(alpha + (1-alpha)*(1-f)^(t/20))
}

#' Calcula el factor de reducción para las mortalidades de estados
#' @param x la edad de la persona
#' @param t el tiempo a aplicar
#' @param sex sexo de la persona
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
    alpha <- 1 + (1-c)*(x-119)/54
    f <- ((119-x)*h + (x-65)*k)/54
  } else {
    alpha <- c
    f <- h
  }
  return(alpha + (1-alpha)*(1-f)^(t/20))
}

#' Calcula el factor de reducción para normalizar las probabilidades
#' @param x la edad de la persona
#' @param sex sexo de la persona
red_factor_norm <- function(x, sex){
  if (sex == 1) {
    c <- 0.373
    h <- 0.313
    k <- 0.297
  } else {
    c <- 0.394
    h <- 0.255
    k <- 0.302
  }
    alpha <- 1 + (1-c)*(x-120)/90
    f <- ((120-x)*h + (x-90)*k)/90
  return((alpha + (1-alpha)*(1-f)^(max(0, x-90)/2))*(1/(1+exp(10*(x-105)/30))))
}














