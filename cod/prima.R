#' Calcula la prima gracias a un desenglose de calculo_vp o cualquiera
#' @param vps anualidades/seguro para cada estado
prima <- function(vps){
  return((5e6*vps[6] + 12*(1e6*vps[4] + 2e6*vps[5]))/(12*(vps[1] + 0.5*vps[2])))
}

#' Calcula los flujos dado un dataframe de estados
#' @param dvps anualidades/seguro para cada estado partiendo de un estado anterior
flujos <- function(dvps){
  names <- row.names(dvps)
  dvps <- as.matrix(dvps)
  ingresos <- dvps %*% c(nivelada, 0.5*nivelada, 0, 0, 0, 0)
  egresos <- dvps %*% c(0, 0, 0, 1e6, 2e6, 5e6)
  flujo <- data.frame(Ingresos = ingresos, Egresos = egresos, row.names = names)
  return(flujo)
}