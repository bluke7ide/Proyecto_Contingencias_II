#' Lee las tablas del Paper, que vienen seccionadas cada 10 años
#' @param sheet Hoja a leer
readdata <- function(sheet){
  data <- read_excel("data/DataPaper.xlsx", sheet = sheet)
  data <- sapply(data, as.double)
  colnames(data)[1] <- "x"
  data[is.na(data)] <- 0
  lista <- list(Able = data.frame(data[2:8,]),
                Mild = data.frame(data[10:16,]),
                Moderate = data.frame(data[18:24,]),
                Severe = data.frame(data[26:32,]),
                Profound = data.frame(data[34:40,]))
  return(lista)
}
# Se leen todas las hojas. Estas hojas están hechas con el scrapping de 
# archivos incluido en Excel.
MaleProb <- readdata("MaleProb")
FemaleProb <- readdata("FemaleProb")
MaleMu <- readdata("MaleMu")
FemaleMu <- readdata("FemaleMu")

# Lectura de las tablas de mortalidades
table_males <- read_excel("data/AUS_1995-1997.xlsx", 
                       sheet = "Males")
table_females <- read_excel("data/AUS_1995-1997.xlsx", 
                         sheet = "Females")

# Limpieza de las tablas
table_males$`T_{x}` <- gsub(',','', table_males$`T_{x}`)
table_females$`T_{x}` <- gsub(',','', table_females$`T_{x}`)
table_males <- as.data.frame(sapply(table_males, as.double))
table_females <- as.data.frame(sapply(table_females, as.double))

# Guardar las variables en listas
O_Data <- list(MaleProb, FemaleProb, MaleMu, FemaleMu)
Aus_tables <- list(table_males, table_females)
rm(MaleProb, FemaleProb, MaleMu, FemaleMu, table_males, table_females)