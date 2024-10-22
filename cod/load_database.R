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
MaleProb <- readdata("MaleProb")
FemaleProb <- readdata("FemaleProb")
MaleMu <- readdata("MaleMu")
FemaleMu <- readdata("FemaleMu")

table_males <- read_excel("data/AUS_1995-1997.xlsx", 
                       sheet = "Males")
table_females <- read_excel("data/AUS_1995-1997.xlsx", 
                         sheet = "Females")
table_males$`T_{x}` <- gsub(',','', table_males$`T_{x}`)
table_females$`T_{x}` <- gsub(',','', table_females$`T_{x}`)
table_males <- as.data.frame(sapply(table_males, as.double))
table_females <- as.data.frame(sapply(table_females, as.double))
