write.table(LW, "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\data\\LW.csv", sep=";",row.names = FALSE)
LW <- read.csv("D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\data\\LW.csv", sep=";")
  save(LW, file="data/LW.rda")
