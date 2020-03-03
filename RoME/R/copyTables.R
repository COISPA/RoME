## 
## Questo metodo copia le tabelle della cartella table presente nel pacchetto
## in una cartella locale in modo da poter operare su di essa.
## 

copyTables <- function()
{
  # Verifico se la cartella Table è presente:
  if(!file.exists(paste(getwd(),"/Table",sep="")))
  {
    dir.create(paste(getwd(),"/Table",sep=""))
    # copio tutti i files nella cartella del package nella cartella table
#     paste(path.package("RoME"),"/extdata/Maturity_parameters.csv"
    flist <- list.files(paste(path.package("RoME"),"/extdata",sep=""), "*.csv", full.names = TRUE)
    file.copy(flist, paste(getwd(),"/Table",sep=""))
  
  }

  # Assegno il percorso della cartella table memorizzata in locale
  working_tables <<- paste(getwd(),"/Table",sep="")

}

copyPDFmanual <- function()
{
  if(!file.exists("RoME_1.3_User_Manual_2013.pdf"))
  {
    # Se il manuale non è presente lo vado a copiare nella wd del programma
#     flist <- list.files(paste(path.package("RoME"),"/extdata",sep=""), "*.pdf", full.names = TRUE)
    flist <- list.files(paste(path.package("RoME"),"/extdata",sep=""), "RoME_1.3_User_Manual_2013.pdf", full.names = TRUE)
    file.copy(flist, "RoME_1.3_User_Manual_2013.pdf")
  }
}