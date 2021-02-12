

check_G1_G2 <- function (DataTC, wd, suffix){
  if (FALSE){
    library(RoME)
    #library(MEDITS)
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    DataTC = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")

    # check_G1_G2(DataTC,wd,suffix)
  }


  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ResultDataTC = DataTC
  write(paste("\n----------- check presence of lengths for G1 and G2 species in TC - ",ResultDataTC$YEAR[1]), file = Errors, append = TRUE)

ResultDataTC$Species = paste  (ResultDataTC$GENUS,ResultDataTC$SPECIES)

list_g1_g2 <- RoME::list_g1_g2

G1 =  data.frame(as.character(list_g1_g2[!is.na(list_g1_g2$MEDITS_G1),"CODE"]))
G2 =  data.frame(as.character(list_g1_g2[!is.na(list_g1_g2$MEDITS_G2),"CODE"]) )

colnames(G1)="Species"
colnames(G2)="Species"

G1_G2 = rbind(G1,G2)
spe=1
for (spe in 1:length(G1_G2)){
ResultDataTC_temp = ResultDataTC[ResultDataTC$Species == G1_G2[spe,],]
       haul= unique(ResultDataTC_temp$HAUL_NUMBER)[1] # counter
       for (haul in unique(ResultDataTC_temp$HAUL_NUMBER))  {
       Lengths=ResultDataTC_temp$LENGTH_CLASS[ResultDataTC_temp$HAUL_NUMBER == haul]
       if (length(Lengths)==0){
       write(paste("Haul",haul,": for MEDITS species G1 and G2 the collection of the length is mandatory. No length data for", G1_G2[spe,]), file = Errors, append = TRUE)
       numberError = numberError +1
       }
       }
}

if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
  if (file.exists(file.path(tempdir(), "Logfiles"))){
  unlink(file.path(tempdir(),"Logfiles"),recursive=T)
  }
  if (file.exists(file.path(tempdir(), "Graphs"))){
  unlink(file.path(tempdir(),"Graphs"),recursive=T)
    }
	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
  unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
    }
    return(TRUE)

}

