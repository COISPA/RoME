############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
#  Check about the presence of lengths for G1 and G2 species

check_G1_G2 <- function (DataTC, wd, suffix){
  if (FALSE){
    library(RoME)
    library(MEDITS)
    wd <- tempdir() # "D:\\Documents and Settings\\Utente\\Documenti\\GitHub\\RoME\\temp"
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTC = read.csv("~/GitHub/RoME/data/TC_GSA18_1994-2018.csv", sep=";")

    # check_G1_G2(DataTC,wd,suffix)
  }


  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }

  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")


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

  #if (numberError ==0) {
    return(TRUE)
  #} else { return(FALSE) }

}

