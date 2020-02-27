###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check about the presence of lengths for G1 and G2 species

check_G1_G2 <- function (){
numberError = 0
ResultDataTC = ResultDataTC #read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
              ----------- check presence of lengths for G1 and G2 species in TC - ",ResultDataTC$YEAR[1]), file = Errors, append = TRUE)
  
#ResultDataTB = read.csv(paste(DataTB,".csv",sep=""), sep=";", header=TRUE)


#ResultDataTB$Species = paste  (ResultDataTB$GENUS,ResultDataTB$SPECIES)
ResultDataTC$Species = paste  (ResultDataTC$GENUS,ResultDataTC$SPECIES)

# list_g1_g2 = read.table("Tables/MEDITS_G1_G2.csv",sep=";",header=T)
# list_g1_g2 = read.table(paste(path.package("RoME"),"/extdata/MEDITS_G1_G2.csv",sep=""),sep=";",header=T)
list_g1_g2 = read.table(paste(working_tables,"/MEDITS_G1_G2.csv",sep=""),sep=";",header=T)
G1 =  data.frame(as.character(list_g1_g2$CODE[!is.na(list_g1_g2$MEDITS_G1)]))
G2 =  data.frame(as.character(list_g1_g2$CODE[!is.na(list_g1_g2$MEDITS_G2)]) )

colnames(G1)="Species"
colnames(G2)="Species"

G1_G2 = rbind(G1,G2)

for (spe in 1:length(G1_G2)){
#ResultDataTB_temp = ResultDataTB[ResultDataTB$Species == G1_G2[spe,],]
ResultDataTC_temp = ResultDataTC[ResultDataTC$Species == G1_G2[spe,],]

       for (haul in ResultDataTC_temp$HAUL_NUMBER)  {
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