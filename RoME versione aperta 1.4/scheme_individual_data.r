############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu #
#   March 2020                                                                                                             #
############################################################################################################################
# Check for summarize the individual data collection (goodness of individual data sampling)


scheme_individual_data <- function(){

  if (FALSE){
    library(RoME)
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
    DataTA = MEDITS::TA
    DataTA <- DataTA[DataTA$YEAR == 2008 , ]
    # graphs_TA(DataTA, wd, suffix)
  }

  if (!file.exists(paste(wd,"Logfiles",sep="/"))){
    dir.create(file.path(wd, "Logfiles"), showWarnings = FALSE)
  }
  if (!file.exists(paste(wd,"Graphs",sep="/"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }
  numberError = 0
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }
  Errors <- paste(wd,"\\Logfiles\\Logfile_",suffix,".dat",sep="")

write(paste("\n----------- check summary of individual measures"), file = Errors, append = TRUE)

ResultDataTE = ResultDataTE # read.csv(paste(DataTE,".csv",sep=""), sep=";", header=TRUE)

write(paste("\n----------- check summary of individual measures - ",ResultDataTE$YEAR[1]), file = Errors, append = TRUE)

ResultDataTC = read.csv(paste(DataTC,".csv",sep=""), sep=";", header=TRUE)
ResultDataTE$Species = paste(ResultDataTE$GENUS,ResultDataTE$SPECIES)
ResultDataTC$Species = paste(ResultDataTC$GENUS,ResultDataTC$SPECIES)

mat = aggregate(ResultDataTC$NUMBER_OF_INDIVIDUALS_IN_THE_LENGTH_CLASS_AND_MATURITY_STAGE, by=list(ResultDataTC$Species,ResultDataTC$LENGTH_CLASS), FUN="sum")
colnames(mat)= c("Species", "LENGHT_CLASS","LENGTHS")
mat_fin= data.frame(mat[order(mat$Species,decreasing=F),])
mat_fin=cbind(mat_fin,NA)
mat_fin=cbind(mat_fin,NA)
colnames(mat_fin)= c("Species", "LENGTH_CLASS","LENGTHS","WEIGHTS", "OTOLITH")

mat2 = aggregate(ResultDataTE$INDIVIDUAL_WEIGHT, by=list(ResultDataTE$Species,ResultDataTE$LENGTH_CLASS), FUN="length")
colnames(mat2) = c("Species","LENGTH_CLASS","NUMBER_WEIGHTS")

ResultDataTE_temp = ResultDataTE[ResultDataTE$OTOLITH_SAMPLED=="Y",]
mat3 = aggregate(ResultDataTE_temp$OTOLITH_SAMPLED, by=list(ResultDataTE_temp$Species,ResultDataTE_temp$LENGTH_CLASS), FUN="length")
colnames(mat3) = c("Species","LENGTH_CLASS","NUMBER_OTOLITH")

#species_vec = unique(ResultDataTE$Species)
for (riga in 1:nrow(mat_fin)){
if(length(mat2$NUMBER_WEIGHTS[mat2$Species==mat_fin$Species[riga] & mat2$LENGTH_CLASS==mat_fin$LENGTH_CLASS[riga]])==0) {
mat_fin[riga,4] = 0
} else {
mat_fin[riga,4] = mat2$NUMBER_WEIGHTS[mat2$Species==mat_fin$Species[riga]& mat2$LENGTH_CLASS==mat_fin$LENGTH_CLASS[riga]]
}
}


for (riga in 1:nrow(mat_fin)){
if(length(mat3$NUMBER_OTOLITH[mat3$Species==mat_fin$Species[riga] & mat3$LENGTH_CLASS==mat_fin$LENGTH_CLASS[riga]])==0) {
mat_fin[riga,5] = 0
} else {
mat_fin[riga,5] = mat3$NUMBER_OTOLITH[mat3$Species==mat_fin$Species[riga]& mat3$LENGTH_CLASS==mat_fin$LENGTH_CLASS[riga]]
}
}

mat_fin = mat_fin[which(mat_fin$Species %in% unique (ResultDataTE$Species)),]
write.table(mat_fin, file = paste("sampling_individual_measures_",ResultDataTE$YEAR[1],".csv"),sep=";",row.names=F)

if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }

  #if (numberError ==0) {
    return(TRUE)
  #} else { return(FALSE) }

}
