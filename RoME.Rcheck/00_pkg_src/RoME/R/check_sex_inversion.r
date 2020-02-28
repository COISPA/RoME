###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2013
###########################################################################################################################
# Check maturity stages using sex-inversion size

if (FALSE){
  ResultDataTC = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TC_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  }


check_sex_inversion<-function(DataTC){

  numberError = 0
  write(paste("
----------- check consistency of sex data TC by means of sex-inversion size"), file = Errors, append = TRUE)
#   maturity_table = read.csv(file=paste(getwd(),"/Tables/Maturity_parameters.csv",sep=""),sep=";",header=TRUE)
#   maturity_table = read.csv(file=paste(path.package("RoME"),"/extdata/Maturity_parameters.csv",sep=""),sep=";",header=TRUE)
  maturity_table = read.csv(file=paste(working_tables,"/Maturity_parameters.csv",sep=""),sep=";",header=TRUE)
  mat_check = maturity_table[maturity_table$Type_of_hermaphroditism!="",]
  herma_species = unique(mat_check$Species)
  write(paste("In Maturity_parameters.csv table, in Tables folder, information about the sex-inversion size of the following hermaphrodite species is present: "), file = Errors, append = TRUE)

  for (i in herma_species){
    maturity_minitable = maturity_table[(as.character(maturity_table$Species) == i),]
    if (length(maturity_minitable$Species)!=0){
      min_inversion_size = as.numeric(maturity_minitable$min_length_SEX_INVERSION[1])
      max_inversion_size = as.numeric(maturity_minitable$max_length_SEX_INVERSION[1])
      write(i, file = Errors, append = TRUE)
    }
  }

  if (numberError ==0) {
    write(paste("Attention: if you decide to change the sex data detected, after the corrections, run again the code, because you could have entered duplicated records in TC."), file = Errors, append = TRUE)
  }

  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }
}
################################################################################
