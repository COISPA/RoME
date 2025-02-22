############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################

# Check if the haul start in the same quadrant
if (FALSE){
    ResultDataTA = RoME::TA
    wd <- tempdir()
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
    year=2007
    # check_quadrant(ResultDataTA,year,wd,suffix)
}


check_quadrant<-function(ResultDataTA,year,wd,suffix){

  if (!file.exists(file.path(wd, "Logfiles"))){
    dir.create(file.path(wd, "Logfiles"), recursive = TRUE, showWarnings = FALSE)
  }
  if (!exists("suffix")){
    suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time_h%Hm%Ms%OS0"),sep="")
  }
  numberError = 0
  Errors <- file.path(wd,"Logfiles",paste("Logfile_",suffix,".dat",sep=""))
  if (!file.exists(Errors)){
    file.create(Errors)
  }

  ### FILTERING DATA FOR THE SELECTED YEAR
  arg <- "year"
  if (!exists(arg)) {
    stop(paste0("'", arg, "' argument should be provided"))
  } else if (length(year) != 1) {
    stop(paste0("only one value should be provided for '", arg, "' argument"))
  } else if (is.na(year)) {
    stop(paste0(arg, " argument should be a numeric value"))
  }
  ResultDataTA <- ResultDataTA[ResultDataTA$YEAR == year, ]
  ########################################

  ResultData = ResultDataTA
  write(paste("\n----------- check start quadrant and end quadrant TA - ",ResultData$YEAR[1]), file = Errors, append = TRUE)


  ResultData=ResultData[ResultData$VALIDITY=="V",]
  for (i in 1:nrow(ResultData)){
    if (ResultData$SHOOTING_QUADRANT[i]!=ResultData$HAULING_QUADRANT[i])  {
      write(paste("Warning: Haul",ResultData$HAUL_NUMBER[i]," starts in the quadrant",ResultData$SHOOTING_QUADRANT[i]," and finishes in the quadrant", ResultData$HAULING_QUADRANT[i]," in",ResultData$TYPE_OF_FILE[1]), file = Errors, append = TRUE)}
  }

  if (numberError ==0) {
    write(paste("No error occurred"), file = Errors, append = TRUE)
  }
#    if (file.exists(file.path(tempdir(), "Logfiles"))){
#   unlink(file.path(tempdir(),"Logfiles"),recursive=T)
#   }
#   if (file.exists(file.path(tempdir(), "Graphs"))){
#   unlink(file.path(tempdir(),"Graphs"),recursive=T)
#     }
# 	if (file.exists(file.path(tempdir(), "files R-Sufi"))){
#   unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
#     }
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
