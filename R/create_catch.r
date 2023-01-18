############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Creation of R-SUFI files:
# capt.csv



create_catch<-function(ResultDataTB,year,wd,save=TRUE){

if (FALSE){
  Result = RoME::TB
  year=2012
  wd <- tempdir()
  create_catch(Result,year,wd,save=FALSE)
}

  if (!file.exists(file.path(wd,"files R-Sufi"))){
    dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
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
  ResultDataTB <- ResultDataTB[ResultDataTB$YEAR == year, ]
  ########################################

  ResultData = ResultDataTB

  capt=matrix(nrow=nrow(ResultData),ncol=6)
  colnames(capt)=(c("Survey",  "Year",	"Haul",	"Species",	"Number",	"Weight") )
  capt[,1]=paste("MEDITS-GSA",as.character(ResultData$AREA[1]),sep="")
  capt[,2]=as.character(ResultData$YEAR[1])
  capt[,3]=ResultData$HAUL_NUMBER
  capt[,4]=paste(ResultData$GENUS,ResultData$SPECIES,sep="")
  capt[,5]=as.numeric(as.character(ResultData$TOTAL_NUMBER_IN_THE_HAUL))
  capt[,6]=round(ResultData$TOTAL_WEIGHT_IN_THE_HAUL/1000,3)

  if (save) {
    write.table(capt,file=file.path(wd,"files R-Sufi",paste("captures_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep="")),row.names=FALSE,quote=FALSE,sep=";")
  } else {
    return(as.data.frame(capt))
  }
 if (file.exists(file.path(tempdir(),"files R-Sufi"))){
unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
}
}
