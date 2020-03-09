###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.eu                #
#   March 2020                                                                                                            #
###########################################################################################################################

# Creation of R-SUFI files:
# capt.csv

if (FALSE){
  Result = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Utente/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
    create_catch(Result,wd)
}

create_catch<-function(ResultDataTB,wd){

  if (!file.exists(file.path(wd,"/files R-Sufi",sep="/"))){
    dir.create(file.path(wd, "/files R-Sufi"), showWarnings = FALSE)
  }

  ResultData = ResultDataTB

  capt=matrix(nrow=nrow(ResultData),ncol=6)
  colnames(capt)=(c("Survey",  "Year",	"Haul",	"Species",	"Number",	"Weight") )
  capt[,1]=paste("MEDITS-GSA",as.character(ResultData$AREA[1]),sep="")
  capt[,2]=as.character(ResultData$YEAR[1])
  capt[,3]=ResultData$HAUL_NUMBER
  capt[,4]=paste(ResultData$GENUS,ResultData$SPECIES,sep="")

 capt[,5]=as.numeric(as.character(ResultData$TOTAL_NUMBER_IN_THE_HAUL))


  capt[,6]=round(ResultData$TOTAL_WEIGHT_IN_THE_HAUL/1000,3)

write.table(capt,file=paste(wd,"/files R-Sufi/captures_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
}
###########################################################################################################################
