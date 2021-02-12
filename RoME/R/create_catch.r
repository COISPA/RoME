create_catch<-function(ResultDataTB,wd){

if (FALSE){
  Result = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TB_GSA18_1994-2018.csv", sep=";")

  #wd <- "C:/Users/Utente/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  #create_catch(Result,wd)
}

  if (!file.exists(file.path(wd,"files R-Sufi"))){
    dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
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

write.table(capt,file=file.path(wd,"files R-Sufi",paste("captures_",ResultData$YEAR[1],"_GSA",ResultData$AREA[1],".csv",sep="")),row.names=FALSE,quote=FALSE,sep=";")
 if (file.exists(file.path(tempdir(),"files R-Sufi"))){
unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
}
}
