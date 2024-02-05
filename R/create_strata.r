############################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files)                                #
#   Authors: I. Bitetto, W. Zupa, M.T. Spedicato                                                                           #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                               #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it, zupa@coispa.it #
#   January 2022                                                                                                           #
############################################################################################################################
# Creation of R-SUFI files:

# strates.csv

if (FALSE){
  Stratification=RoME::stratification_scheme
  wd <- tempdir()
  AREA = 18
  create_strata(Stratification,"18",wd,save=TRUE)
}


create_strata<-function(Stratification=RoME::stratification_scheme,AREA,wd,save=TRUE){

  if (!file.exists(file.path(wd,"files R-Sufi"))){
    dir.create(file.path(wd, "files R-Sufi"), showWarnings = FALSE)
  }
  Strata = Stratification[Stratification$GSA==AREA,]

  #write.xlsx(Strata,file=paste(Stratification,".xls", sep = ""))
  #channel <- odbcConnectExcel(paste(Stratification,".xls", sep = ""))

  #query= paste("SELECT GSA, CODE, sum(SURF) as Surface from Strata where GSA=",AREA," Group by GSA, CODE order by CODE", sep="")

  Stratif=aggregate(Strata$SURF,by=list(Strata$GSA, Strata$CODE), FUN="sum") # sqldf(query)
  colnames(Stratif)=c("GSA","CODE","Surface")

  strates=Stratif[,2:3]
  strates=rbind(strates,NA)
  strates$CODE[6]="Total"
  strates$Surface[6]=sum(strates$Surface[1:5])
  colnames(strates)=c("Strate","Surface")
  Campagne=rep(paste("MEDITS-GSA",AREA,sep=""),6)
  strates=cbind(Campagne,strates)
#   write.table(strates,file=paste("./files R-Sufi/strates_GSA",AREA,".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
  if (save) {
      write.table(strates,file=file.path(wd,"files R-Sufi",paste("strates_GSA",AREA,".csv",sep="")),row.names=FALSE,quote=FALSE,sep=";")
      # if (file.exists(file.path(tempdir(),"files R-Sufi"))){
      #    unlink(file.path(tempdir(),"files R-Sufi"),recursive=T)
      # }
  } else {
      return(strates)
  }
  #odbcClose(channel)
  #unlink(paste(Stratification,".xls", sep = ""))

}
###########################################################################################################################
