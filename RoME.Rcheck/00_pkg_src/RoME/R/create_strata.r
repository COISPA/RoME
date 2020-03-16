###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2013                                                                                                            #
###########################################################################################################################
# Creation of R-SUFI files:

# strates.csv

if (FALSE){
  #ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")

  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  #suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  create_strata(stratification_scheme,"18",wd)
}


create_strata<-function(Stratification=MEDITS::stratification_scheme,AREA,wd){

  Strata = Stratification[Stratification$GSA==AREA,] # MEDITS::stratification_scheme #read.csv(paste(Stratification,".csv",sep=""), sep=";", header=TRUE)

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
  write.table(strates,file=file.path(wd,"files R-Sufi",paste("strates_GSA",AREA,".csv",sep="")),row.names=FALSE,quote=FALSE,sep=";")
  #odbcClose(channel)
  #unlink(paste(Stratification,".xls", sep = ""))
}
###########################################################################################################################
