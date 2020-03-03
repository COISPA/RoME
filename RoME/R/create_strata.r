###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Creation of R-SUFI files:

# strates.csv

create_strata<-function(Stratification,AREA){
  Strata = read.csv(paste(Stratification,".csv",sep=""), sep=";", header=TRUE)
  #write.xlsx(Strata,file=paste(Stratification,".xls", sep = ""))
  #channel <- odbcConnectExcel(paste(Stratification,".xls", sep = ""))
  query= paste("SELECT GSA, CODE, sum(SURF) as Surface from Strata where GSA=",AREA," Group by GSA, CODE order by CODE", sep="")
  Stratif=sqldf(query)
  strates=Stratif[,2:3]
  strates=rbind(strates,NA)
  strates$CODE[6]="Total"
  strates$Surface[6]=sum(strates$Surface[1:5])
  colnames(strates)=c("Strate","Surface")
  Campagne=rep(paste("MEDITS-GSA",AREA,sep=""),6)
  strates=cbind(Campagne,strates)
#   write.table(strates,file=paste("./files R-Sufi/strates_GSA",AREA,".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
  write.table(strates,file=paste("files R-Sufi/strates_GSA",AREA,".csv",sep=""),row.names=FALSE,quote=FALSE,sep=";")
  #odbcClose(channel)
  unlink(paste(Stratification,".xls", sep = ""))
}
###########################################################################################################################
