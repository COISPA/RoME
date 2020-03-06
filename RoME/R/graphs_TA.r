###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #
#   March 2013                                                                                                            #
###########################################################################################################################
# Qualitative control (by means of 2 graphs) of relation between shooting depth e warp opening and between warp length e wing opening

if (FALSE){
  ResultDataTA = read.csv("C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/data/TA_GSA18_1994-2018.csv", sep=";")
  wd <- "C:/Users/Bitetto Isabella/OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L/Rome/ROME/temp"
  #suffix=paste(as.character(Sys.Date()),format(Sys.time(), "_time h%Hm%Ms%OS0"),sep="")
  graphs_TA(ResultDataTA,wd)
  }


graphs_TA<-function(ResultDataTA,wd){

  if (!file.exists(paste(wd,"Graphs",sep="/"))){
    dir.create(file.path(wd, "Graphs"), showWarnings = FALSE)
  }



  Errors <- paste(wd,"/Logfiles/Logfile_",suffix,".dat",sep="")




  ResultData = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  tiff(file=paste(wd,"/Graphs/qualitative_control_TA_", ResultData$YEAR[1], "_AREA_",ResultData$AREA[1],".tif",sep=""), width=21, height=29.7, bg="white", units="cm", compression="none",res=200)
  par(mfrow=c(2,1), mai=c(0.3,0.8,0.8,0.3), omi=c(0.8,0.8,1,0.8))
  X=ResultData$SHOOTING_DEPTH
  Y=ResultData$WARP_LENGTH
  plot(X,Y,xlab="Shooting depth",ylab="Warp length",col="blue",pch=16,main = paste("Shooting depth versus Warp length- ",ResultData$YEAR[1]))
  mtext(paste("Shooting depth"),side=1)
  text(X+0.1,Y,labels=ResultData$HAUL_NUMBER)
  Z=Y
  H=ResultData$WING_OPENING
  plot(Z,H,xlab="Warp length",ylab="Wing opening",col="green",pch=16,main = paste("Warp length versus Wing opening - ",ResultData$YEAR[1]))
  text(Z+0.1,H,labels=ResultData$HAUL_NUMBER)
  mtext(paste("Warp length"),side=1)
  dev.off()

  write("Qualitative check TA: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)

}
