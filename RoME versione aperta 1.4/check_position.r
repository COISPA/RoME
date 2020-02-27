###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Visual check of the haul positions

check_position<-function(ResultDataTA)
{
  
  ResultData = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  tiff(file=paste(getwd(),"/Graphs/hauls_position ", ResultData$YEAR[1], " AREA ",ResultData$AREA[1],".tif",sep=""), width=21, height=29.7, bg="white", units="cm", compression="none",res=200)
  ResultData=convert_coordinates(ResultData)
  for (i in 1:nrow(ResultData)){
    if (ResultData$HAULING_QUADRANT[i]=="7"){
      ResultData$lon_start[i]=(-1)* ResultData$lon_start[i]
      ResultData$lon_end[i]=(-1)* ResultData$lon_end[i] }
  }
  par(mfrow=c(3,1), mai=c(0.6,0.6,0.6,0.3), omi=c(0.6,0.8,0.8,0.8))
  plot(1,1,type="p",xlim=c(min(ResultData$lon_start)-0.1, max(ResultData$lon_start)+0.1), ylim=c(min(ResultData$lat_start)-0.1, max(ResultData$lat_start)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Hauls position - ",ResultData$YEAR[1]))
  #plot(1,1,type="p",xlim=c(-5.6,35), ylim=c(34,46), xlab="Longitude", ylab="Latitude",main="Mediterranea Sea")
  map("world", fill=T, col="yellow",add=T)
  points(ResultData$lon_start,ResultData$lat_start,col="blue",pch=16)
  points(ResultData$lon_end,ResultData$lat_end,col="green",pch=16)
  legend("topleft", paste(c("start position","end position")), pch=c(16,16), col=c("blue","green")  )
  
  plot(1,1,type="p",xlim=c(min(ResultData$lon_start)-0.1, max(ResultData$lon_start)+0.1), ylim=c(min(ResultData$lat_start)-0.1, max(ResultData$lat_start)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Hauls start position- ",ResultData$YEAR[1]))
  map("world", fill=T, col="yellow",add=T)
  points(ResultData$lon_start,ResultData$lat_start,col="blue",pch=16)
  text(ResultData$lon_start+0.1,ResultData$lat_start,labels=ResultData$HAUL_NUMBER)
  
  
  plot(1,1,type="p",xlim=c(min(ResultData$lon_start)-0.1, max(ResultData$lon_start)+0.1), ylim=c(min(ResultData$lat_start)-0.1, max(ResultData$lat_start)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Hauls end position - ",ResultData$YEAR[1]))
  map("world", fill=T, col="yellow",add=T)
  points(ResultData$lon_end,ResultData$lat_end,col="green",pch=16)
  text(ResultData$lon_end+0.1,ResultData$lat_end,labels=ResultData$HAUL_NUMBER)
  
  dev.off()
  write("Check of hauls position: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)
  if (Type_of_files==".csv"){
    
  } 
}
################################################################################