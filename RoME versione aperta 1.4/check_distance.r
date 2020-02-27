###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################
# Check consistency of the hauls coordinates with the distance

check_distance<-function(ResultDataTA){
  numberError = 0
  ResultData = ResultDataTA #read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  write(paste("
----------- check consistency of the hauls coordinates with the distance - ", ResultData$YEAR[1]), file = Errors, append = TRUE)

  
  ResultData=ResultData[ResultData$VALIDITY=="V",]
  ResultData=convert_coordinates(ResultData)
  for (i in 1:nrow(ResultData)){
    ResultData$computed_distance[i]=calculate_distance(ResultData$SHOOTING_LATITUDE[i], ResultData$SHOOTING_LONGITUDE[i], ResultData$HAULING_LATITUDE[i], ResultData$HAULING_LONGITUDE[i])
  }
  if (nrow(ResultData)!=0){
    for (j in 1:nrow(ResultData)){
      if (  (ResultData$DISTANCE[j]<=ResultData$computed_distance[j]-0.3*ResultData$computed_distance[j]) | (ResultData$DISTANCE[j]>=ResultData$computed_distance[j]+0.3*ResultData$computed_distance[j])){
        write(paste("Warning: Haul",ResultData$HAUL_NUMBER[j],": the distance in TA ",ResultData$DISTANCE[j],"is quite different from the computed distance",round(ResultData$computed_distance[j],4),"(haul duration:",ResultData$HAUL_DURATION[j],"min)"), file = Errors, append = TRUE)
        tiff(file=paste(getwd(),"/Graphs/haul ", ResultData$HAUL_NUMBER[j], " AREA ",ResultData$AREA[1],"_",ResultData$YEAR[1],".tif",sep=""), width=29.7, height=21, bg="white", units="cm", compression="none",res=200)
        plot(1,1,type="p",xlim=c(min(ResultData$lon_start)-0.1, max(ResultData$lon_start)+0.1), ylim=c(min(ResultData$lat_start)-0.1, max(ResultData$lat_start)+0.1), xlab="Longitude", ylab="Latitude",main=paste("Haul",ResultData$HAUL_NUMBER[j],"-",ResultData$YEAR[j]))
        map("world", fill=T, col="yellow",add=T)
        points(ResultData$lon_start[j],ResultData$lat_start[j],col="blue",pch=16)
        points(ResultData$lon_end[j],ResultData$lat_end[j],col="green",pch=16)
        legend("topleft", paste(c("start position","end position")), pch=c(16,16), col=c("blue","green")  )   
        dev.off() 
      }
    }
  }
  write("Some of the hauls coordinates may be inconsistent with the computed distance. For a visual check, look at the .tiff files in Graphs directory",file = Errors, append = TRUE)
  write(paste("No error occurred"), file = Errors, append = TRUE)
  return(TRUE)
}
################################################################################