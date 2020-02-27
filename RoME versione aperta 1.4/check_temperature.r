###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
################################################################################
# Check if the temperature by haul is reasonable

check_temperature <- function (){
  
  if(!is.na(start_temp) && !is.na(end_temp))
  {
    rm( start_temp, end_temp)
  }

  
 
  numberError = 0
  
  if ((Format=="before_2012")&(DataTD != "")){
  Dataset = ResultDataTD # read.csv(paste(DataTD,".csv",sep=""), sep=";",header=TRUE)
  ResultData = ResultDataTA # read.csv(paste(DataTA,".csv",sep=""), sep=";", header=TRUE)
  ResultData = ResultData[ResultData$HAUL_NUMBER %in% Dataset$HAUL_NUMBER,]
  ResultData= ResultData[order(ResultData$HAUL_NUMBER),]
  Dataset= Dataset[order(Dataset$HAUL_NUMBER),]
   } else {
  Dataset = ResultDataTA #read.csv(paste(DataTA,Type_of_files,sep=""), sep=";",header=TRUE)
  }
  write(paste("
              ----------- check temperature - ",Dataset$YEAR[1]), file = Errors, append = TRUE)
			  
if (!all(is.na(Dataset$BOTTOM_TEMPERATURE_BEGINNING))){
start_temp<<-cbind(Dataset$HAUL_NUMBER,Dataset$BOTTOM_TEMPERATURE_BEGINNING)  
}
if (!all(is.na(Dataset$BOTTOM_TEMPERATURE_END))){  
end_temp<<-cbind(Dataset$HAUL_NUMBER,Dataset$BOTTOM_TEMPERATURE_END)
}


# if (exists("start_temp")){    
# if (!is.na(start_temp)){    
if (length(start_temp[,2]) > 0){    
indices = which(!is.na(start_temp[,2]) & !is.na(end_temp[,2]))
start_temp  <<- start_temp[indices,]
end_temp <<- end_temp[indices,]

for (i in 1:nrow(start_temp)){
  # check beginning temperature
  if((start_temp[i,2] > 30) | (start_temp[i,2] < 10)){
    write(paste("Warning: Haul",start_temp[i,1], ": the beginning temperature is out of the range (10,30) in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
    numberError = numberError +1
  } 
}
}

# if (exists("end_temp")) {
# if (!is.na(end_temp)) {
if (length(end_temp) > 0) {
  for (j in 1:nrow(end_temp)){
# check end temperature
  if((end_temp[j,2] > 30) | (end_temp[j,2] < 10)){
    write(paste("Warning: Haul",end_temp[j,1], ": the end temperature is out of the range (10,30) in",  Dataset$TYPE_OF_FILE[1]), file = Errors, append = TRUE)
   numberError = numberError + 1 
    } 
}
}
#   if (exists("start_temp") & exists("end_temp")){
#   if (!is.na(start_temp) & !is.na(end_temp)){
  if (length(start_temp) > 0 & length(end_temp) > 0 ){
  mean_temp = rowMeans(cbind(start_temp[,2],end_temp[,2]))
  
  if (Format=="before_2012"){
  mean_depth = rowMeans(cbind(ResultData$SHOOTING_DEPTH[ResultData$HAUL_NUMBER %in% start_temp[,1]],ResultData$HAULING_DEPTH [ResultData$HAUL_NUMBER %in% start_temp[,1]]))
  }else {
  mean_depth = rowMeans(cbind(Dataset$SHOOTING_DEPTH[Dataset$HAUL_NUMBER %in% start_temp[,1]],Dataset$HAULING_DEPTH[Dataset$HAUL_NUMBER %in% start_temp[,1]]))
  }
  
  
  
  tiff(file=paste(getwd(),"/Graphs/temperature_control_", Dataset$YEAR[1], "_AREA_",Dataset$AREA[1],".tif",sep=""), width=21, height=29.7, bg="white", units="cm", compression="none",res=200)
  par(mfrow=c(2,1), mai=c(0.3,0.8,0.8,0.3), omi=c(0.8,0.8,1,0.8))
  X=mean_depth
  Y=mean_temp
  #plot(X,Y,xlab="Mean depth (m)",ylab="Mean temperature (?C)",col="blue",pch=16)
  plot(X,Y,xlab="Mean depth (m)",ylab="Mean temperature (Celsius)",col="blue",pch=16,main=paste("Temperature data - ", Dataset$YEAR[1]))
  #mtext("Shooting depth",side=1)
  text(X+0.1,Y,labels=Dataset$HAUL_NUMBER)
  
  dev.off()
  
  write("Temperature check: see the graphs automatically generated in Graphs directory", file = Errors, append = TRUE)
  }
  
   if (numberError ==0) {
    write("No error occurred",file = Errors, append = TRUE)
    }
    
  if (numberError ==0) {
    return(TRUE)
  } else { return(FALSE) }

}
