###########################################################################################################################
#   RoME: R code to perform multiple checks on MEDITS Survey data (TA, TB, TC and TE files - old and new MEDITS formats)                                   #                   
#   Authors: I. Bitetto, M.T. Facchini, M.T. Spedicato                                                                    #
#   Coispa Tecnologia & Ricerca - Stazione sperimentale per lo Studio delle Risorse del Mare                              #
#   If you have any comments or suggestions please contact the following e-mail address: bitetto@coispa.it                #                                                                                  
#   March 2013                                                                                                            #
###########################################################################################################################

# Facilities

# f.1

calculate_distance<-function(lat_start, lon_start, lat_end, lon_end)   {
  LatStartDeg = floor(floor(lat_start)/100);
  LonStartDeg = floor(floor(lon_start)/100);
  LatStartMin=(lat_start-LatStartDeg*100)/60
  LonStartMin=(lon_start-LonStartDeg*100)/60
  LatEndDeg = floor(floor(lat_end)/100);
  LonEndDeg = floor(floor(lon_end)/100);
  LatEndMin=(lat_end-LatEndDeg*100)/60
  LonEndMin=(lon_end-LonEndDeg*100)/60
  lat_start2 = LatStartDeg + LatStartMin 
  lon_start2 = LonStartDeg + LonStartMin 
  lat_end2 = LatEndDeg + LatEndMin
  lon_end2 = LonEndDeg + LonEndMin
  lat_start = lat_start2;
  lon_start = lon_start2;
  lat_end = lat_end2;
  lon_end = lon_end2;
  
  if (lat_start==lat_end) {
    distance=abs(lon_end-lon_start)
    } else if (lon_end==lon_start){
      distance=abs(lat_end-lat_start)} else {
  
  N1= (((lat_start/2)+45)*pi)/180
  N2= (((lat_end/2)+45)*pi)/180
  N3= atan((pi*(lon_end-lon_start))/(180*(log(tan(N2),exp(1))- (log(tan(N1),exp(1))) )) )
  distance=abs(60*(lat_end-lat_start)/cos(N3))*1852
  return(distance)
  }
}

# f.2
convert_coordinates<-function(Data)  {
  lat_start=Data$SHOOTING_LATITUDE
  lon_start= Data$SHOOTING_LONGITUDE
  lat_end=Data$HAULING_LATITUDE
  lon_end= Data$HAULING_LONGITUDE
  LatStartDeg = floor(floor(lat_start)/100);
  LonStartDeg = floor(floor(lon_start)/100);
  LatStartMin=(lat_start-LatStartDeg*100)/60
  LonStartMin=(lon_start-LonStartDeg*100)/60
  LatEndDeg = floor(floor(lat_end)/100);
  LonEndDeg = floor(floor(lon_end)/100);
  LatEndMin=(lat_end-LatEndDeg*100)/60
  LonEndMin=(lon_end-LonEndDeg*100)/60  
  
  lat_start2= LatStartDeg + LatStartMin 
  lon_start2 = LonStartDeg + LonStartMin 
  lat_end2 = LatEndDeg + LatEndMin
  lon_end2 = LonEndDeg + LonEndMin
  Data$lat_start = lat_start2
  Data$lon_start = lon_start2
  Data$lat_end = lat_end2
  Data$lon_end = lon_end2
  return(Data)
}

# f.3
check.integer <- function(x) {
  x == round(x)
}


# f.4
initializeErrors<-function(){
  write(paste("-------------------------------------------------------------
              LIST OF ERRORS   				
              -------------------------------------------------------------"), file = Errors, append = TRUE)
}

printError<-function(funname,check_without_errors, stop_) {
  if (check_without_errors == FALSE) {
    if (stop_ == FALSE) {
      print(paste(funname,": errors occurred! Please correct files and run again the script. For more details see Logfile.dat", sep=""),quote=FALSE)
      stop_ = TRUE
    }	
  } else {
    print(paste(funname," successfully completed!", sep=""), quote = FALSE)
  }
  
  return(stop_)
}

# f.5
get_types<-function(Data){
#   require(xlsReadWrite)
  ResultData=read.xlsx(normalizePath(Data))
  types=class(ResultData[,1])
  for (i in 2:ncol(ResultData)){
    types=cbind(types,class(ResultData[,i]))
  }
  return(types) 
}

