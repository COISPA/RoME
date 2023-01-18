### MEDITS.to.dd function trasforms MEDITS coordinates in dd.ddd format
# data <- merge_TATC
MEDITS.to.dd<-function(data)  {
  lat_start=data$SHOOTING_LATITUDE
  lon_start= data$SHOOTING_LONGITUDE
  lat_end=data$HAULING_LATITUDE
  lon_end= data$HAULING_LONGITUDE
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
  data$SHOOTING_LATITUDE = lat_start2
  data$SHOOTING_LONGITUDE = lon_start2
  data$HAULING_LATITUDE = lat_end2
  data$HAULING_LONGITUDE = lon_end2
  k <- 1
  for (k in 1:length(data$SHOOTING_QUADRANT )){
    if (data$SHOOTING_QUADRANT[k] == 7){ data$SHOOTING_LONGITUDE[k] <- -1 * data$SHOOTING_LONGITUDE[k] }
    if (data$SHOOTING_QUADRANT[k] == 3){ data$SHOOTING_LATITUDE[k] <- -1 * data$SHOOTING_LATITUDE[k] }
    if (data$SHOOTING_QUADRANT[k] == 5){
      data$SHOOTING_LONGITUDE[k] <- -1 * data$SHOOTING_LONGITUDE[k]
      data$SHOOTING_LATITUDE[k] <- -1 * data$SHOOTING_LATITUDE[k]
    }


    if (data$HAULING_QUADRANT[k] == 7){ data$HAULING_LONGITUDE[k] <- -1 * data$HAULING_LONGITUDE[k] }
    if (data$HAULING_QUADRANT[k] == 3){ data$HAULING_LATITUDE[k] <- -1 * data$HAULING_LATITUDE[k] }
    if (data$HAULING_QUADRANT[k] == 5){
      data$SHOOTING_LONGITUDE[k] <- -1 * data$HAULING_LONGITUDE[k]
      data$SHOOTING_LATITUDE[k] <- -1 * data$HAULING_LATITUDE[k]
    }
  }

    return(data)

}
