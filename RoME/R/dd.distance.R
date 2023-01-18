### ### MEDITS.distance estimates the distances using the dd.ddd coordinate format
dd.distance<-function(data, unit = "m", verbose=TRUE)  {
  N1  <- (((data$SHOOTING_LATITUDE/2)+45)*pi )/180
  N2  <- (((data$HAULING_LATITUDE/2)+45)*pi )/180
  N3  <- atan((pi*(data$HAULING_LONGITUDE-data$SHOOTING_LONGITUDE))/(180*(log(tan(N2))-log(tan(N1)))))
  dist <- abs(60*(data$HAULING_LATITUDE-data$SHOOTING_LATITUDE)/cos(N3))*1852
  if (unit =="m"){
    dist = dist
    if (verbose){
    message("meters")
      }
  }
  if (unit =="km"){
    dist = dist/1000
    if (verbose){
    message("kilometers")
    }
    }
  if (unit =="NM"){
    dist = dist/1852
    if (verbose){
      message("Nautical Miles")
    }
      }
  return(dist)
}
