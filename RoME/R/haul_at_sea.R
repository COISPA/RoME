# MedSea <- shapefile("D:\\GIS\\vettori\\GSA\\GFCM - GSA - shp\\GSAs\\gsas (11_unita)2.shp")
# plot(MedSea, col="gray")
# save(MedSea, file="data/MedSea.rda", compress="xz")



haul_at_sea <- function(DataTA, seas=MedSea, verbose=TRUE) {
  MedSea <- Sea
  data <- MEDITS.to.dd(DataTA)

  start_coord <- data[ , colnames(data) %in% c("HAUL_NUMBER","AREA","YEAR","MONTH","DAY","SHOOTING_LONGITUDE","SHOOTING_LATITUDE","SHOOTING_QUADRANT")]
  end_coord <- data[ , colnames(data) %in% c("HAUL_NUMBER","AREA","YEAR","MONTH","DAY","HAULING_LONGITUDE","HAULING_LATITUDE","HAULING_QUADRANT")]
  coordinates(start_coord) <- ~ SHOOTING_LONGITUDE + SHOOTING_LATITUDE
  coordinates(end_coord) <- ~ HAULING_LONGITUDE + HAULING_LATITUDE
  # proj4string(MedSea) <- CRS("+proj=longlat")
  proj4string(start_coord) <- proj4string(MedSea)
  proj4string(end_coord) <- proj4string(MedSea)

  res_start <- over(start_coord,MedSea)
  start_coord$over <- res_start$sel
  start_coord <- as.data.frame(start_coord)
  start_coord <- start_coord[is.na(start_coord$over),]



  res_end <- over(end_coord, MedSea)
  end_coord$over <- res_end$sel
  end_coord <- as.data.frame(end_coord)
  end_coord <- end_coord[is.na(end_coord$over),]


  l_start <- length(start_coord[,1])
  l_end <- length(end_coord[,1])

  if (verbose){
    if(l_start > 0) {
      message("Check the shooting coordinates in the following hauls:\n")
    }
    if(l_end > 0) {
      message("Check the hauling coordinates in the following hauls:\n")
    }
  } # verbose
  if (l_start > 0 & l_end > 0 ){
    results <- list(start_coord, end_coord)
  } else {
    if(l_start > 0){ results <- start_coord} else {
      if(l_end > 0){ results <- end_coord}
    }
  }
  if (l_start == 0 & l_end == 0 ){
    results <- NA
    if (verbose){
      message("\nNone of the coordinates is on the land\n")
    }
  }
  return(results)
}
