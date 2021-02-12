
haul_at_sea <- function(DataTA, seas=RoME::MedSea, verbose=TRUE) {

  data <- MEDITS.to.dd(DataTA)

  start_coord <- data[ , colnames(data) %in% c("HAUL_NUMBER","AREA","YEAR","MONTH","DAY","SHOOTING_LONGITUDE","SHOOTING_LATITUDE","SHOOTING_QUADRANT")]
  end_coord <- data[ , colnames(data) %in% c("HAUL_NUMBER","AREA","YEAR","MONTH","DAY","HAULING_LONGITUDE","HAULING_LATITUDE","HAULING_QUADRANT")]
  suppressWarnings(sp::coordinates(start_coord) <- ~ SHOOTING_LONGITUDE + SHOOTING_LATITUDE)
  suppressWarnings(sp::coordinates(end_coord) <- ~ HAULING_LONGITUDE + HAULING_LATITUDE)
  # proj4string(MedSea) <- CRS("+proj=longlat")
  suppressWarnings(sp::proj4string(start_coord) <- sp::proj4string(seas))
  suppressWarnings(sp::proj4string(end_coord) <- sp::proj4string(seas))

  suppressWarnings(res_start <- sp::over(start_coord,seas))
  start_coord$over <- res_start$sel
  start_coord <- as.data.frame(start_coord)
  start_coord <- start_coord[is.na(start_coord$over),]



  suppressWarnings(res_end <- sp::over(end_coord, seas))
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
