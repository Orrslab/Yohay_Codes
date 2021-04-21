require(utils)
require(sp)
require(maptools)
PolyAssign <-  function(KML_Folder, Locations_Data, Lat, Lon) {
  kmlfiles <- list.files(KML_Folder, pattern=".kml", full.names=TRUE) ## importing list of KMLs files in folder
  kmlnames <- list.files(KML_Folder, pattern=".kml") ## create list of file names
  
  if (length(kmlfiles)==0) {
    er <- errorCondition("Invalid KML Files Folder")
    stop(er)
  }
  
  latlon <- data.frame(Latitude = Locations_Data[Lat], Longitude = Locations_Data[Lon])
  colnames(latlon) <- c("Latitude", "Longitude")
  Locations_Data$Polygon <- NA ## Here You will Find the Results
  
  for (k in 1:length(kmlfiles)) {
    tmp_kml <- getKMLcoordinates(kmlfile=kmlfiles[[k]], ignoreAltitude=T) ## Get coordinates from KML
    tmp_kml <- as.data.frame(tmp_kml) ## Make data frame of coordinates
    colnames(tmp_kml) <- c("Longitude", "Latitude")
    tmp_kml <- tmp_kml[c(2,1)]
    
    in_poly_tmp <- point.in.polygon(point.x = latlon$Longitude, ## Check if points in polygon
                                    point.y = latlon$Latitude,
                                    pol.x = tmp_kml$Longitude,
                                    pol.y = tmp_kml$Latitude)
    Locations_Data$Polygon[which(in_poly_tmp==1)] <- kmlnames[[k]]
    rm(in_poly_tmp)
    rm(tmp_kml)
  }
  return(Locations_Data$Polygon)
}
