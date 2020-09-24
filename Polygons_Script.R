
library(utils)
library(maptools)
library(SDMTools)

KML_Folder <- " SET THE DIRECTORY WITH ALL THE POLYGONS FILES IN KML FORMAT " ## Set Directory with Polygons Files
Locations_Data <- read.csv(" SET THE PATH TO YOUR DATA ") ## Import Locations Data with Latitude & Longitude Columns


kmlfiles <- list.files(KML_Folder, pattern=".kml", full.names=TRUE) ## importing list of KMLs files in folder
kmlnames <- list.files(KML_Folder, pattern=".kml") ## create list of file names

latlon <- data.frame(Latitude = Locations_Data$Latitude, Longitude = Locations_Data$Longitude)
Locations_Data$Polygon <- NA ## Here You will Find the Results

for (k in 1:length(kmlfiles)) {
  tmp_kml <- getKMLcoordinates(kmlfile=kmlfiles[[k]], ignoreAltitude=T) ## Get coordinates from KML
  tmp_kml <- as.data.frame(tmp_kml) ## Make data frame of coordinates
  colnames(tmp_kml) <- c("Longitude", "Latitude")
  tmp_kml <- tmp_kml[c(2,1)]
  
  #assign(kmlnames[[k]],tmp_kml) ### ENABLE THIS TO SAVE POLYGONS AS ELEMENTS ###
  
  in_poly_tmp <- pnt.in.poly(latlon, tmp_kml) ## Check if points in polygon
  Locations_Data$Polygon[which(in_poly_tmp$pip==1)] <- kmlnames[[k]]
  rm(in_poly_tmp)
  rm(tmp_kml)
  
}


