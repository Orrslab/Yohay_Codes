require(adehabitatHR)
require(suncalc)
require(sp)
require(raster)



HR_Accumulation <- function(locs_data, Tag_ID, Time_col, Latitude, Longitude, locs_CRS, 
                            HR_Method = c("KDE", "MCP"), 
                            h = "href", 
                            By_Days = TRUE,
                            Activity_Type = "Diurnal",
                            Percentage = 95) {
  
  ### https://cran.r-project.org/web/packages/adehabitatHR/vignettes/adehabitatHR.pdf ###
  ### http://scbi-migbirds.github.io/homeRange.html ###
  
  if (length(HR_Method)>1) stop("Undefined HR Method (KDE or MCP)")
  if (HR_Method[1]!="KDE" & HR_Method[1]!="MCP") stop("Undefined HR Method (KDE or MCP)")
  if (Activity_Type!="Diurnal" & Activity_Type!="Nocturnal") stop("Undefined Activity Type (Diurnal or Nocturnal)")
  
  locs_data <- data.frame("Tag_ID" = locs_data[Tag_ID],
                          "Time_POSI" = locs_data[Time_col],
                          "Latitude" = locs_data[Latitude],
                          "Longitude" = locs_data[Longitude])
  colnames(locs_data) <- c("Tag_ID", "Time_POSI", "Latitude", "Longitude")
  locs_data$Time_POSI <- as.POSIXct(locs_data$Time_POSI, format = "%Y-%m-%d %H:%M:%S", tz = "utc")
  
  
  ids <- unique(locs_data$Tag_ID)
  cumHRkde <- list() ## List with cumulative sample size for all individuals
  for (i in 1:length(ids)) {  ## loop for individuals
    temp_ID <- locs_data[which(locs_data$Tag_ID == ids[i]),] # select an individual
    rownames(temp_ID) <- 1:nrow(temp_ID)
    temp <- SpatialPoints(coordinates(data.frame(temp_ID$Longitude,temp_ID$Latitude)), locs_CRS)
    temp_84 <- data.frame(spTransform(temp, CRS("+init=epsg:4326")))
    Sun <- getSunlightPosition(data = (data.frame(date = temp_ID$Time_POSI, lat = temp_84$temp_ID.Latitude, lon = temp_84$temp_ID.Longitude)), keep = "altitude")
    temp_ID$Sun_Altitude <- deg(Sun$altitude)
    temp_ID$Light <- ifelse(temp_ID$Sun_Altitude >=0 , "Day", "Night")
    temp_ID$Days <- as.Date.character(temp_ID$Time_POSI)
    temp_ID$Days <- as.character(temp_ID$Days)
    temp_ID$Days <- as.numeric(factor(temp_ID$Days))
    if (Activity_Type=="Nocturnal") temp_ID$Days <- ifelse(temp_ID$Sun_Altitude<=0 , temp_ID$Days-1, temp_ID$Days)
    if (Activity_Type=="Nocturnal") temp_ID <- subset(temp_ID, temp_ID$Light=="Night")
    if (Activity_Type=="Diurnal") temp_ID <- subset(temp_ID, temp_ID$Light=="Day")
    temp <- SpatialPoints(coordinates(data.frame(temp_ID$Longitude,temp_ID$Latitude)), locs_CRS)
    ext_temp <- extent(matrix(ncol = 2, nrow = 2, data = c(125782.7,378245,284002.1, 804366.3)))
    #temp@bbox <- as.matrix(extent(ext_temp))
    
    cumulative <- vector()
    if (By_Days==TRUE) {
      for(k in 1:length(unique(temp_ID$Days))){  ##loop for sample size from 5 locations to all locations
        DayX <- which(temp_ID$Tag_ID==ids[i] & temp_ID$Days<=k)
        if (length(DayX)<5) next()
        UD <- kernelUD(temp[DayX,], h = h)
        #UD@bbox <- as.matrix(extent(ext_temp))
        if (HR_Method=="MCP") cumulative[k] <- mcp.area(temp[DayX], percent = Percentage, plotit = F, unin = "m", unout = "km2")
        if (HR_Method=="KDE") cumulative[k] <- kernel.area(UD, percent = Percentage, unin = "m", unout = "km2")
      }
      cumulative <- cumulative[1:length(cumulative)]
      cumHRkde[[i]] <- data.frame(hr = unlist(cumulative), ssize = tail(unique(temp_ID$Days), n=length(unlist(cumulative))))
    }
    
    if (By_Days==FALSE) {
      for(k in 5:nrow(temp_ID)) {  ##loop for sample size from 5 locations to all locations
        DayX <- rownames(temp_ID[0:k,])
        UD <- kernelUD(temp[DayX,], h = h)
        #UD@bbox <- as.matrix(extent(ext_temp))
        if (HR_Method=="MCP") cumulative[k] <- mcp.area(temp[DayX], percent = Percentage, plotit = F, unin = "m", unout = "km2")
        if (HR_Method=="KDE") cumulative[k] <- kernel.area(UD, percent = Percentage, unin = "m", unout = "km2")
      }
      cumulative <- cumulative[1:length(cumulative)]
      cumHRkde[[i]] <- data.frame(hr = unlist(cumulative)[5:length(cumulative)], ssize = tail(rownames(temp_ID), n=nrow(temp_ID)-4))
    }
  }
  names(cumHRkde) <- ids
  return(cumHRkde)
}
