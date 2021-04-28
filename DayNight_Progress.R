# The function calculates time progress in order to compare the activity along days / night with different starting time / length
#output:
#   A vector of progress percentiles:
#   the night is valued 0   to 100 from sunset  to sunrise
#   The day   is valued 100 to 200 from sunrise to sunset 
#input parameters:
#   a vector Time_POSI - with time in POSIXct format
#   a single value or a vector containing the corresponding coordinates Lat, Lon in geographic / wgs format

require("suncalc")
require("dplyr")
require("lubridate")
DayNight_Progress <- function(Time_POSI, Lat, Lon) {
  if (!is.POSIXct(Time_POSI))
    {er <- errorCondition("the Time_POSI vector must be in POSIXct format")
    stop(er)}
  if (length(Lat)!=length(Lon))
    {er <- errorCondition("the Lat and Lon vectors must be of the same length")
    stop(er)}
  if (length(Lat)==1)
   { Lat <- rep(Lat,length(Time_POSI))
    Lon <- rep(Lon,length(Time_POSI))}
  if (length(Time_POSI)!=length(Lon))
    {er <-errorCondition("the coordinates and time vectors must be of the same length")
    stop(er)}
  tz_loc <- tz(Time_POSI)
  data <- data.frame(Time_POSI=Time_POSI,date=Time_POSI, lat=  Lat,lon= Lon)
  Sun_Alt = getSunlightPosition(data = data, keep = "altitude") # finding the angle of the sun above horizon
  data$date <- date(data$Time_POSI)
  data$Sun_Alt <- Sun_Alt$altitude
  data$Part = ifelse(data$Sun_Alt<=0, "Night", "Day") # defining each point as night or day
  data <- data[order(data$Time_POSI),]
  
  # the next three lines identifyes night/ date/date changes, 
  # the length of day and beginning of each segment (day/night in a date) is calculated once for each segment  for computational efficiency)
  
  data$diff <- data$Part!=lag(data$Part)|(date(data$Time_POSI)!=lag(date(data$Time_POSI))) 
  data <- data %>% mutate(partNum=cumsum(ifelse(is.na(diff), 0, diff)))
  Days <- data %>% group_by(partNum) %>%  slice(1) # takes only the first of each segment
  
  # calculates the segment length :  
  Days$length <- (
    ifelse(Days$Part=="Night", (ifelse(hour(Days$Time_POSI)>12, 
      
      {
        sunset1 <- getSunlightTimes(data = Days, keep = "sunset", tz = tz_loc)
        sunrise1 <- getSunlightTimes(data = data.frame(date = date(Days$Time_POSI)+1, lat = Days$lat, lon = Days$lon), keep = "sunrise", tz = tz_loc)
        as.numeric(difftime(sunrise1$sunrise, sunset1$sunset, units = "secs"))
      }, 
      {
        sunset2 <- getSunlightTimes(data = data.frame(date = date(Days$Time_POSI)-1, lat = Days$lat, lon = Days$lon), keep = "sunset", tz = tz_loc)
        sunrise2 <- getSunlightTimes(data = Days, keep = "sunrise", tz = tz_loc)
        as.numeric(difftime(sunrise2$sunrise, sunset2$sunset, units = "secs"))
      })), 
      {
        sunset3 <- getSunlightTimes(data = Days, keep = "sunset", tz = tz_loc)
        sunrise3 <- getSunlightTimes(data = Days, keep = "sunrise", tz = tz_loc)
        abs(as.numeric(difftime(sunrise3$sunrise, sunset3$sunset, units = "secs")))
      })
  )
  # calculates the segment  beginning time:   
  Days$start <- as.POSIXct(
    ifelse(Days$Part=="Night", (ifelse(hour(Days$Time_POSI)>12, 
                                       
                                       {
                                         sunset1 <- getSunlightTimes(data = Days, keep = "sunset", tz = tz_loc)
                                         sunset1$sunset
                                       }, 
                                       {
                                         sunset2 <- getSunlightTimes(data = data.frame(date = date(Days$Time_POSI)-1, lat = Days$lat, lon = Days$lon), keep = "sunset", tz = tz_loc)
                                         sunset2$sunset
          
                                       })), 
                                       {
                                         sunrise3 <- getSunlightTimes(data = Days, keep = "sunrise", tz = tz_loc)
                                         sunrise3$sunrise
                                       }),
    tz="UTC", origin="1970-01-01")

data <- left_join(data,select(Days,partNum,length,start),by="partNum") # joining the length and beginning time for all data 
data$progress <- as.numeric((data$Time_POS-data$start)/data$length)
data$progress[which(data$Part=="Day")] <- data$progress[which(data$Part=="Day")]+1
return (abs(data$progress*100)) # the absolute value is taken to make sure no negative values are taken stemming from the fact that
#the sunrise is defined slightly before the the sun angle become positive (insgnificant number of samples)
}
