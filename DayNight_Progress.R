DayNight_Progress <- function(Time_POSI, Lat, Lon) {
  
  tz_loc <- tz(Time_POSI)
  Sun_Alt <- getSunlightPosition(data = data.frame(date = Time_POSI, lat = Lat, lon = Lon, keep = "altitude"))
  Part <- ifelse(Sun_Alt$altitude<=0, "Night", "Day")
  Night_Progress <- Part
  
  return(
  ifelse(Part=="Night", (ifelse(hour(Time_POSI)>12, {
    
    sunset1 <- getSunlightTimes(data = data.frame(date = date(Time_POSI), lat = Lat, lon = Lon), keep = "sunset", tz = tz_loc)
    sunrise1 <- getSunlightTimes(data = data.frame(date = date(Time_POSI)+1, lat = Lat, lon = Lon), keep = "sunrise", tz = tz_loc)
    NightLength1 <- as.numeric(difftime(sunrise1$sunrise, sunset1$sunset, units = "secs"))
    Sunset2Time1 <- as.numeric(difftime(Time_POSI,sunset1$sunset, units = "secs"))
    Sunset2Time1/NightLength1*100}, {
      
        sunset2 <- getSunlightTimes(data = data.frame(date = date(Time_POSI)-1, lat = Lat, lon = Lon), keep = "sunset", tz = tz_loc)
        sunrise2 <- getSunlightTimes(data = data.frame(date = date(Time_POSI), lat = Lat, lon = Lon), keep = "sunrise", tz = tz_loc)
        NightLength2 <- as.numeric(difftime(sunrise2$sunrise, sunset2$sunset, units = "secs"))
        Sunset2Time2 <- as.numeric(difftime(Time_POSI,sunset2$sunset, units = "secs"))
        Sunset2Time2/NightLength2*100})) , {
          
        sunset3 <- getSunlightTimes(data = data.frame(date = date(Time_POSI), lat = Lat, lon = Lon), keep = "sunset", tz = tz_loc)
        sunrise3 <- getSunlightTimes(data = data.frame(date = date(Time_POSI), lat = Lat, lon = Lon), keep = "sunrise", tz = tz_loc)
        NightLength3 <- abs(as.numeric(difftime(sunrise3$sunrise, sunset3$sunset, units = "secs")))
        Sunset2Time3 <- abs(as.numeric(difftime(Time_POSI,sunset3$sunset, units = "secs")))
        Sunset2Time3/NightLength3*100*-1+200})
  )
  
}
