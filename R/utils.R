civriset <- function (lat, lon, start_date, tz = "America/New_York", 
                      n_days = 365, to_GMT = TRUE)
{
  ll <- matrix(c(lon, lat), nrow = 1)
  day <- as.POSIXct(start_date, tz = tz)
  sequence <- seq(from = day, length.out = n_days, by = "days")
  civrise <- maptools::crepuscule(ll, sequence, direction = "dawn", 
                                  solarDep = 6, POSIXct = TRUE)$time
  civset <- maptools::crepuscule(ll, sequence, direction = "dusk", 
                                 solarDep = 6, POSIXct = TRUE)$time
  if (to_GMT) {
    attributes(civrise)$tzone <- "GMT"
    attributes(civset)$tzone <- "GMT"
   }
  ss <- data.frame(date = as.Date(civrise), civrise, civset)
  return(ss)
}