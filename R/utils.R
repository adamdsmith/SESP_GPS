civriset <- function (lat, lon, start_date, 
                      n_days = 365, out_tz = "GMT")
{
  ll <- matrix(c(lon, lat), nrow = 1)
  day <- as.POSIXct(start_date); attributes(day)$tzone <- "GMT"
  sequence <- seq(from = day, length.out = n_days, by = "days")
  civrise <- maptools::crepuscule(ll, sequence, direction = "dawn", 
                                  solarDep = 6, POSIXct = TRUE)$time
  civset <- maptools::crepuscule(ll, sequence, direction = "dusk", 
                                 solarDep = 6, POSIXct = TRUE)$time
  if (out_tz != "GMT") {
    attributes(civrise)$tzone <- out_tz
    attributes(civset)$tzone <- out_tz
   }
  ss <- data.frame(date = as.Date(civrise), civrise, civset)
  return(ss)
}

read_tide <- function(fn) {
  tmp <- readr::read_file(fn)
  # Some double tabs to get rid of...
  tmp <- gsub("\t\t", "\t", tmp)
  # head(read_lines(tmp, skip = 19))
  tmp <- readr::read_tsv(tmp, skip = 20,
                         col_types = "c_cd_", 
                         col_names = c("date", "time", "ht_m"))
  tmp
}