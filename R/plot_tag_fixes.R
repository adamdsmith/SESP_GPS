pacman::p_load(rnoaa, dplyr, pinpoint)
source("./R/utils.R")

# Get tide data for Charleston (Cooper River; CO-OPS 8665530)
start_dts <- seq(as.Date("2016/12/28"), by = "31 days", length.out = 2)
end_dts <- start_dts + as.difftime(30, units = "days")

# Convert dates to rnoaa format
start_dts <- rnoaa_dt(start_dts)
end_dts <- rnoaa_dt(end_dts)

tides <- lapply(seq_along(start_dts), function(i) {
  tmp_tides <- try(coops_search(station_name = 8665530, begin_date = start_dts[i],
                            end_date = end_dts[i], units = "english",
                            product = "water_level", datum = "MLLW"))
  if (inherits(tmp_tides, "try-error")) {
    out <- NULL
  } else {
    out <- tmp_tides$data %>%
      select(dt = t, tide_ht = v) %>%
      mutate(dt = dt + as.difftime(10, units = "mins"))
  }
  
  out
})

tides <- do.call("rbind", tides)
attr(tides$dt, "tzone") <- "America/New_York"
