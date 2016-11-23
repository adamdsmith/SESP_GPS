# devtools::install_github("adamdsmith/pinpoint")
pacman::p_load(pinpoint, readr, dplyr, lubridate)
source("./R/utils.R")

# # Make tag testing schedule
# start <- Sys.time() + as.difftime(20, units = "mins")
# test <- seq(start, length.out = 8, by = "122 mins")
# test <- sort(c(test, test + as.difftime(1, units = "mins")))
# sched_pp_fixes(test, tz = "America/New_York", "./test.ASF")

# Get tide data
# http://tidesandcurrents.noaa.gov/noaatidepredictions/NOAATidesFacade.jsp?Stationid=8666767&bmon=12&bday=01&byear=2016&edate=&timelength=monthly&timeZone=0&dataUnits=0&datum=MLLW&interval=high&format=Submit
# From custom monthly tide table saved individually to "./Data/snake_mmm_yyyy.txt"
fn <- list.files("./Data/", pattern = "snake", full.names = TRUE)
sn <- lapply(fn, function(f) {
  tmp <- read_file(f)
  # Some double tabs to get rid of...
  tmp <- gsub("\t\t", "\t", tmp)
  # head(read_lines(tmp, skip = 19))
  tmp <- read_tsv(tmp, skip = 20,
                 col_types = "c_cd_", 
                 col_names = c("date", "time", "ht_m"))
  tmp
})
sn <- do.call("rbind", sn)
sn <- mutate(sn,
             tide_dt = ymd_hm(paste(date, time), tz = "GMT"),
             date = as.Date(tide_dt)) %>%
  select(date, tide_dt, ht_m) %>%
  arrange(tide_dt)

# Add civil sunrise/sunset
# Set range of sampling date-times
start_dt <- as.POSIXct("2016-12-15 EST")
end_dt <- as.POSIXct("2017-08-01 EST")
study_period <- seq(start_dt, end_dt, by = "days")
kiaw_ss <- civriset(32.625990, -80.048901, start_dt, n_days = length(study_period))

poo <- left_join(sn, kiaw_ss, by = "date") %>% na.omit() %>%
  # Is high tide more than 4 hours from civil rise and set?
  mutate(day = tide_dt >= civrise + as.difftime(4, units = "hours") &
           tide_dt <= civset - as.difftime(4, units = "hours"),
         h_rise = as.numeric(difftime(tide_dt, civrise, units = "hours")),
         h_set = as.numeric(difftime(civset, tide_dt, units = "hours")),
         h_night = ifelse(h_rise < h_set, h_rise, h_set)) %>%
  filter(day) %>%
  group_by(month(date)) %>%
  slice(which.max(h_night)) %>%
  ungroup() %>%
  
  select(date, tide_dt, ht_m, )
  
  
  
# Let's schedule 30 tags for December deployment
pp_ids <- 41444 + 0:29
n_tags <- length(pp_ids)

# Assume 60 viable detections from Dec through July ... 
n_firm <- 60
n_bonus <- 40

# Sampling probababilities by season
# Winter: Dec - Mar
# Spring migration: Apr - May
# Summer: June - Aug
season_props <- c(0.70, 0.10, 0.20)
season_firm <- ceiling(season_props * n_firm)
names(season_firm) <- c("winter", "spring", "summer")
total_firm <- n_tags * season_firm
total_bonus <- n_tags * n_bonus

### Do the sampling (simple random weighted by season, no replacement)
# Set range of sampling date-times
start_dt <- as.POSIXct("2016-12-15 00:00:00 EST")
end_dt <- as.POSIXct("2017-08-01 00:00:00 EST")
interval <- 30 # minutes
poss_dt <- seq(start_dt, end_dt, by = interval * 60)

# Set up data frame
tag_df <- data.frame()

# Loop through seasons and draw firm samples
for (s in names(total_firm)) {
  if(s == "winter") {
    months <- c(12, 1:3)
  } else if(s == "spring") {
    months <- 4:5
  } else months <- 6:8
  filt_dt <- poss_dt[month(poss_dt) %in% months]
  samp <- sample(filt_dt, size = total_firm[s])
  tag_df <- rbind(tag_df, 
                  data.frame(pp_id = rep(pp_ids, each = season_firm[s]),
                             dt = samp))
}

## Now draw "bonus" sample
# Set range of bonus sampling date-times
start_dt <- as.POSIXct("2017-08-01 00:00:00 EST")
end_dt <- as.POSIXct("2017-11-01 00:00:00 EST")
interval <- 30 # minutes
bonus_dt <- seq(start_dt, end_dt, by = interval * 60)
samp <- sample(bonus_dt, size = total_bonus)
tag_df <- rbind(tag_df, 
                data.frame(pp_id = rep(pp_ids, each = n_bonus),
                           dt = samp))

# Clean up data frame
tag_df <- group_by(tag_df, pp_id) %>% arrange(pp_id, dt) %>%
  ungroup() %>% as.data.frame()

# Now export schedule for each tag
source("./R/sched_pp_fixes.R")
for (i in pp_ids) {
  tmp <- tag_df[tag_df$pp_id == i, "dt"]
  sched_pp_fixes(tmp, tz = "America/New_York", 
                 out_file = file.path("./Schedules/December", 
                                      paste0("dec_", i, ".ASF")))
}