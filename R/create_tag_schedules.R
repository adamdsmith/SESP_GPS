# devtools::install_github("adamdsmith/pinpoint")
pacman::p_load(pinpoint, maptools, readr, dplyr, lubridate)
source("./R/utils.R")

# # Make tag testing schedule
# start <- Sys.time() + as.difftime(20, units = "mins")
# test <- seq(start, length.out = 8, by = "122 mins")
# test <- sort(c(test, test + as.difftime(1, units = "mins")))
# sched_pp_fixes(test, tz = "America/New_York", "./test.ASF")

# Get custom monthly tide tables saved individually to "./Data/kiawah_mmm_yyyy.txt" beginning here:
# http://tinyurl.com/Kiawah-tides
fns <- list.files("./Data/Tides", pattern = "kiawah", full.names = TRUE)
kiaw <- lapply(fns, read_tide)
kiaw <- do.call("rbind", kiaw)
kiaw <- mutate(kiaw,
             tide_dt = ymd_hm(paste(date, time), tz = "GMT"),
             date = as.Date(tide_dt)) %>%
  select(date, tide_dt, ht_m) %>%
  arrange(tide_dt)

# Add civil sunrise/sunset
# Set range of sampling date-times
start <- as.Date("2016-12-15")
end <- as.Date("2018-04-30")
study_period <- seq(start, end, by = "days")
kiaw_ss <- civriset(32.625990, -80.048901, start, n_days = length(study_period))

kiaw <- left_join(kiaw, kiaw_ss, by = "date") %>% na.omit() %>%
  # Calculate how far high tide is from civil rise and set
  mutate(h_rise = as.numeric(difftime(tide_dt, civrise, units = "hours")),
         h_set = as.numeric(difftime(civset, tide_dt, units = "hours")),
         h_night = ifelse(h_rise < h_set, h_rise, h_set)) %>%
  # # Restrict to high tides >= 4 hours from civil dawn/dusk
  # filter(h_night >= 4)
  # Restrict to high tides within 2 hours of civil rise
  filter(h_rise >=0 & h_rise <= 1)

# Crudely find date clusters around high tides
# Should be about every 14+ days...lunar cycle and all
kiaw$cluster <- 1L
diffs <- c(1, diff(kiaw$date))
grp <- 1L
for (i in seq_along(diffs)) {
  if (diffs[i] > 1) grp <- grp + 1L
  kiaw$cluster[i] <- grp
}

# Most clusters have multiple qualifying dates
# Pick one closest to civil rise
kiaw <- kiaw %>%
  group_by(cluster) %>%
  slice(which.min(h_rise)) %>% 
  ungroup() %>%
  select(date, tide_dt, ht_m, civrise, h_rise) %>%
  arrange(date) %>% as.data.frame()

######################
## Set up schedules ##
######################

## December 2016 deployments

# Dec 28 - Mar 15: 7 hourly fixes (6 hr window) starting at earliest morning tide 
#                  surrounding each 2wk high tide
# May 2017 - Apr 2018: double up (10 min separation) each high tide
# After Aug 2017, any data are gravy...

# Dec 28 - Mar 15
base_dt <- kiaw %>% 
  filter(date > as.Date("2016-12-15"), date < as.Date("2017-03-15")) %>%
  .[["tide_dt"]]
base_samps <- lapply(base_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# May 1 - on...
bonus_dt <- kiaw %>% 
  filter(date >= as.Date("2017-05-01")) %>%
  .[["tide_dt"]]
bonus_samps <- lapply(bonus_dt, function(dt) dt + as.difftime(c(-5, 5), units = "mins")) %>%
  do.call("c", .)

# Add initial fix, required to be "known" for Swift fixes to work properly
init_fix <- lubridate::ymd_hm("2016-12-12 07:30", tz = "America/New_York")

# Put back to GMT; they seem to get coerced to local TZ
all_samps <- c(init_fix, base_samps, bonus_samps)
attributes(all_samps)$tzone <- "GMT"

# Now export schedule for each tag
sched_pp_fixes(all_samps,
               out_file = "./Schedules/dec16.ASF")

## January 2017 deployments
# Jan 26 - Mar 31: 7 hourly fixes (6 hr window) starting at earliest morning tide 
#                  surrounding each 2wk high tide
# May 2017 - Apr 2018: double up (10 min separation) each high tide
# After Sep 2017, any data are gravy...

# Jan 26 - Mar 31
base_dt <- kiaw %>% 
  filter(date > as.Date("2017-01-15"), date < as.Date("2017-03-31")) %>%
  .[["tide_dt"]]
base_samps <- lapply(base_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# May 1 - on...
bonus_dt <- kiaw %>% 
  filter(date >= as.Date("2017-05-01")) %>%
  .[["tide_dt"]]
bonus_samps <- lapply(bonus_dt, function(dt) dt + as.difftime(c(-5, 5), units = "mins")) %>%
  do.call("c", .)

# Add initial fix, required to be "known" for Swift fixes to work properly
init_fix <- lubridate::ymd_hm("2017-01-09 10:20", tz = "America/New_York")

# Put back to GMT; they seem to get coerced to local TZ
all_samps <- c(init_fix, base_samps, bonus_samps)
attributes(all_samps)$tzone <- "GMT"

# Now export schedule
sched_pp_fixes(all_samps, out_file = "./Schedules/jan17.ASF")

## mid February 2017 deployments
# Feb 15 - Apr 15: 7 hourly fixes (6 hr window) starting at earliest morning tide 
#                  surrounding each 2wk high tide
# May 2017 - Nov 2017: double up (10 min separation) each high tide
# Dec 2018 on, same as Mar - Apr, but any data are gravy...

# Feb 15 - Apr 15
base_dt <- kiaw %>% 
  filter(date > as.Date("2017-02-15"), date < as.Date("2017-04-15")) %>%
  .[["tide_dt"]]
base_samps <- lapply(base_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# May 1 - 30 Nov
bonus_dt <- kiaw %>% 
  filter(date >= as.Date("2017-05-01") & date <= as.Date("2017-11-30")) %>%
  .[["tide_dt"]]
bonus_samps <- lapply(bonus_dt, function(dt) dt + as.difftime(c(-5, 5), units = "mins")) %>%
  do.call("c", .)

# 1 Dec 2017 - 31 Mar 2018
base2_dt <- kiaw %>% 
  filter(date >= as.Date("2017-12-01"), date < as.Date("2018-03-31")) %>%
  .[["tide_dt"]]
base2_samps <- lapply(base2_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# Add initial fix, required to be "known" for Swift fixes to work properly
init_fix <- lubridate::ymd_hm("2017-02-07 09:00", tz = "America/New_York")

# Put back to GMT; they seem to get coerced to local TZ
all_samps <- c(init_fix, base_samps, bonus_samps, base2_samps)
attributes(all_samps)$tzone <- "GMT"

# Now export schedule
sched_pp_fixes(all_samps, out_file = "./Schedules/mid_feb17.ASF")

## late February 2017 deployments
# Mar 10 - Apr 15: 7 hourly fixes (6 hr window) starting at earliest morning tide 
#                  surrounding each 2wk high tide
# May 2017 - Nov 2017: double up (10 min separation) each high tide
# Dec 2018 on, same as Mar - Apr, but any data are gravy...

# Mar 1 - Apr 15
base_dt <- kiaw %>% 
  filter(date > as.Date("2017-02-28"), date < as.Date("2017-04-15")) %>%
  .[["tide_dt"]]
base_samps <- lapply(base_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# May 1 - 30 Nov
bonus_dt <- kiaw %>% 
  filter(date >= as.Date("2017-05-01") & date <= as.Date("2017-11-30")) %>%
  .[["tide_dt"]]
bonus_samps <- lapply(bonus_dt, function(dt) dt + as.difftime(c(-5, 5), units = "mins")) %>%
  do.call("c", .)

# 1 Dec 2017 - 31 Mar 2018
base2_dt <- kiaw %>% 
  filter(date >= as.Date("2017-12-01"), date < as.Date("2018-03-31")) %>%
  .[["tide_dt"]]
base2_samps <- lapply(base2_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# Add initial fix, required to be "known" for Swift fixes to work properly
init_fix <- lubridate::ymd_hm("2017-02-23 11:30", tz = "America/New_York")

# Put back to GMT; they seem to get coerced to local TZ
all_samps <- c(init_fix, base_samps, bonus_samps, base2_samps)
attributes(all_samps)$tzone <- "GMT"

# Now export schedule
sched_pp_fixes(all_samps, out_file = "./Schedules/feb17.ASF")

## March 2017 deployments
# Mar 20 - Apr 15: 7 hourly fixes (6 hr window) starting at earliest morning tide 
#                  surrounding each 2wk high tide
# May 2017 - Nov 2017: double up (10 min separation) each high tide
# Dec 2018 on, same as Mar - Apr, but any data are gravy...

# Mar 20 - Apr 15
base_dt <- kiaw %>% 
  filter(date > as.Date("2017-03-20"), date < as.Date("2017-04-15")) %>%
  .[["tide_dt"]]
base_samps <- lapply(base_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# May 1 - 30 Nov
bonus_dt <- kiaw %>% 
  filter(date >= as.Date("2017-05-01") & date <= as.Date("2017-11-30")) %>%
  .[["tide_dt"]]
bonus_samps <- lapply(bonus_dt, function(dt) dt + as.difftime(c(-5, 5), units = "mins")) %>%
  do.call("c", .)

# 1 Dec 2017 - 31 Mar 2018
base2_dt <- kiaw %>% 
  filter(date >= as.Date("2017-12-01"), date < as.Date("2018-03-31")) %>%
  .[["tide_dt"]]
base2_samps <- lapply(base2_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# Add initial fix, required to be "known" for Swift fixes to work properly
init_fix <- lubridate::ymd_hm("2017-01-09 10:20", tz = "America/New_York")

# Put back to GMT; they seem to get coerced to local TZ
all_samps <- c(init_fix, base_samps, bonus_samps, base2_samps)
attributes(all_samps)$tzone <- "GMT"

# Now export schedule
sched_pp_fixes(all_samps, out_file = "./Schedules/mar17.ASF")

## late March 2017 deployments
# Apr 1 - Apr 15: 7 hourly fixes (6 hr window) starting at earliest morning tide 
#                  surrounding each 2wk high tide
# May 2017 - Nov 2017: double up (10 min separation) each high tide
# Dec 2018 on, same as Mar - Apr, but any data are gravy...

# Apr 1 - Apr 15
base_dt <- kiaw %>% 
  filter(date > as.Date("2017-03-31"), date < as.Date("2017-04-15")) %>%
  .[["tide_dt"]]
base_samps <- lapply(base_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# Apr 15 - 30 Nov
bonus_dt <- kiaw %>% 
  filter(date >= as.Date("2017-04-15") & date <= as.Date("2017-11-30")) %>%
  .[["tide_dt"]]
bonus_samps <- lapply(bonus_dt, function(dt) dt + as.difftime(c(-5, 5), units = "mins")) %>%
  do.call("c", .)

# 1 Dec 2017 - 31 Mar 2018
base2_dt <- kiaw %>% 
  filter(date >= as.Date("2017-12-01"), date < as.Date("2018-03-31")) %>%
  .[["tide_dt"]]
base2_samps <- lapply(base2_dt, function(dt) dt + as.difftime(0:6, units = "hours")) %>%
  do.call("c", .)

# Add initial fix, required to be "known" for Swift fixes to work properly
init_fix <- lubridate::ymd_hm("2017-03-24 14:00", tz = "America/New_York")

# Put back to GMT; they seem to get coerced to local TZ
all_samps <- c(init_fix, base_samps, bonus_samps, base2_samps)
attributes(all_samps)$tzone <- "GMT"

# Now export schedule
sched_pp_fixes(all_samps, out_file = "./Schedules/late_mar17.ASF")







