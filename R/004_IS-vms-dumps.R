# ------------------------------------------------------------------------------
# run this as:
#  nohup R < 000_data_vms_tows.R --vanilla &
#
# The segementation if for every whole minute within a year
# ------------------------------------------------------------------------------

# TODO:
#  * get rid of wacky points - Looks like there may not be a need
#  * check if time of tows overlap (t1 and t2) - DONE
#  * filter out points-on-land, points-close to harbour
#  * convert lon lat so one can do proper linear interpolations
#  * drop records with visir undefined? - NOT DONE, is questionable
#  * add the metiers 4 and 6 right away
#     use integer values for each and then provide a lookup table (saves space).
#  * Take case of "missing" visir (where ibotni and/or togtimi are NA
#     one idea is to filter vms by vedags and then just a speed criterion
#  * expand so algorithm works for all gids

library(lubridate)
library(tidyverse)
library(mar)

YEARS <- 2018
# YEARS <- 2018
#source("R/000_Gear.R")
#GID <-
#  Gear %>%
#  filter(gclass %in% c(6, 9, 14, 15)) %>%
#  pull(gid)

lb_wide <-
  read_rds("dataIS/is_lb_wide.rds")

xx <-
  lb_wide %>%
  #filter(gid %in% c(6, 9, 14, 15, 38, 40)) %>%
  select(visir, vid, t1, t2) %>%
  # 2019-10-29 moved code to 001_IS-Logbook-annex2.R
  # arrange(vid, t1) %>%
  # group_by(vid) %>%
  # mutate(overlap = if_else(t2 > lead(t1), TRUE, FALSE, NA),
  #        t2 = ifelse(overlap,
  #                    lead(t1) - minutes(1), # need to subtract 1 minute but get the format right
  #                    t2),
  #        t2 = ymd_hms(format(as.POSIXct(t2, origin="1970-01-01", tz="UTC")))) %>%
  # ungroup() %>%
  # select(vid, visir, t1, t2) %>%
  gather(tow, time, t1:t2) %>%
  arrange(vid, time) %>%
  mutate(year = year(time))


con <- connect_mar()

for(y in 1:length(YEARS)) {

  YEAR <- YEARS[y]

  TOWS <-
    xx %>%
    filter(year %in% YEAR)

  VID <-
    TOWS %>%
    pull(vid) %>%
    unique() %>%
    as.character()

  VMS <-
    vms(con, YEAR) %>%
    filter(vid %in% VID) %>%
    select(mobileid, vid, time = date, lon, lat, speed) %>%
    collect(n = Inf) %>%
    filter(between(lon, -44, 68.50),
           between(lat,  36, 85.50)) %>%
    distinct() %>%
    mutate(vid = as.integer(vid),
           vms = TRUE) %>%
    arrange(vid, time) %>%
    select(mobileid, vid, time, lon, lat, speed, vms)

  # Sometimes vessels in logbooks but not in vms
  VID <-
    VMS %>%
    pull(vid) %>%
    unique() %>%
    sort()

  res <- list()

  for(v in 1:length(VID)) {
    print(paste(YEAR, v))
    vms <-
      VMS %>%
      filter(vid == VID[v]) %>%
      # HERE: get rid of wacky points via derived speed
      arrange(vid, time) %>%
      #group_by(vid) %>%
      #mutate(dist = geo::arcdist(lead(lat), lead(lon), lat, lon),   # distance to next point
      #       time2 = as.numeric(lead(time) - time) / (60 * 60),     # duration to next point
      #       speed2 = dist/time2) %>%                               # speed on next "leg"
      #filter(speed2 <= 20 | is.na(speed2)) %>%
      #select(-c(time2, speed2, dist)) %>%
      # end of wacky
      # HERE: get rid of wacky points again via derived speed
      #arrange(vid, time) %>%
      #group_by(vid) %>%
      #mutate(dist = geo::arcdist(lead(lat), lead(lon), lat, lon),   # distance to next point
      #       time2 = as.numeric(lead(time) - time) / (60 * 60),     # duration to next point
      #       speed2 = dist/time2) %>%                               # speed on next "leg"
      #filter(speed2 <= 20 | is.na(speed2)) %>%
      #select(-c(time2, speed2, dist)) %>%
      # end of wacky
      mutate(time = round_date(time, "minutes"))

    tows <-
      TOWS %>%
      filter(vid == as.integer(VID[v]))

    res[[v]] <-
      # dataframe of minutes over the whole year
      tibble(time = seq(ymd_hms(paste0(YEAR, "-01-01 00:00:00")),
                        max(tows$time, na.rm = TRUE),
                        by = "1 min")) %>%
      left_join(vms %>% filter(vid == VID[v]), by = "time") %>%
      mutate(y = 1:n())

    if(sum(!is.na(res[[v]]$lon)) > 1) {

      res[[v]] <-
        res[[v]] %>%
        mutate(lon = approx(y, lon, y, method = "linear", rule = 1, f = 0, ties = mean)$y,
               lat = approx(y, lat, y, method = "linear", rule = 1, f = 0, ties = mean)$y,
               speed = approx(y, speed, y, method = "linear", rule = 1, f = 0, ties = mean)$y) %>%
        select(-y) %>%
        bind_rows(tows) %>%
        arrange(time) %>%
        mutate(x = if_else(tow == "t1", 1, 0, 0)) %>%
        mutate(x = case_when(tow == "t1" ~ 1,
                             tow == "t2" ~ -1,
                             TRUE ~ 0)) %>%
        mutate(x = cumsum(x)) %>%
        fill(visir) %>%
        mutate(visir = ifelse(x == 1 | tow == "t2", visir, NA_integer_)) %>%
        mutate(y = 1:n()) %>%
        mutate(lon = approx(y, lon, y, method = "linear", rule = 1, f = 0, ties = mean)$y,
               lat = approx(y, lat, y, method = "linear", rule = 1, f = 0, ties = mean)$y,
               speed = approx(y, speed, y, method = "linear", rule = 1, f = 0, ties = mean)$y,
               vid = as.integer(VID[v])) %>%
        select(time:speed, visir)
    }

  }
  bind_rows(res) %>%
    write_rds(paste0("dataIS/is_vms_visir", YEAR, ".rds"))
}
