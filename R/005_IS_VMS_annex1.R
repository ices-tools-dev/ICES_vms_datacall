# TODO
#   1. Current loop for gid 6, 9 & 14
#   2. Do just speed for gid 15, 38 & 40


library(lubridate)
library(tidyverse)
YEARS <- 2018:2009

lgs <-
  read_rds("dataIS/is_lb_wide.rds") %>%
  filter(gid %in% c(6, 9, 14, 15, 38, 40),
         year %in% YEARS)

IDS <-
  lgs %>%
  pull(visir)

fil <- paste0("dataIS/is_vms_visir", YEARS, ".rds")

res <- list()

for(y in 1:length(YEARS)) {

  res[[y]] <-
    read_rds(fil[y]) %>%
    filter(!is.na(mobileid),
           visir %in% IDS,
           between(speed, 1, 5.5))

}

vms <-
  bind_rows(res) %>%
  group_by(visir) %>%
  mutate(n.pings = n()) %>%
  ungroup() %>%
  mutate(year = year(time),
         month = month(time)) %>%
  left_join(lgs %>%
              select(visir, gid, towtime = effort, gear.width, catch, dcf4:grt)) %>%
  mutate(catch = catch / n.pings,
         towtime = towtime / n.pings,
         csquare = vmstools::CSquare(lon, lat, 0.05))

# not really needed
xxx <- data.frame(SI_LONG = vms$lon,
                  SI_LATI = vms$lat)
vms$ices <- vmstools::ICESrectangle(xxx)

# where door spread not available use the median from the metier
vms <-
  vms %>%
  group_by(dcf6) %>%
  mutate(gear.width = ifelse(!is.na(gear.width), gear.width, median(gear.width, na.rm = TRUE))) %>%
  ungroup()

write_rds(vms, "dataIS/is_vms_summary.rds")

annex1 <-
  vms %>%
  group_by(year, month, csquare, vessel_length_class, dcf4, dcf6) %>%
  summarise(speed  = mean(speed, na.rm = TRUE),
            time   = sum(towtime, na.rm = TRUE),
            length = mean(length, na.rm = TRUE),
            kw     = mean(kw, na.rm = TRUE),
            kwh    = kw * time,
            catch  = sum(catch, na.rm = TRUE),
            spread = mean(gear.width, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(type = "VE",
         country = "ICE",
         value = NA) %>%
  select(type, country, year, month, csquare, vessel_length_class, dcf4, dcf6,
         speed, time, length, kw, kwh, catch, value, spread)

write_csv(annex1, "delivery/iceland_annex1_2009-2018.csv")
