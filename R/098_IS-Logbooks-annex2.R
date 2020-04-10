# ------------------------------------------------------------------------------
# Input: Files generate in scripot 002_IS-logbooks.....
# Output: iceland_annex2_2019.csv

# 2020 datacall:
#  TODO:
#   1. Correct landings for dredge gear
#   2. Try recursive correction of logbook positions based on VMS data

library(lubridate)
library(tidyverse)


lb.wide <- read_rds("data/IS_lb-wide.rds")

annex2 <-
  lb.wide %>%
  # get rid of demersal gear below at of 59.5
  filter(!(between(lon1, -40, 40) &
             between(lat1, 0, 59.5) &
             gid != 7)) %>%
  group_by(date, vid) %>%
  # 2020 - new, older datacall deliveries were wrong
  mutate(fdps = 1 / n()) %>%  # fishing days per setting
  ungroup() %>%
  group_by(type, country, year, month, ices, dcf4,
           dcf6, vlclass) %>%
  summarise(fd = sum(fdps),
            kwfd = sum(fd * kw),
            wgt = sum(catch, na.rm = TRUE),
            value = NA_real_,
            unqv = n_distinct(vid)) %>%
  ungroup() %>%
  mutate(vms = "yes")

# SANITY TEST

library(mar)
con <- connect_mar()
fishing.days <-
  mar:::lb_base(con) %>%
  filter(year %in% 2009:2019) %>%
  select(year, date, vid) %>%
  collect(n = Inf) %>%
  distinct() %>%
  count(year)
# NOTE: does not take into account that invalids are going to be discarded
annex2 %>%
  group_by(year) %>%
  summarise(fd = sum(fd)) %>%
  full_join(fishing.days) %>%
  mutate(p.difference = (fd - n) / n * 100)

annex2 <-
  annex2 %>%
  select(
    type,                    #  1
    country,                 #  2
    year,                    #  3
    month,                   #  4
    unqv,                    #  5
    #Anonymized_vessel_id     #  6
    ices,                    #  7
    dcf4,                    #  8
    dcf6,                    #  9
    vlclass,                 # 10
    vms,                     # 11
    fd,                      # 12
    kwfd,                    # 13
    wgt,                     # 14
    value                    # 15
  ) %>%
  # get rid of invalid ices rectangles
  mutate(valid = !is.na(vmstools::ICESrectangle2LonLat(ices)$SI_LONG))

glimpse(annex2)

annex2 %>%
  write_rds("data/ICES_LE_ISL.rds")

annex2 %>%
  filter(valid) %>%
  select(-valid) %>%
  write_csv("delivery/ICES_LE_ISL.csv")



