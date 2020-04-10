# ------------------------------------------------------------------------------
# could run this from terminal via:
#  nohup R < R/****.R --vanilla &

# ------------------------------------------------------------------------------
# LOGBOOK DUMPS
#  2020 datacall: 1. using mar::*lb_*** for the first time, to simplify
#                    coding downstream
#                 2. Added gear from landings
#                 3. Added species with highest catch in a setting

YEARS <- 2009:2019

library(data.table)
library(mar)
library(stringr)
library(lubridate)
library(tidyverse)
con <- connect_mar()

match_nearest_date <- function(lb, ln) {

  lb.dt <-
    lb %>%
    select(vid, datel) %>%
    distinct() %>%
    setDT()

  ln.dt <-
    ln %>%
    select(vid, datel) %>%
    distinct() %>%
    mutate(dummy = datel) %>%
    setDT()

  res <-
    lb.dt[, date.ln := ln.dt[lb.dt, dummy, on = c("vid", "datel"), roll = "nearest"]] %>%
    as_tibble()

  lb %>%
    left_join(res,
              by = c("vid", "datel")) %>%
    left_join(ln %>% select(vid, date.ln = datel, gid.ln),
              by = c("vid", "date.ln"))

}


# ------------------------------------------------------------------------------
# A. Landings gid

ln.base <-
  mar:::ln_catch(con) %>%
  mutate(year = year(date)) %>%
  filter(year %in% YEARS,
         !is.na(vid), !is.na(date)) %>%
  select(vid, gid.ln = gid, datel = date) %>%
  distinct() %>%
  collect(n = Inf)


# ------------------------------------------------------------------------------
# B. Logbooks

# 1. The base table ------------------------------------------------------------
#     Common table for all fishing activity. One row here
#     constitues a single fishing activity (longline or gillnet settings, a tow)
#     with the exception of the jiggers (gid [veidarfaeri] == 3) were fishermen
#     are allowed to compile the daily activity into a single record.

lb.base <-
  mar:::lb_base(con) %>%
  filter(year %in% YEARS) %>%
  select(visir, vid, gid, date, datel, hidl,
         lon1, lat1, lon2, lat2,
         sq, ssq, distance)

# counting
N.lb.base <- lb.base %>% select(visir) %>% collect(n = Inf) %>% nrow()

# 2. The catch composition table -----------------------------------------------
#

lb.catch <-
  lb.base %>%
  select(visir) %>%
  left_join(mar:::lb_catch(con),
            by = "visir")

# get the "target species"
catch.sid.target <-
  lb.catch %>%
  collect(n = Inf) %>%
  group_by(visir) %>%
  mutate(catcht = sum(catch, na.rm = TRUE),
         p = catch / catcht) %>%
  arrange(visir, desc(p), sid) %>%
  # if multiple maxes, the species "selected" is in the order of 1, 2, 3, ...
  slice(1) %>%
  ungroup() %>%
  select(visir, sid.target = sid, p, catcht)

if(catch.sid.target %>%
   summarise(n.visir = n_distinct(visir)) == nrow(catch.sid.target)
) print("test: catch.sid.target has unique visir")

lb.catch %>%
  collect(n = Inf) %>%
  write_rds("data-dump/IS_lb-catch.rds")

# 3. The "mobile" detail table -------------------------------------------------
#    NOTE: Oddly some of the trap gear records are included here, something that
#    needs to be checked internally at the MFRI.

lb.detail.mobile <-
  lb.base %>%
  select(visir) %>%
  inner_join(mar:::lb_mobile(con),
             by = "visir") %>%
  select(visir, effort, effort_unit, mesh, t1, t2, sweeps, plow_width, mesh_min) %>%
  collect(n = Inf)

# 4. "Static" gear detail table ------------------------------------------------

lb.detail.static <-
  lb.base %>%
  select(visir) %>%
  inner_join(mar:::lb_static(con),
             by = "visir") %>%
  select(visir, effort, effort_unit, mesh, t0, t1, t2) %>%
  collect(n = Inf)

# write
bind_rows(lb.detail.static %>% mutate(source = "static"),
          lb.detail.mobile %>% mutate(source = "mobile")) %>%
  write_rds("data-dump/IS_lb-detail.rds")

# 5. Join gid from landings and the sid.target from catch to base

lb.base <-
  lb.base %>%
  collect(n = Inf) %>%
  match_nearest_date(ln.base)

N.lb.base_added.landings.gid <- nrow(lb.base)

lb.base <-
  lb.base %>%
  left_join(catch.sid.target)

# any zeros?
lb.base %>%
  filter(is.na(catcht))
lb.base %>%
  filter(is.na(sid.target))


N.lb.base_added.landings.gid_added.sid.target <- nrow(lb.base)

print("lb.base records")
print(tibble(id = 1:3,
             n = c(N.lb.base,
                   N.lb.base_added.landings.gid,
                   N.lb.base_added.landings.gid_added.sid.target)))

lb.base %>%
  write_rds("data-dump/IS_lb-base.rds")

# 6. The gear table - a lookup table, containing some description of the gear
lb.base %>%
  select(gid) %>%
  distinct() %>%
  left_join(mar:::lu_gear(con) %>%
              collect(n = Inf)) %>%
  drop_na() %>%
  write_rds("data-dump/IS_lu-gear.rds")

# ------------------------------------------------------------------------------
# C. Vessel registry

lb.base %>%
  select(vid) %>%
  distinct() %>%
  left_join(mar:::vessel_registry(con, standardize = TRUE) %>%
              collect(n = Inf),
            by = "vid") %>%
  select(vid, name, uid, cs, imo, kw = engine_kw,
         brl, grt, length,
         vlclass = length_class) %>%
  left_join(mar:::stk_mid_vid_2020(con) %>%
              select(vid, mid, localid, globalid) %>%
              collect(n = Inf),
            by = "vid") %>%
  write_rds("data-dump/IS_lu-vessel.rds")
