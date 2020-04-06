# ------------------------------------------------------------------------------
# could run this from terminal via:
#  nohup R < R/****.R --vanilla &

# ------------------------------------------------------------------------------
# LOGBOOK DUMPS
#  2020 datacall: using mar::*lb_*** for the first time, to simplify
#                 coding downstream

YEARS <- 2009:2019

library(mar)
library(stringr)
library(lubridate)
library(tidyverse)
con <- connect_mar()

# ------------------------------------------------------------------------------
# A. Logbooks

# 1. The base table ------------------------------------------------------------
#     Common table for all fishing activity. One row here
#     constitues a single fishing activity (longline or gillnet settings, a tow)
#     with the exception of the jiggers (gid [veidarfaeri] == 3) were fishermen
#     are allowed to compile the daily activity into a single record.

lb.base <-
  mar:::lb_base(con) %>%
  filter(year %in% YEARS) %>%
  select(visir, vid, gid, date, datel, hidl,
         lon1, lat1, sq, ssq, distance)

# 2. The catch composition table -----------------------------------------------
#

lb.base %>%
  select(visir) %>%
  left_join(mar:::lb_catch(con),
            by = "visir") %>%
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
lb.base %>%
  collect(n = Inf) %>%
  write_rds("data-dump/IS_lb-base.rds")

# 5. The gear table - a lookup table, containing some description of the gear
lb.base %>%
  select(gid) %>%
  distinct() %>%
  left_join(mar:::lu_gear(con)) %>%
  collect(n = Inf) %>%
  drop_na() %>%
  write_rds("data-dump/IS_lu-gear.rds")

# ------------------------------------------------------------------------------
# B. Vessel registry

lb.base %>%
  select(vid) %>%
  distinct() %>%
  left_join(mar:::vessel_registry(con, standardize = TRUE),
            by = "vid") %>%
  select(vid, name, uid, cs, imo, vclass, engine_kw,
         brl, grt, length_registered, length,
         length_class) %>%
  left_join(mar:::stk_mid_vid_2020(con) %>%
              select(vid, mid, localid, globalid),
            by = "vid") %>%
  collect(n = Inf) %>%
  write_rds("data-dump/IS_lu-vessel.rds")
