# ------------------------------------------------------------------------------
# Input: Files generate in scripot 002_IS-logbooks.....
# Output: iceland_annex2_2019.csv

# 2020 datacall:
#  TODO:
#   1. Correct landings for dredge gear
#   2. Try recursive correction of logbook positions based on VMS data

library(lubridate)
library(tidyverse)

# ------------------------------------------------------------------------------
VID <-
  read_rds("data-dump/IS_lb-base.rds") %>%
  pull(vid) %>%
  unique() %>%
  sort()

vessel_category <-
  read_rds("data-dump/IS_lu-vessel.rds") %>%
  filter(vid %in% VID) %>%
  # "correct" brl for Ásgrímur Halldórsson
  mutate(brl = ifelse(vid == 2780, 1000, brl)) %>%
  select(vid, kw, length, vlclass, grt)

metier <-
  tribble(
    ~gid, ~dcf4, ~dcf5, ~dcf5b,
    1, "LLS", "Fish",      "DMF",     # Long line
    2, "GSN", "Fish",      "DMF",     # Gill net
    3, "LHP", "Fish",      "DMF",     # Jiggers (hooks)
    4, "PS",  "Fish",      "SPF",     # "Cod" seine
    5, "SDN", "Fish",      "DMF",     # Scottish seine
    6, "OTB", "Fish",      "DMF",     # Bottom fish trawl
    7, "OTM", "Fish",      "SPF",     # Pelagic trawl
    9, "OTB", "Nephrops",  "NEP",     # Bottom nephrops trawl
    10, "PS",  "Fish",      "SPF",     # "Herring" seine
    12, "PS",  "Fish",      "SPF",     # "Capelin" seine
    14, "OTB", "Shrimp",    "SHR",     # Bottom shrimp trawl
    15, "DRB", "Mollusc",   "MOL",     # Mollusc (scallop) dredge
    18, "TRP", "Crustacean","CRU",     # Crab trap
    38, "DRB", "Mollusc",   "MOL",     # Mollusc	(cyprine) dredge
    39, "TRP", "Mollusc",   "MOL",     # Buccinum trap
    40, "DRB", "Echinoderm","ECH",     # Sea-urchins dredge
    99, "DRB", "Echinoderm","ECH",     # Sea-cucumber dredge
    42, NA_character_,    NA_character_, NA_character_)       # Blue mussel on-growing line

lb.base <-
  read_rds("data-dump/IS_lb-base.rds") %>%
  rename(gid.lb = gid)

# ------------------------------------------------------------------------------
# gear correction
lb.base <-
  lb.base %>%
  # gear correction based on target species
  mutate(gid.source = ifelse(sid.target %in% c(45, 43, 46, 191, 199), "sid.target", NA_character_),
         gid = case_when(sid.target == 45 ~ 39L,     # whelk --> whelktrap
                         sid.target == 43 ~ 15L,     # scallop --> scallop dredge
                         sid.target == 46 ~ 38L,     # cyprine (kúfiskur) --> cyprinid dredge,
                         sid.target == 191 ~ 40L,    # sea-urchin --> sea-urchin dredge
                         sid.target == 199 ~ 99L,   # sea-cucumber --> new gear (for now)
                         TRUE ~ NA_integer_)) %>%
  # gear correction based gear in landings and if target species "matches"
  mutate(gid = case_when(gid.ln ==  9 & sid.target == 40 ~ 9L,   # nephrops
                         gid.ln == 14 & sid.target == 41 ~ 14L,  # shrimp
                         TRUE ~ gid),
         gid.source = ifelse(is.na(gid) & !is.na(gid.source), "gid.ln_and_sid.target", gid.source)) %>%
  # logbook gid as pelagic but target species is demersal
  mutate(gid = case_when(gid.lb == 7 & gid.ln == 6 & sid.target %in% c(1, 2, 3, 4, 5, 22, 61) ~ 6L,
                         TRUE ~ gid),
         gid.source = ifelse(is.na(gid) & !is.na(gid.source), "gid.ln_and_sid.target", gid.source)) %>%
  mutate(gid = ifelse(!is.na(gid), gid, gid.lb))


# END: gear correction
# ------------------------------------------------------------------------------


lb.details <-
  read_rds("data-dump/IS_lb-detail.rds") %>%
  left_join(lb.base %>% select(visir, vid, gid, date)) %>%
  mutate(effort = case_when(effort > 12 & gid ==  6 ~ 12,
                            effort > 24 & gid ==  7 ~ 24,
                            effort > 15 & gid == 14 ~ 15,
                            TRUE ~ effort),
         # 2020 data call: added gid = 7 to below
         gear.width = case_when(gid %in% c(6, 7, 9, 14) ~ as.numeric(sweeps),
                                gid %in% c(15, 38, 40) ~ as.numeric(plow_width),
                                TRUE ~ NA_real_),
         mesh = ifelse(gid == 7, mesh_min, mesh)) %>%
  select(-c(mesh_min, plow_width)) %>%
  # "standarize" mesh size
  mutate(mesh.std = case_when(gid ==  9 ~ 80,
                              gid %in% c(7, 10, 12, 14) ~ 40,
                              gid %in% c(5, 6) & (mesh <= 147 | is.na(mesh)) ~ 135,
                              gid %in% c(5, 6) &  mesh >  147 ~ 155,
                              gid %in% c(15, 38, 40) ~ 100,
                              TRUE ~ NA_real_)) %>%
  # 2019-10-29 code added, is moved from 000_IS-vms-dumps.R
  #            NOTE: Effort not adjusted accordingly
  arrange(vid, t1) %>%
  group_by(vid) %>%
  mutate(overlap = if_else(t2 > lead(t1), TRUE, FALSE, NA),
         # testing
         t22 = if_else(overlap,
                     lead(t1) - minutes(1), # need to subtract 1 minute but get the format right
                     t2,
                     as.POSIXct(NA)),
         t22 = ymd_hms(format(as.POSIXct(t22, origin="1970-01-01", tz="UTC"))),
         t2 = ifelse(overlap & !is.na(t22), t22, t2)) %>%
  ungroup() %>%
  # 2019-10-29 NOTE: TODO - adjust effort (towtime) accordingly
  select(-c(overlap, t22, vid, gid, date, mesh))

# Here just get the total catch of a setting, i.e. drop the species
lb.catch <-
  read_rds("data-dump/IS_lb-catch.rds") %>%
  group_by(visir) %>%
  summarise(catch = sum(catch, na.rm = TRUE)) %>%
  ungroup()

lb.wide <-
  lb.base %>%
  inner_join(lb.details,
             by = "visir") %>%
  left_join(lb.catch,
            by = "visir") %>%
  left_join(metier,
            by = "gid") %>%
  # set mesh size for traps as zero
  mutate(mesh.std = ifelse(is.na(mesh.std), 0, mesh.std)) %>%
  mutate(dcf6 = paste(dcf4, dcf5b, mesh.std, "0_0", sep = "_")) %>%
  left_join(vessel_category,
            by = "vid") %>%
  filter(between(lon1, -44, 68.50),
         between(lat1,  36, 85.50))


# ICES rectangles
res <- data.frame(SI_LONG = lb.wide$lon1,
                  SI_LATI = lb.wide$lat1)
lb.wide$ices <- vmstools::ICESrectangle(res)

lb.wide <-
  lb.wide %>%
  mutate(type = "LE",
         country = "ICE",
         year = year(date),
         month = month(date)) %>%
  rename(mesh = mesh.std)

# CHECK
# lb.wide %>% select(gid, mesh, dcf4:dcf6) %>% distinct()

write_rds(lb.wide, "data/IS_lb-wide.rds")



