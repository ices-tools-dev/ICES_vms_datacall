# ------------------------------------------------------------------------------
# Input: Raw datadumps from the MRI database
# Output: A single flat file (lb_wide) that contains what is hopefully all
#         relevant variables neeed in further post processing
# Main tidying:
# 1. Names standardard- and anglicized
# 2. Mesh size standardised
# 3. A single variable for effort (effort) with a variable (effort.unit) that
#      defines the effort unit.


library(lubridate)
library(tidyverse)

# ------------------------------------------------------------------------------
VID <-
  read_rds("data_dump/is_lb-base.rds") %>%
  pull(skipnr) %>%
  unique() %>%
  sort()

vessel <-
  read_rds("data_dump/is_vessel-siglo.rds") %>%
  select(vid = skipnr,
         name = nafnskips,
         umdnr,
         cs = kallmerki,
         imo = imonr,
         homeharbour = heimahofn,
         propeller_diameter = thvermskrufu,
         engine_kw = vel_kw,
         power_index = aflvisir,
         length_registered = skradlengd,
         width = skradbreidd,
         depth = skraddypt,
         length = mestalengd,
         brl = bruttoruml,    # neet a proper acronym
         grt = bruttotonn) %>%
  # limit to vessels in logbooks 2017
  filter(vid %in% VID) %>%
  # the "claimed" kw unit is likely wrong, corrected here
  #   the original unit seems to be strange, normally have w or kw, here
  #   it seems to be deciwatts
  mutate(engine_kw = engine_kw / 100) %>%
  mutate(length_registered = length_registered / 100,
         # units of cm to meters
         length = length / 100,
         width = width / 100,
         depth = depth / 100,
         brl = brl / 100,
         grt = grt / 100,
         name = str_trim(name),
         homeharbour = str_trim(homeharbour)) %>%
  # not really used
  separate(umdnr, c("ich", "inu"), sep = 2, convert = TRUE) %>%
  # "correct" brl for Ásgrímur Halldórsson
  mutate(brl = ifelse(vid == 2780, 1000, brl)) %>%
  # "correct" variable for the ghost-ship,
  mutate(length = ifelse(vid == 9928, 5, length),
         brl = ifelse(vid == 9928, 2, brl),
         grt = ifelse(vid == 9928, 2, grt)) %>%
  # Blífari has abnormal engine_kw, divied by 100
  mutate(engine_kw = ifelse(vid == 2069, engine_kw / 100, engine_kw)) %>%
  # now for some metier stuff
  mutate(vessel_length_class = case_when(length < 8 ~ "<8",
                                         length >= 8  & length < 10 ~ "08-10",
                                         length >= 10 & length < 12 ~ "10-12",
                                         length >= 12 & length < 15 ~ "12-15",
                                         length >= 15 ~ ">=15",
                                         TRUE ~ NA_character_))

vessel_category <-
  vessel %>%
  select(vid, kw = engine_kw, length, lngth_class = vessel_length_class, grt)

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
    42, NA_character_,    NA_character_, NA_character_)       # Blue mussel on-growing line

lb_base <-
  read_rds("data_dump/is_lb-base.rds") %>%
  select(visir,
         gid = veidarf,
         vid = skipnr,
         date = vedags,
         lat1 = breidd,
         lon1 = lengd,
         sq = reitur,        # Local statistical rectange (same resolution as ICES)
         ssq = smareitur,    # The quarter within a rectangle
         z1 = dypi,          # depth
         winddirection = vindatt,
         beaufort = vindstig,
         datel = ldags,
         hid = lhofn,        # Harbour id,
         m_sec = m_sek,      # Meters per second??
         lat2 = breidd_lok,
         lon2 = lengd_lok,
         z2 = dypi_lok,
         distance = toglengd)  %>% # a derived variable, distance between
                                   # c(lon1, lat1) and c(lon2, lat2)
  # a single spærlingsvarpa - put as flotvarpa
  mutate(gid = ifelse(gid == 8, 7, gid)) %>%
  # drop diver and blue mussel lines
  filter(!gid %in% c(41, 42))
lb_mobile <-
  read_rds("data_dump/is_lb-mobile.rds") %>%
  # 2019-10-29 get vid
  left_join(lb_base %>% select(visir, vid)) %>%
  select(visir,
         vid,
         on.bottom = ibotni,     # of the form (h)hmm
         towtime = togtimi,      # in minutes
         btemp1 = botnhiti,      # bottom temperature
         headline = hoflina,
         mesh = moskvi,
         moskvi_minnsti,
         doors.kg = hlerar,
         sweeps = grandarar,
         plow.width = pl_breidd,
         btemp2 = botnhiti_lok,
         stemp1 = uppsj_hiti,         # surface temperature
         stemp2 = uppsj_hiti_lok) %>%
  # get date of fishing and the gid
  left_join(lb_base %>% select(visir, date, gid)) %>%
  # get rid of any gear not supposed to be in the "mobile" details
  filter(gid > 3) %>%
  # 2019-10-29 Setting maximum towtime to 12 hours for gid 7 and 14 is not valid
  #mutate(towtime = ifelse(towtime / 60 > 12, 12 * 60, towtime)) %>%
  mutate(towtime = case_when(towtime / 60 > 12 & gid ==  6 ~ 12L * 60L,
                             towtime / 60 > 24 & gid ==  7 ~ 24L * 60L,
                             towtime / 60 > 15 & gid == 14 ~ 15L * 60L,
                             TRUE ~ towtime)) %>%
  # drop diver and blue mussel lines
  filter(!gid %in% c(41, 42)) %>%
  mutate(on.bottom = ifelse(!is.na(on.bottom), str_pad(on.bottom, 4, "left", pad = "0"), NA_character_),
         on.bottom = ifelse(!is.na(on.bottom),
                            paste0(str_sub(on.bottom, 1, 2),
                            ":",
                            str_sub(on.bottom, 3, 4)),
                            NA_character_),
         t1 = ymd_hm(paste(as.character(date), on.bottom)),
         t2 = t1 + minutes(towtime),
         gear.width = case_when(gid %in% c(6, 9, 14) ~ as.numeric(sweeps),
                                gid %in% c(15, 38, 40) ~ as.numeric(plow.width),
                                TRUE ~ NA_real_),
         # For pelagic trawls the smallest mesh size is stored in the variable moskvi_minnsti
         mesh = ifelse(gid == 7, moskvi_minnsti, mesh))%>%
  # date was just "borrowed" from lb_base to calculate e.g. t1 and t2
  select(-moskvi_minnsti) %>%
  mutate(effort = case_when(gid %in% c(6, 7, 9, 14, 15, 38, 40) ~ towtime / 60,
                            # for seine and traps use setting as effort
                            gid %in% c(5, 18, 39, 42) ~ 1,
                            TRUE ~ NA_real_),
         effort.unit = case_when(gid %in% c(6, 7, 9, 14, 15, 38, 40) ~ "hours towed",
                                 # for seine just use the setting
                                 gid %in% c(5, 18, 39, 42) ~ "setting",
                                 TRUE ~ NA_character_)) %>%
  # "standarize" mesh size
  mutate(mesh.std = case_when(gid ==  9 ~ 80,
                              gid %in% c(7, 10, 12, 14) ~ 40,
                              gid %in% c(5, 6) & (mesh <= 147 | is.na(mesh)) ~ 135,
                              gid %in% c(5, 6) &  mesh >  147 ~ 155,
                              gid %in% c(15, 38, 40) ~ 100)) %>%
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
  select(-c(overlap, t22))

lb_static <-
  read_rds("data_dump/is_lb-static.rds") %>%
  # 2019-10-29 get vid
  left_join(lb_base %>% select(visir, vid)) %>%
  select(visir,
         vid,
         n.hooks = onglar,        # longline, hooks per stamp
         n.stamps = bjod,         # longline
         n.nets = dregin,         # gillnets
         n.nights = naetur,       # gillnets
         mesh = moskvi,           # gillnets
         height = haed,           # gillnets
         n.jigs = faeri,          # number of jigs
         hours = klst,            # hours fishing (jigs)
         bait = beita,
         mean_gillnet_length = medal_lengd_neta,
         t1 = drattur_hefst,      # time gear hauling starts
         t2 = drattur_lykur,      # time gear hauling ends
         btemp1 = botnhiti,       # bottom temperature
         stemp1 = uppsjavarhiti,  # surface temperature
         n.jhooks = fj_kroka,     # check what this really is, looks like it is
         # a "new" variable related to longline fishing
         t0 = logn_hefst) %>%     # time of setting gear (to < t1 < t2)
  # get gid
  left_join(lb_base %>% select(visir, gid)) %>%
  # get rid of all gid not "supposed to be" in the static table
  filter(gid %in% c(1, 2, 3)) %>%
  # Question really how to define effort, below is one way - each gear having a
  #  different unit of measure
  # number of longline hooks
  #   question if (soak)time should be added to the effort variable for gid == 1
  #   this may though be difficult to quantify
  mutate(effort = case_when(gid == 1 ~ as.numeric(n.hooks * n.stamps),
                            # netnights - the old measure used in iceland
                            gid == 2 ~ as.numeric(n.nets * n.nights),
                            # jigger hookhours
                            gid == 3 ~ as.numeric(n.jigs * hours)),
         effort.unit = case_when(gid == 1 ~ "hooks",
                                 gid == 2 ~ "netnights",
                                 gid == 3 ~ "hookours")) %>%
  mutate(mesh.std = NA_real_) %>%
  # Standardized gillnet meshes - at this stage it may though be better to
  #  just ignore them
  #mutate(mesh.std = case_when(gid == 2 & mesh < 150 ~ 125,
  #                            gid == 2 & between(mesh, 150, 200) ~ 175,
  #                            gid == 2 & between(mesh, 201, 250) ~ 225,
  #                            gid == 2 & between(mesh, 250, 300) ~ 275,
  #                            gid == 2 & mesh > 300 ~ 325,
  #                            gid == 2 & is.na(mesh) ~ 275))
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
  select(-c(overlap, t22))

# Here just get the total catch of a setting, i.e. drop the species
lb_catch <-
  read_rds("data_dump/is_lb-catch.rds") %>%
  group_by(visir) %>%
  summarise(catch = sum(afli, na.rm = TRUE)) %>%
  ungroup()

lb_details <-
  bind_rows(lb_static %>%
              select(visir, gid, effort, effort.unit, t0, t1, t2, mesh.std, btemp1, stemp1),
            lb_mobile %>%
              select(visir, gid, effort, effort.unit, t1, t2, mesh.std, btemp1, stemp1,
                     btemp2, stemp2, gear.width))

lb_wide <-
  lb_base %>%
  # use here inner_join to not include records that have no details
  # (about 9800 records - less than 1% of all records)
  inner_join(lb_details) %>%
  left_join(lb_catch) %>%
  # flag if no detail information was found
  #mutate(in.detail = ifelse(visir %in% c(lb_mobile$visir, lb_static$visir),
  #                          TRUE,
  #                          FALSE)) %>%
  left_join(metier) %>%
  # set mesh size for traps as zero
  mutate(mesh.std = ifelse(is.na(mesh.std), 0, mesh.std)) %>%
  mutate(dcf6 = paste(dcf4, dcf5b, mesh.std, "0_0", sep = "_")) %>%
  left_join(vessel_category) %>%
  filter(between(lon1, -44, 68.50),
         between(lat1,  36, 85.50))

# ICES rectangles
res <- data.frame(SI_LONG = lb_wide$lon1,
                  SI_LATI = lb_wide$lat1)
lb_wide$ices <- vmstools::ICESrectangle(res)

lb_wide <-
  lb_wide %>%
  mutate(type = "LE",
         country = "ICE",
         year = year(date),
         month = month(date)) %>%
  rename(vessel_length_class = lngth_class,
         mesh = mesh.std)


write_rds(lb_wide, "data/is_lb_wide.rds")


# CHECK
#lb_wide %>% select(gid, mesh, dcf4:dcf6) %>% distinct()

annex2 <-
  lb_wide %>%
  group_by(type, country, year, month, ices, vid, dcf4, dcf6, vessel_length_class, gid, mesh) %>%
  summarise(FishingDays = n(),
            kwdays = sum(FishingDays * kw),
            catch = sum(catch, na.rm = TRUE),
            value = NA_real_) %>%
  ungroup() %>%
  mutate(vms = "Yes") %>%
  select(type, country, year, month, ices, dcf4, dcf6, vessel_length_class,
         vms, FishingDays, kwdays, catch, value)


# HERE - FILTER OUT INVALID ICES RECTANGLES - just so the computer does not complain
annex2 <-
  annex2 %>%
  mutate(valid.ices = !is.na(vmstools::ICESrectangle2LonLat(ices)$SI_LONG))

write_rds(annex2, "data/logbooks_annex2.rds")
annex2 %>%
  filter(year %in% 2009:2018,
         valid.ices) %>%
  select(-valid.ices) %>%
  write_csv("delivery/iceland_annex2_2009-2018.csv")

