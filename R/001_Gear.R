library(tidyverse)
library(mar)
con <- connect_mar()
gid.in.logbooks <-
  afli_stofn(con) %>%
  select(gid = veidarf) %>%
  distinct() %>%
  filter(!is.na(gid)) %>%
  collect(n = Inf) %>%
  mutate(in.logbooks = TRUE) %>%
  arrange(gid)
Gear <-
  lesa_veidarfaeri(con) %>%
  collect(n = Inf) %>%
  left_join(husky::gearlist) %>%
  arrange(veidarfaeri) %>%
  select(gid = veidarfaeri,
         gclass = gear,
         description = lysing_enska,
         lysing) %>%
  # Note: Need to check if 8 (spærlingsvarpa) should be classed as bottom trawl
  # Note: Need to check if 8 (spærlingsvarpa) should be classed as bottom trawl
  mutate(gclass = ifelse(gid %in% c(16, 36, 42), 1, gclass),                    # long line
         gclass = ifelse(gid %in% c(2, 11, 25, 29, 32, 72,90,91), 2, gclass),   # net
         gclass = ifelse(gid %in% c(4, 10, 12, 56, 57), 4, gclass),             # seine
         gclass = ifelse(gid %in% c(5, 26, 27), 5, gclass),                     # scotish seine
         gclass = ifelse(gid %in% c(22, 31, 58, 66, 68, 69, 73, 74, 76, 77, 78, 139), 6, gclass), # botnvarpa
         gclass = ifelse(gid %in% c(7,8, 13, 19, 21, 23, 24, 33, 34, 44), 7, gclass),                  # flotvarpa
         gclass = ifelse(gid %in% c(14, 30), 14, gclass),                       # rækjuvarpa
         gclass = ifelse(gid %in% c(15, 37, 38, 40, 160, 172, 173), 15, gclass),# dredge
         gclass = ifelse(gid %in% c(17, 18, 39), 17, gclass),                   # trap
         gclass = ifelse(gid %in% c(20, 41, 45, 99, 43), 20, gclass),           # misc
         gclass = ifelse(is.na(gclass), 20, gclass)) %>%                        # misc
  full_join(gid.in.logbooks) %>%
  mutate(gid = as.integer(gid),
         gclass = as.integer(gclass))
