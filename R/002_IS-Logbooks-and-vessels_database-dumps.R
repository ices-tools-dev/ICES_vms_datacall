# ------------------------------------------------------------------------------
# could run this from terminal via:
#  nohup R < scripts/0_IS-Logbooks-and-vessels_database-dumps.R --vanilla &

# ------------------------------------------------------------------------------
# A. LIBRARIES AND FUNCTIONS
library(mar)
library(stringr)
library(lubridate)
library(tidyverse)

con <- connect_mar()
YEARS <- 2019

# ------------------------------------------------------------------------------
# B. Logbook dumps
#    NOTE: Here, unlike in the first round of logbook datadump the variable names
#          are retained as they occur in the database
#          The variable names will then be converted downstream to what we have
#          been accustomized to.
#          Here, unlike in the first round the catch table only contains the
#          catch information (prior dump included a lot of the variables that
#          were really part of the base-table.
#          Here, unlike in the first round of datadumps the mobile vs the static
#          detail information are kept, as in the orginal database, as separate
#          tables

# 1. The base table
#    This table is the common table for all fishing activity. One row here
#    constitues a single fishing activity (longline or gillnet settings, a tow)
#    with the exception of the jiggers (gid [veidarfaeri] == 3) were fishermen
#    are allowed to compile the daily activity into a single record.
lb_base <-
  afli_stofn(con) %>%
  filter(ar %in% YEARS)

# 2. The catch composition table
lb_base %>%
  select(visir) %>%
  left_join(afli_afli(con)) %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_lb-catch.rds")

# 3. The "mobile" detail table
#    Provides additional information of the setting associated with mobile gear
#    Oddly some of the trap gear records are included here, something that
#    needs to be checked internally at the MFRI.
lb_base %>%
  select(visir) %>%
  inner_join(afli_toga(con)) %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_lb-mobile.rds")

# 4. The "static" detail table
#    Provides additional information of the setting associated with static gear
lb_base %>%
  select(visir) %>%
  inner_join(afli_lineha(con)) %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_lb-static.rds")

# 5. The gear table - a lookup table, containing some description of the gear
afli_stofn(con) %>%
  select(veidarf) %>%
  distinct() %>%
  collect(n = Inf) %>%
  drop_na() %>%
  arrange(veidarf)  %>%
  left_join(lesa_veidarfaeri(con) %>%
              collect(n = Inf),
            by = c("veidarf" = "veidarfaeri")) %>%
  write_rds("data_dump/is_lb-gear.rds")
lb_base %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_lb-base.rds")

# C. Vessel dumps
#    NOTE: Again here, all variable names as appear in the database are retained
lesa_skipaskra(con) %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_vessel-registry.rds")
tbl_mar(con, "kvoti.skipasaga") %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_vessel-history.rds")
tbl_mar(con, "kvoti.skipaskra_siglo") %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_vessel-siglo.rds")
# New table, provides data on power
tbl_mar(con, "kvoti.skip_extra") %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_vessel-extra.rds")

# Auxillary tables
lesa_veidarfaeri(con) %>%
  arrange(veidarfaeri) %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_lookup_gear.rds")
lesa_tegundir(con) %>%
  arrange(tegund) %>%
  collect(n = Inf) %>%
  write_rds("data_dump/is_lookup_species.rds")
