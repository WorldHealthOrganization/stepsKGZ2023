################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Largest number of drinks in the past 30 days"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Alargestnum.R"))

alargestnum_df <- alargestnum(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- alargestnum_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean maximum number of standard drinks consumed on one occasion in the past 30 days
alargestnum_a8_list_long <- tbls_summary(.mn_pct_md = mn, .variable = a8)

# DATABOOK prep
alargestnum_a8 <- tbls_list_split(.data = alargestnum_a8_list_long, .vars_amount_number = 0)

################################################################################


