################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Mean number of unrecorded drinkings in past 7 days"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Ameanunrecorded.R"))

ameanunrecorded_df <- ameanunrecorded(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ameanunrecorded_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean number of standard drinks of unrecorded alcohol consumed on average 
# per day in the past 7 days among current drinkers
ameanunrecorded_list_long <- tbls_summary(.mn_pct_md = mn, .variable = avgperday, .cln2 = onecln, .cln2_val = 1)

# DATABOOK prep
ameanunrecorded <- tbls_list_split(.data = ameanunrecorded_list_long, .vars_amount_number = 0)

################################################################################





