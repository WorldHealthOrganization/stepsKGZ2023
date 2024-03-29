################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Frequency of needing first drink in the morning"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Amorningdrink.R"))

amorningdrink_df <- amorningdrink(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- amorningdrink_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of needing a first drink in the morning to get going during 
# the past 12 months among past 12 month drinkers
amorningdrink_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
amorningdrink_c <- 
  tbls_list_split(.data = amorningdrink_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################


