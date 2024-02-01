################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Belief about having had a COVID-19 infection"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Covid", "functions", "COinfect.R"))

coinfect_df <- coinfect(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- coinfect_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of those that reasonably believe they have had a COVID-19 infection
coinfect_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
coinfect_c <- tbls_list_split(
  .data = coinfect_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################

