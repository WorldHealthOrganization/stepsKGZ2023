################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Infection confirmed by a test"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Covid", "functions", "COconfirm.R"))

coconfirm_df <- coconfirm(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- coconfirm_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of those who had their infection either confirmed or not by a test
# out of those who reasonably believe they have had a COVID-19 infection.
coconfirm_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
coconfirm_c <- tbls_list_split(
  .data = coconfirm_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

